#!/usr/bin/env python3
"""Build a GitHub-only coverage manifest and tiny dashboard.

This observes release assets and workflow metadata. It does not call Meta,
download report files, or run scraper code.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import csv
import datetime as dt
import html
import http.client
import json
import os
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Any


API_ROOT = "https://api.github.com"
REPORTS_REPOS = ("favstats/meta_ad_reports2", "favstats/meta_ad_reports")
REPORTS_PRIMARY_REPO = REPORTS_REPOS[0]
TARGETING_REPO = "favstats/meta_ad_targeting"

REPORT_WINDOWS = ("yesterday", "last_7_days", "last_30_days", "last_90_days", "lifelong")
TARGETING_WINDOWS = ("last_7_days", "last_30_days", "last_90_days")
WORKFLOWS = (
    (REPORTS_PRIMARY_REPO, "reports", "yesterday", "reportsyesterday.yml", "Reports Yesterday"),
    (REPORTS_PRIMARY_REPO, "reports", "last_7_days", "reports7.yml", "Reports 7"),
    (REPORTS_PRIMARY_REPO, "reports", "last_30_days", "reports30.yml", "Reports 30"),
    (REPORTS_PRIMARY_REPO, "reports", "last_90_days", "reports90.yml", "Reports 90"),
    (REPORTS_PRIMARY_REPO, "reports", "lifelong", "reportslifelong.yml", "Reports Lifelong"),
    (TARGETING_REPO, "targeting", "last_7_days", "targeting7.yml", "Targeting 7"),
    (TARGETING_REPO, "targeting", "last_30_days", "targeting30.yml", "Targeting 30"),
    (TARGETING_REPO, "targeting", "last_90_days", "targeting90.yml", "Targeting 90"),
    (TARGETING_REPO, "targeting", "info", "info.yml", "Targeting Info"),
    ("favstats/metaus", "vpn", "us", "retrieve.yml", "VPN / Meta US"),
    ("favstats/metade", "vpn", "de", "retrieve.yml", "VPN / Meta DE"),
)

TAG_RE = re.compile(r"^(?P<country>[A-Z]{2})-(?P<window>yesterday|last_7_days|last_30_days|last_90_days|lifelong)$")
DATE_ASSET_RE = re.compile(r"^(?P<date>\d{4}-\d{2}-\d{2})\.(?P<ext>rds|zip|parquet)$")
PLACEHOLDER_DATA_DATES = {"2000-01-01"}
HTML_ROW_LIMIT = 900


class GitHub:
    def __init__(self, token: str | None) -> None:
        self.token = token

    def get(self, path_or_url: str) -> tuple[Any, dict[str, str]]:
        url = path_or_url if path_or_url.startswith("http") else f"{API_ROOT}{path_or_url}"
        request = urllib.request.Request(url)
        request.add_header("Accept", "application/vnd.github+json")
        request.add_header("X-GitHub-Api-Version", "2022-11-28")
        request.add_header("User-Agent", "meta-coverage-dashboard")
        if self.token:
            request.add_header("Authorization", f"Bearer {self.token}")

        for attempt in range(4):
            try:
                with urllib.request.urlopen(request, timeout=45) as response:
                    headers = {k.lower(): v for k, v in response.headers.items()}
                    return json.loads(response.read().decode("utf-8")), headers
            except urllib.error.HTTPError as exc:
                if exc.code in {403, 429, 500, 502, 503, 504} and attempt < 3:
                    reset = exc.headers.get("x-ratelimit-reset")
                    wait = max(1, min(60, int(reset) - int(time.time()) + 1)) if reset and exc.code == 403 else 2**attempt
                    time.sleep(wait)
                    continue
                body = exc.read().decode("utf-8", errors="replace")[:500]
                raise RuntimeError(f"GitHub API error {exc.code} for {url}: {body}") from exc
            except (http.client.IncompleteRead, TimeoutError, urllib.error.URLError):
                if attempt < 3:
                    time.sleep(2**attempt)
                    continue
                raise
        raise RuntimeError(f"GitHub API failed for {url}")

    def paginate(self, path: str) -> list[Any]:
        url = f"{API_ROOT}{path}"
        out: list[Any] = []
        while url:
            payload, headers = self.get(url)
            out.extend(payload if isinstance(payload, list) else [payload])
            url = next_link(headers.get("link", ""))
        return out


def next_link(header: str) -> str | None:
    for part in header.split(","):
        match = re.search(r'<([^>]+)>;\s*rel="next"', part)
        if match:
            return match.group(1)
    return None


def now_utc() -> str:
    return dt.datetime.now(dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def date_or_none(value: str | None) -> dt.date | None:
    if not value:
        return None
    try:
        return dt.date.fromisoformat(value)
    except ValueError:
        return None


def date_ranges(dates: list[str]) -> list[list[str]]:
    parsed = sorted(date_or_none(date) for date in dates)
    parsed = [date for date in parsed if date is not None]
    if not parsed:
        return []

    ranges = []
    start = parsed[0]
    previous = parsed[0]
    for current in parsed[1:]:
        if current == previous + dt.timedelta(days=1):
            previous = current
            continue
        ranges.append([start.isoformat(), previous.isoformat()])
        start = current
        previous = current
    ranges.append([start.isoformat(), previous.isoformat()])
    return ranges


def lag_days(latest: str | None, expected: str | None) -> int | None:
    latest_date = date_or_none(latest)
    expected_date = date_or_none(expected)
    return None if latest_date is None or expected_date is None else (expected_date - latest_date).days


def dates_from_ranges(ranges: list[list[str]]) -> list[str]:
    dates = []
    for start, end in ranges:
        start_date = date_or_none(start)
        end_date = date_or_none(end)
        if start_date is None or end_date is None:
            continue
        current = start_date
        while current <= end_date:
            dates.append(current.isoformat())
            current += dt.timedelta(days=1)
    return dates


def inventory(releases: list[dict[str, Any]], dataset: str, source_repo: str) -> dict[str, dict[str, dict[str, Any]]]:
    by_country: dict[str, dict[str, dict[str, Any]]] = {}
    for release in releases:
        tag = release.get("tag_name") or ""
        match = TAG_RE.match(tag)
        if not match:
            continue

        country = match.group("country")
        window = match.group("window")
        if dataset == "reports" and window not in REPORT_WINDOWS:
            continue
        if dataset == "targeting" and window not in TARGETING_WINDOWS:
            continue

        dated = []
        assets = release.get("assets") or []
        for asset in assets:
            asset_name = asset.get("name") or ""
            asset_match = DATE_ASSET_RE.match(asset_name)
            if not asset_match:
                continue
            asset_date = asset_match.group("date")
            if asset_date in PLACEHOLDER_DATA_DATES:
                continue
            if dataset == "reports" and asset_match.group("ext") != "rds":
                continue
            if dataset == "targeting" and asset_match.group("ext") != "parquet":
                continue
            dated.append(
                {
                    "name": asset_name,
                    "date": asset_date,
                    "updated_at": asset.get("updated_at"),
                    "size": asset.get("size"),
                }
            )

        dated.sort(key=lambda x: x["date"], reverse=True)
        available_dates = sorted({item["date"] for item in dated})
        latest = dated[0] if dated else {}
        by_country.setdefault(country, {})[window] = {
            "tag": tag,
            "html_url": release.get("html_url"),
            "source_repo": source_repo,
            "source_repos": [source_repo],
            "release_sources": [
                {
                    "repo": source_repo,
                    "tag": tag,
                    "html_url": release.get("html_url"),
                    "dated_asset_count": len(dated),
                    "latest_data_date": latest.get("date"),
                }
            ],
            "asset_count": len(assets),
            "dated_asset_count": len(dated),
            "latest_data_date": latest.get("date"),
            "latest_asset_name": latest.get("name"),
            "latest_asset_updated_at": latest.get("updated_at"),
            "latest_asset_size": latest.get("size"),
            "available_ranges": date_ranges(available_dates),
            "_available_dates": available_dates,
        }
    return by_country


def merge_release_entries(country: str, window: str, entries: list[dict[str, Any]]) -> dict[str, Any]:
    if not entries:
        return {}

    available_dates = sorted({date for entry in entries for date in entry.get("_available_dates", dates_from_ranges(entry.get("available_ranges", [])))})
    sources = [source for entry in entries for source in entry.get("release_sources", [])]
    source_repos = sorted({source.get("repo") for source in sources if source.get("repo")})
    latest_entry = max(
        entries,
        key=lambda entry: entry.get("latest_data_date") or "",
    )
    latest_source = max(
        sources,
        key=lambda source: source.get("latest_data_date") or "",
    ) if sources else {}

    return {
        "tag": f"{country}-{window}",
        "html_url": latest_source.get("html_url") or latest_entry.get("html_url"),
        "source_repo": latest_source.get("repo") or latest_entry.get("source_repo"),
        "source_repos": source_repos,
        "release_sources": sorted(sources, key=lambda source: (source.get("repo") or "", source.get("tag") or "")),
        "asset_count": sum(entry.get("asset_count", 0) for entry in entries),
        "dated_asset_count": len(available_dates),
        "latest_data_date": latest_entry.get("latest_data_date"),
        "latest_asset_name": latest_entry.get("latest_asset_name"),
        "latest_asset_updated_at": latest_entry.get("latest_asset_updated_at"),
        "latest_asset_size": latest_entry.get("latest_asset_size"),
        "available_ranges": date_ranges(available_dates),
        "_available_dates": available_dates,
    }


def merge_inventories(inventories: list[dict[str, dict[str, dict[str, Any]]]]) -> dict[str, dict[str, dict[str, Any]]]:
    countries = sorted({country for inventory_item in inventories for country in inventory_item})
    merged: dict[str, dict[str, dict[str, Any]]] = {}
    for country in countries:
        windows = sorted({window for inventory_item in inventories for window in inventory_item.get(country, {})})
        for window in windows:
            entries = [inventory_item[country][window] for inventory_item in inventories if window in inventory_item.get(country, {})]
            merged.setdefault(country, {})[window] = merge_release_entries(country, window, entries)
    return merged


def release_tags(client: GitHub, repo: str, windows: tuple[str, ...]) -> list[str]:
    payload, _ = client.get(f"/repos/{repo}/git/matching-refs/tags/")
    tags = []
    for ref in payload:
        tag = (ref.get("ref") or "").replace("refs/tags/", "", 1)
        match = TAG_RE.match(tag)
        if match and match.group("window") in windows:
            tags.append(tag)
    return sorted(set(tags))


def releases_for_tags(client: GitHub, repo: str, tags: list[str]) -> list[dict[str, Any]]:
    workers = max(1, int(os.environ.get("COVERAGE_WORKERS", "8")))

    def fetch(tag: str) -> dict[str, Any]:
        path = f"/repos/{repo}/releases/tags/{urllib.parse.quote(tag, safe='')}"
        try:
            release, _ = client.get(path)
            return release
        except Exception as exc:
            print(f"warning: could not fetch {repo} release {tag}: {exc}", file=sys.stderr)
            return {"tag_name": tag, "html_url": f"https://github.com/{repo}/releases/tag/{tag}", "assets": []}

    releases: list[dict[str, Any]] = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as pool:
        for release in pool.map(fetch, tags):
            releases.append(release)
    return releases


def releases_for_repo(client: GitHub, repo: str) -> list[dict[str, Any]]:
    env_name = "COVERAGE_REPORTS_PER_PAGE" if repo in REPORTS_REPOS else "COVERAGE_TARGETING_PER_PAGE"
    default = "10"
    per_page = max(1, min(100, int(os.environ.get(env_name, os.environ.get("COVERAGE_RELEASES_PER_PAGE", default)))))
    try:
        return client.paginate(f"/repos/{repo}/releases?per_page={per_page}")
    except Exception as exc:
        windows = REPORT_WINDOWS if repo in REPORTS_REPOS else TARGETING_WINDOWS
        print(f"warning: release listing failed for {repo}; falling back to tag fetches: {exc}", file=sys.stderr)
        return releases_for_tags(client, repo, release_tags(client, repo, windows))


def expected_dates(rows: dict[str, dict[str, dict[str, Any]]], windows: tuple[str, ...]) -> dict[str, str | None]:
    expected: dict[str, str | None] = {}
    for window in windows:
        dates = [
            entry["latest_data_date"]
            for country_rows in rows.values()
            for row_window, entry in country_rows.items()
            if row_window == window and entry.get("latest_data_date")
        ]
        expected[window] = max(dates) if dates else None
    return expected


def report_status(release: dict[str, Any], expected: str | None) -> str:
    if not release.get("tag"):
        return "missing_release"
    if release.get("dated_asset_count", 0) == 0:
        return "empty_release"
    if expected and release.get("latest_data_date") == expected:
        return "fresh"
    return "lagging"


def targeting_status(release: dict[str, Any], source: dict[str, Any], expected: str | None) -> str:
    source_count = source.get("dated_asset_count", 0)
    target_count = release.get("dated_asset_count", 0)
    source_latest = source.get("latest_data_date")
    target_latest = release.get("latest_data_date")

    if target_count == 0:
        return "skipped_no_source" if source_count == 0 else "missing_targeting"
    if source_count == 0 and expected and target_latest != expected:
        return "skipped_no_source"
    if source_latest and target_latest and date_or_none(target_latest) and date_or_none(source_latest):
        if date_or_none(target_latest) < date_or_none(source_latest):
            return "behind_source"
    if expected and target_latest == expected:
        return "fresh"
    return "lagging"


def coverage_rows(
    reports: dict[str, dict[str, dict[str, Any]]],
    targeting: dict[str, dict[str, dict[str, Any]]],
) -> tuple[list[dict[str, Any]], dict[str, dict[str, str | None]]]:
    report_expected = expected_dates(reports, REPORT_WINDOWS)
    targeting_expected = expected_dates(targeting, TARGETING_WINDOWS)
    countries = sorted(set(reports) | set(targeting))
    out: list[dict[str, Any]] = []

    for country in countries:
        for window in REPORT_WINDOWS:
            release = reports.get(country, {}).get(window, {})
            expected = report_expected.get(window)
            out.append(row(country, "reports", window, release, expected, report_status(release, expected)))

        for window in TARGETING_WINDOWS:
            release = targeting.get(country, {}).get(window, {})
            source = reports.get(country, {}).get(window, {})
            expected = targeting_expected.get(window)
            status = targeting_status(release, source, expected)
            item = row(country, "targeting", window, release, expected, status)
            item["source_latest_data_date"] = source.get("latest_data_date")
            item["source_dated_asset_count"] = source.get("dated_asset_count", 0)
            item["source_status"] = report_status(source, report_expected.get(window))
            item["source_available_ranges"] = source.get("available_ranges", [])
            item["source_repo_sources"] = source.get("source_repos", [])
            out.append(item)

    return out, {"reports": report_expected, "targeting": targeting_expected}


def row(country: str, dataset: str, window: str, release: dict[str, Any], expected: str | None, status: str) -> dict[str, Any]:
    latest = release.get("latest_data_date")
    return {
        "dataset": dataset,
        "country": country,
        "window": window,
        "status": status,
        "latest_data_date": latest,
        "expected_data_date": expected,
        "lag_days": lag_days(latest, expected),
        "asset_count": release.get("asset_count", 0),
        "dated_asset_count": release.get("dated_asset_count", 0),
        "latest_asset_name": release.get("latest_asset_name"),
        "latest_asset_updated_at": release.get("latest_asset_updated_at"),
        "latest_asset_size": release.get("latest_asset_size"),
        "available_ranges": release.get("available_ranges", []),
        "repo_sources": release.get("source_repos", []),
        "release_sources": release.get("release_sources", []),
        "tag": release.get("tag") or f"{country}-{window}",
        "release_url": release.get("html_url"),
        "source_latest_data_date": None,
        "source_dated_asset_count": None,
        "source_status": None,
        "source_available_ranges": [],
        "source_repo_sources": [],
    }


def summaries(rows: list[dict[str, Any]]) -> list[dict[str, Any]]:
    out = []
    for dataset, window in sorted({(x["dataset"], x["window"]) for x in rows}):
        selected = [x for x in rows if x["dataset"] == dataset and x["window"] == window]
        counts: dict[str, int] = {}
        for item in selected:
            counts[item["status"]] = counts.get(item["status"], 0) + 1
        out.append(
            {
                "dataset": dataset,
                "window": window,
                "total": len(selected),
                "expected_data_date": selected[0].get("expected_data_date") if selected else None,
                "status_counts": dict(sorted(counts.items())),
            }
        )
    return out


def previous_completed_months(anchor: dt.date, count: int = 2) -> list[tuple[dt.date, dt.date]]:
    periods = []
    cursor = anchor.replace(day=1) - dt.timedelta(days=1)
    for _ in range(count):
        start = cursor.replace(day=1)
        periods.append((start, cursor))
        cursor = start - dt.timedelta(days=1)
    return list(reversed(periods))


def date_span(start: dt.date, end: dt.date) -> list[dt.date]:
    return [start + dt.timedelta(days=index) for index in range((end - start).days + 1)]


def missing_runs(missing_dates: list[dt.date]) -> list[list[str]]:
    if not missing_dates:
        return []
    sorted_dates = sorted(missing_dates)
    runs = []
    start = sorted_dates[0]
    previous = sorted_dates[0]
    for current in sorted_dates[1:]:
        if current == previous + dt.timedelta(days=1):
            previous = current
            continue
        runs.append([start.isoformat(), previous.isoformat()])
        start = current
        previous = current
    runs.append([start.isoformat(), previous.isoformat()])
    return runs


def gap_summary(rows: list[dict[str, Any]], generated_at: str) -> dict[str, Any]:
    generated_date = date_or_none(generated_at[:10]) or dt.date.today()
    periods = previous_completed_months(generated_date)
    period_dates = [date for start, end in periods for date in date_span(start, end)]
    report_rows = [row for row in rows if row["dataset"] == "reports"]
    summaries_by_window = []

    for window in REPORT_WINDOWS:
        selected = [row for row in report_rows if row["window"] == window]
        active = [row for row in selected if row.get("dated_asset_count", 0) > 0]
        top_countries = []
        missing_by_date: dict[str, int] = {}
        countries_with_gaps = 0
        total_missing_days = 0

        for item in active:
            available = {date_or_none(value) for value in dates_from_ranges(item.get("available_ranges", []))}
            available.discard(None)
            missing = [date for date in period_dates if date not in available]
            if not missing:
                continue
            countries_with_gaps += 1
            total_missing_days += len(missing)
            for missing_date in missing:
                key = missing_date.isoformat()
                missing_by_date[key] = missing_by_date.get(key, 0) + 1
            top_countries.append(
                {
                    "country": item["country"],
                    "missing_days": len(missing),
                    "status": item["status"],
                    "latest_data_date": item.get("latest_data_date"),
                    "missing_ranges": missing_runs(missing)[:6],
                    "repo_sources": item.get("repo_sources", []),
                }
            )

        top_dates = sorted(missing_by_date.items(), key=lambda pair: (-pair[1], pair[0]))[:12]
        top_countries.sort(key=lambda item: (-item["missing_days"], item["country"]))
        summaries_by_window.append(
            {
                "dataset": "reports",
                "window": window,
                "period_start": periods[0][0].isoformat(),
                "period_end": periods[-1][1].isoformat(),
                "active_count": len(active),
                "empty_count": len(selected) - len(active),
                "complete_count": len(active) - countries_with_gaps,
                "countries_with_gaps": countries_with_gaps,
                "total_missing_days": total_missing_days,
                "max_missing_countries_per_date": max(missing_by_date.values()) if missing_by_date else 0,
                "top_dates": [{"date": date, "missing_countries": count} for date, count in top_dates],
                "top_countries": top_countries[:20],
            }
        )

    return {
        "periods": [{"start": start.isoformat(), "end": end.isoformat()} for start, end in periods],
        "summary": summaries_by_window,
    }


def workflow_runs(client: GitHub) -> list[dict[str, Any]]:
    out = []
    for repo, dataset, window, workflow, label in WORKFLOWS:
        path = f"/repos/{repo}/actions/workflows/{urllib.parse.quote(workflow, safe='')}/runs?per_page=5"
        try:
            payload, _ = client.get(path)
            runs = payload.get("workflow_runs", [])
            latest = runs[0] if runs else {}
            out.append(
                {
                    "repo": repo,
                    "dataset": dataset,
                    "window": window,
                    "workflow": workflow,
                    "label": label,
                    "status": latest.get("status"),
                    "conclusion": latest.get("conclusion"),
                    "event": latest.get("event"),
                    "created_at": latest.get("created_at"),
                    "updated_at": latest.get("updated_at"),
                    "head_sha": latest.get("head_sha"),
                    "html_url": latest.get("html_url"),
                    "recent_runs": [
                        {
                            "database_id": run.get("database_id"),
                            "status": run.get("status"),
                            "conclusion": run.get("conclusion"),
                            "event": run.get("event"),
                            "created_at": run.get("created_at"),
                            "updated_at": run.get("updated_at"),
                            "head_sha": run.get("head_sha"),
                            "html_url": run.get("html_url"),
                        }
                        for run in runs
                    ],
                }
            )
        except Exception as exc:
            out.append(
                {
                    "repo": repo,
                    "dataset": dataset,
                    "window": window,
                    "workflow": workflow,
                    "label": label,
                    "status": "error",
                    "conclusion": "error",
                    "error": str(exc),
                    "recent_runs": [],
                }
            )
    return out


def build_manifest(client: GitHub) -> dict[str, Any]:
    report_inventories = [inventory(releases_for_repo(client, repo), "reports", repo) for repo in REPORTS_REPOS]
    reports = merge_inventories(report_inventories)
    targeting = inventory(releases_for_repo(client, TARGETING_REPO), "targeting", TARGETING_REPO)
    rows, expected = coverage_rows(reports, targeting)
    rows.sort(key=lambda x: (x["dataset"], x["window"], x["status"], x["country"]))
    generated_at = now_utc()
    return {
        "generated_at": generated_at,
        "note": "GitHub-only coverage: release assets and workflow run metadata; no Meta endpoints are called.",
        "repos": {
            "reports": list(REPORTS_REPOS),
            "reports_primary": REPORTS_PRIMARY_REPO,
            "reports_legacy": REPORTS_REPOS[1],
            "targeting": TARGETING_REPO,
            "vpn_us": "favstats/metaus",
            "vpn_de": "favstats/metade",
        },
        "expected_dates": expected,
        "summary": summaries(rows),
        "gap_summary": gap_summary(rows, generated_at),
        "coverage": rows,
        "workflows": workflow_runs(client),
    }


def write_csv(rows: list[dict[str, Any]], path: Path) -> None:
    fields = [
        "dataset",
        "country",
        "window",
        "status",
        "latest_data_date",
        "expected_data_date",
        "lag_days",
        "asset_count",
        "dated_asset_count",
        "latest_asset_name",
        "latest_asset_updated_at",
        "latest_asset_size",
        "repo_sources",
        "source_latest_data_date",
        "source_dated_asset_count",
        "source_status",
        "source_repo_sources",
        "tag",
        "release_url",
    ]
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fields, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def badge(value: str | None) -> str:
    label = value or "unknown"
    klass = {
        "fresh": "ok",
        "success": "ok",
        "in_progress": "run",
        "queued": "run",
        "startup_queued": "run",
        "skipped_no_source": "skip",
        "empty_release": "skip",
        "cancelled": "skip",
        "lagging": "warn",
        "behind_source": "bad",
        "missing_targeting": "bad",
        "missing_release": "bad",
        "failure": "bad",
        "error": "bad",
    }.get(label, "warn")
    return f'<span class="badge {klass}">{html.escape(label)}</span>'


def render_summary(manifest: dict[str, Any]) -> str:
    cards = []
    for item in manifest["summary"]:
        counts = item["status_counts"]
        fresh = counts.get("fresh", 0)
        skipped = counts.get("skipped_no_source", 0) + counts.get("empty_release", 0)
        trouble = item["total"] - fresh - skipped
        cards.append(
            "\n".join(
                [
                    '<article class="card">',
                    f'<div class="eyebrow">{html.escape(item["dataset"])}</div>',
                    f'<h3>{html.escape(item["window"])}</h3>',
                    f'<div class="big">{fresh}/{item["total"]}</div>',
                    f'<p>Expected <strong>{html.escape(str(item.get("expected_data_date") or "none"))}</strong></p>',
                    f'<p>{badge("fresh")} {fresh} {badge("skipped_no_source")} {skipped} {badge("lagging")} {trouble}</p>',
                    "</article>",
                ]
            )
        )
    return "\n".join(cards)


def render_gap_summary(manifest: dict[str, Any]) -> str:
    gap_data = manifest.get("gap_summary", {})
    rows = []
    for item in gap_data.get("summary", []):
        top_dates = ", ".join(f"{entry['date']} ({entry['missing_countries']})" for entry in item.get("top_dates", [])[:4])
        top_countries = ", ".join(
            f"{entry['country']} ({entry['missing_days']})"
            for entry in item.get("top_countries", [])[:6]
        )
        rows.append(
            "<tr>"
            f"<td>{html.escape(item['window'])}</td>"
            f"<td>{html.escape(item['period_start'])} to {html.escape(item['period_end'])}</td>"
            f"<td>{item['complete_count']}/{item['active_count']}</td>"
            f"<td>{item['countries_with_gaps']}</td>"
            f"<td>{item['total_missing_days']}</td>"
            f"<td>{item['max_missing_countries_per_date']}</td>"
            f"<td>{html.escape(top_dates)}</td>"
            f"<td>{html.escape(top_countries)}</td>"
            "</tr>"
        )
    return "\n".join(rows)


def render_workflows(manifest: dict[str, Any]) -> str:
    rows = []
    for item in manifest["workflows"]:
        status = item.get("conclusion") or item.get("status")
        sha = (item.get("head_sha") or "")[:7]
        url = item.get("html_url") or "#"
        rows.append(
            "<tr>"
            f'<td><a href="{html.escape(url)}">{html.escape(item["label"])}</a></td>'
            f"<td>{html.escape(item['repo'])}</td>"
            f"<td>{badge(status)}</td>"
            f"<td>{html.escape(item.get('updated_at') or '')}</td>"
            f"<td>{html.escape(item.get('event') or '')}</td>"
            f"<td><code>{html.escape(sha)}</code></td>"
            "</tr>"
        )
    return "\n".join(rows)


def render_rows(manifest: dict[str, Any]) -> str:
    priority = {
        "missing_targeting": 0,
        "behind_source": 1,
        "missing_release": 2,
        "lagging": 3,
        "empty_release": 4,
        "skipped_no_source": 5,
        "fresh": 6,
    }
    rows = sorted(manifest["coverage"], key=lambda x: (priority.get(x["status"], 9), x["dataset"], x["window"], x["country"]))
    rendered = []
    for item in rows[:HTML_ROW_LIMIT]:
        url = item.get("release_url") or "#"
        lag = "" if item.get("lag_days") is None else str(item["lag_days"])
        repos = ", ".join(item.get("repo_sources") or item.get("source_repo_sources") or [])
        rendered.append(
            "<tr>"
            f"<td>{html.escape(item['country'])}</td>"
            f"<td>{html.escape(item['dataset'])}</td>"
            f"<td>{html.escape(item['window'])}</td>"
            f"<td>{badge(item['status'])}</td>"
            f"<td>{html.escape(str(item.get('latest_data_date') or ''))}</td>"
            f"<td>{html.escape(str(item.get('expected_data_date') or ''))}</td>"
            f"<td>{html.escape(lag)}</td>"
            f"<td>{html.escape(str(item.get('source_latest_data_date') or ''))}</td>"
            f"<td>{html.escape(repos)}</td>"
            f'<td><a href="{html.escape(url)}">{html.escape(item["tag"])}</a></td>'
            "</tr>"
        )
    return "\n".join(rendered)


def heatmap_payload(manifest: dict[str, Any]) -> dict[str, Any]:
    rows = []
    for item in manifest["coverage"]:
        rows.append(
            {
                "country": item["country"],
                "dataset": item["dataset"],
                "window": item["window"],
                "status": item["status"],
                "ranges": item.get("available_ranges", []),
                "source_ranges": item.get("source_available_ranges", []),
                "latest": item.get("latest_data_date"),
                "expected": item.get("expected_data_date"),
                "source_latest": item.get("source_latest_data_date"),
                "repo_sources": item.get("repo_sources", []),
                "source_repo_sources": item.get("source_repo_sources", []),
            }
        )

    return {
        "generated_at": manifest["generated_at"],
        "windows": {
            "reports": list(REPORT_WINDOWS),
            "targeting": list(TARGETING_WINDOWS),
        },
        "rows": rows,
    }


def write_html(manifest: dict[str, Any], path: Path) -> None:
    heatmap_json = (
        json.dumps(heatmap_payload(manifest), separators=(",", ":"))
        .replace("<", "\\u003c")
        .replace(">", "\\u003e")
        .replace("&", "\\u0026")
    )
    template = """<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Meta Scraper Coverage</title>
  <link rel="icon" href="data:,">
  <style>
    :root { --ink:#17202a; --muted:#627083; --line:#d9e0e7; --paper:#fff; --soft:#f4f7fa; --ok:#16794c; --warn:#a15c00; --bad:#b3261e; --skip:#596579; --run:#3867b7; }
    * { box-sizing: border-box; }
    body { margin:0; font:14px/1.45 -apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif; color:var(--ink); background:var(--soft); }
    header { padding:28px clamp(18px,4vw,48px) 16px; background:var(--paper); border-bottom:1px solid var(--line); }
    main { padding:18px clamp(18px,4vw,48px) 42px; }
    h1 { margin:0 0 8px; font-size:clamp(24px,3vw,36px); letter-spacing:0; }
    h2 { margin:28px 0 12px; font-size:18px; letter-spacing:0; }
    h3 { margin:4px 0 8px; font-size:16px; letter-spacing:0; }
    p { margin:0 0 8px; color:var(--muted); }
    a { color:#1d5fa7; text-decoration:none; }
    a:hover { text-decoration:underline; }
    code { background:#eef3f7; padding:2px 5px; border-radius:4px; }
    .note { max-width:920px; }
    .grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:10px; }
    .card { background:var(--paper); border:1px solid var(--line); border-radius:8px; padding:14px; min-height:142px; }
    .eyebrow { color:var(--muted); font-size:12px; text-transform:uppercase; letter-spacing:.04em; }
    .big { font-size:28px; font-weight:750; margin:4px 0 8px; }
    .toolbar { display:flex; flex-wrap:wrap; gap:8px; align-items:center; margin:8px 0 12px; }
    .toolbar label { display:inline-flex; gap:6px; align-items:center; color:var(--muted); }
    select { height:34px; border:1px solid var(--line); border-radius:6px; padding:0 10px; background:var(--paper); color:var(--ink); }
    .legend { display:flex; flex-wrap:wrap; gap:10px; margin:10px 0 12px; color:var(--muted); }
    .key { display:inline-flex; align-items:center; gap:6px; }
    .swatch { width:14px; height:14px; border-radius:3px; border:1px solid rgba(0,0,0,.08); }
    .heatmap-shell { border:1px solid var(--line); border-radius:8px; background:var(--paper); overflow:hidden; }
    .heatmap-head { display:grid; grid-template-columns:72px minmax(0,1fr); border-bottom:1px solid var(--line); background:#f8fafc; }
    .heatmap-corner { height:58px; display:flex; align-items:end; padding:0 10px 10px; border-right:1px solid var(--line); color:var(--muted); font-size:12px; font-weight:650; text-transform:uppercase; }
    .hm-axis-viewport { height:58px; overflow:hidden; position:relative; }
    .heatmap-body { display:grid; grid-template-columns:72px minmax(0,1fr); max-height:720px; }
    .hm-label-viewport { overflow:hidden; border-right:1px solid var(--line); background:#fff; }
    .hm-scroll { overflow:auto; max-height:720px; background:#fff; }
    #hmXAxis, #hmYAxis, #heatmap { display:block; transform-origin:0 0; }
    #hmXAxis, #hmYAxis { will-change:transform; }
    #hmTip { min-height:24px; margin:8px 0 0; color:var(--muted); }
    .table-wrap { overflow:auto; border:1px solid var(--line); border-radius:8px; background:var(--paper); }
    table { border-collapse:collapse; min-width:940px; width:100%; }
    th,td { padding:9px 10px; border-bottom:1px solid var(--line); text-align:left; white-space:nowrap; }
    th { background:#eef3f7; font-size:12px; color:#334155; position:sticky; top:0; z-index:1; }
    tr:last-child td { border-bottom:0; }
    .badge { display:inline-flex; align-items:center; min-height:22px; padding:2px 7px; border-radius:999px; font-size:12px; font-weight:650; background:#edf1f5; }
    .ok { color:var(--ok); background:#e7f4ee; }
    .warn { color:var(--warn); background:#fff1dc; }
    .bad { color:var(--bad); background:#fdebea; }
    .skip { color:var(--skip); background:#eef1f5; }
    .run { color:var(--run); background:#e9f0fb; }
  </style>
</head>
<body>
  <header>
    <h1>Meta Scraper Coverage</h1>
    <p class="note">Generated __GENERATED_AT__. GitHub-only observability: release assets and workflow metadata, no Meta endpoints. Report coverage merges __REPORT_REPOS__.</p>
  </header>
  <main>
    <section>
      <h2>Coverage</h2>
      <div class="grid">__SUMMARY__</div>
    </section>
    <section>
      <h2>Backfill Gaps</h2>
      <p>Previous two completed months, using merged report release assets across current and legacy report repos.</p>
      <div class="table-wrap">
        <table>
          <thead><tr><th>Window</th><th>Period</th><th>Complete Countries</th><th>Countries With Gaps</th><th>Missing Country-Days</th><th>Worst Date Count</th><th>Worst Dates</th><th>Worst Countries</th></tr></thead>
          <tbody>__GAP_SUMMARY__</tbody>
        </table>
      </div>
    </section>
    <section>
      <h2>Master Heatmap</h2>
      <p>Country rows by date columns, built from release asset filenames only.</p>
      <div class="toolbar">
        <label>Dataset <select id="hmDataset"><option value="reports">reports</option><option value="targeting">targeting</option></select></label>
        <label>Window <select id="hmWindow"></select></label>
        <label>Range <select id="hmRange"><option value="90">90 days</option><option value="180">180 days</option><option value="365" selected>365 days</option><option value="all">all</option></select></label>
        <label>Density <select id="hmDensity"><option value="readable" selected>readable</option><option value="compact">compact</option><option value="wide">wide</option></select></label>
        <label>Sort <select id="hmSort"><option value="country">country</option><option value="status">status</option></select></label>
      </div>
      <div class="legend">
        <span class="key"><span class="swatch" style="background:#16794c"></span>available</span>
        <span class="key" title="For targeting rows: the source reports file exists for that date, but the targeting file does not."><span class="swatch" style="background:#e0a321"></span>source report only</span>
        <span class="key"><span class="swatch" style="background:#d6dde6"></span>no source / skipped</span>
        <span class="key"><span class="swatch" style="background:#f3f6f8"></span>missing</span>
      </div>
      <div class="heatmap-shell">
        <div class="heatmap-head">
          <div class="heatmap-corner">Country</div>
          <div class="hm-axis-viewport" id="hmAxisViewport">
            <canvas id="hmXAxis" width="1200" height="58"></canvas>
          </div>
        </div>
        <div class="heatmap-body">
          <div class="hm-label-viewport" id="hmLabelViewport">
            <canvas id="hmYAxis" width="72" height="600"></canvas>
          </div>
          <div class="hm-scroll" id="hmScroll">
            <canvas id="heatmap" width="1200" height="600"></canvas>
          </div>
        </div>
      </div>
      <p id="hmTip"></p>
    </section>
    <section>
      <h2>Workflow Runs</h2>
      <div class="table-wrap">
        <table>
          <thead><tr><th>Workflow</th><th>Repo</th><th>Status</th><th>Updated</th><th>Event</th><th>SHA</th></tr></thead>
          <tbody>__WORKFLOWS__</tbody>
        </table>
      </div>
    </section>
    <section>
      <h2>Rows Needing Attention First</h2>
      <p>Showing the first __ROW_LIMIT__ rows ordered by attention priority. The complete machine-readable manifest is in coverage.json.</p>
      <div class="table-wrap">
        <table>
          <thead><tr><th>Country</th><th>Dataset</th><th>Window</th><th>Status</th><th>Latest</th><th>Expected</th><th>Lag</th><th>Source Latest</th><th>Repos</th><th>Release</th></tr></thead>
          <tbody>__ROWS__</tbody>
        </table>
      </div>
    </section>
  </main>
  <script id="heatmapData" type="application/json">__HEATMAP_JSON__</script>
  <script>
    const heatmapData = JSON.parse(document.getElementById('heatmapData').textContent);
    const hmDataset = document.getElementById('hmDataset');
    const hmWindow = document.getElementById('hmWindow');
    const hmRange = document.getElementById('hmRange');
    const hmDensity = document.getElementById('hmDensity');
    const hmSort = document.getElementById('hmSort');
    const canvas = document.getElementById('heatmap');
    const axisCanvas = document.getElementById('hmXAxis');
    const labelCanvas = document.getElementById('hmYAxis');
    const scrollEl = document.getElementById('hmScroll');
    const labelViewport = document.getElementById('hmLabelViewport');
    const heatmapBody = document.querySelector('.heatmap-body');
    const tip = document.getElementById('hmTip');
    const ctx = canvas.getContext('2d');
    const axisCtx = axisCanvas.getContext('2d');
    const labelCtx = labelCanvas.getContext('2d');
    const colors = {
      available: '#16794c',
      sourceOnly: '#e0a321',
      noSource: '#d6dde6',
      missing: '#f3f6f8',
      line: '#d9e0e7',
      majorLine: '#9aa7b5',
      stripe: '#fafcfe',
      axisBg: '#f8fafc',
      axisBand: '#edf3f7',
      text: '#17202a',
      muted: '#627083'
    };
    let drawState = null;

    function parseDate(value) {
      const parts = value.split('-').map(Number);
      return new Date(Date.UTC(parts[0], parts[1] - 1, parts[2]));
    }

    function formatDate(date) {
      return date.toISOString().slice(0, 10);
    }

    function rangeDateSet(ranges) {
      const out = new Set();
      ranges.forEach(range => {
        const start = parseDate(range[0]);
        const end = parseDate(range[1]);
        const total = daysBetween(start, end) + 1;
        for (let index = 0; index < total; index += 1) {
          const date = new Date(start);
          date.setUTCDate(date.getUTCDate() + index);
          out.add(formatDate(date));
        }
      });
      return out;
    }

    function daysBetween(start, end) {
      return Math.round((end - start) / 86400000);
    }

    function updateWindowOptions() {
      const windows = heatmapData.windows[hmDataset.value];
      hmWindow.innerHTML = windows.map(windowName => `<option value="${windowName}">${windowName}</option>`).join('');
      if (hmDataset.value === 'reports' && windows.includes('last_30_days')) hmWindow.value = 'last_30_days';
      if (hmDataset.value === 'targeting' && windows.includes('last_7_days')) hmWindow.value = 'last_7_days';
    }

    function monthLabel(date) {
      return date.toLocaleString('en-US', { month: 'short', timeZone: 'UTC' });
    }

    function setCanvasSize(targetCanvas, targetCtx, cssW, cssH) {
      const ratio = window.devicePixelRatio || 1;
      targetCanvas.style.width = `${cssW}px`;
      targetCanvas.style.height = `${cssH}px`;
      targetCanvas.width = Math.ceil(cssW * ratio);
      targetCanvas.height = Math.ceil(cssH * ratio);
      targetCtx.setTransform(ratio, 0, 0, ratio, 0, 0);
      targetCtx.clearRect(0, 0, cssW, cssH);
    }

    function heatmapCellWidth(dateCount) {
      const base = hmRange.value === '90' ? 10 : hmRange.value === '180' ? 8 : hmRange.value === '365' ? 6 : (dateCount > 800 ? 4 : 5);
      const multiplier = hmDensity.value === 'compact' ? 0.7 : hmDensity.value === 'wide' ? 1.35 : 1;
      return Math.max(2, Math.round(base * multiplier));
    }

    function segments(dates, precision) {
      if (!dates.length) return [];
      const keyFor = date => precision === 'year' ? date.slice(0, 4) : date.slice(0, 7);
      const out = [];
      let key = keyFor(dates[0]);
      let start = 0;
      for (let index = 1; index <= dates.length; index += 1) {
        const nextKey = index < dates.length ? keyFor(dates[index]) : null;
        if (nextKey === key) continue;
        out.push({ key, start, end: index - 1, width: index - start });
        key = nextKey;
        start = index;
      }
      return out;
    }

    function statusColor(row) {
      if (row.status === 'fresh') return colors.available;
      if (row.status === 'lagging' || row.status === 'behind_source') return colors.sourceOnly;
      if (row.status === 'skipped_no_source' || row.status === 'empty_release') return colors.noSource;
      return '#b3261e';
    }

    function selectedRows() {
      let rows = heatmapData.rows.filter(row => row.dataset === hmDataset.value && row.window === hmWindow.value);
      if (hmSort.value === 'status') {
        const rank = { missing_targeting: 0, behind_source: 1, missing_release: 2, lagging: 3, empty_release: 4, skipped_no_source: 5, fresh: 6 };
        rows = rows.slice().sort((a, b) => (rank[a.status] ?? 9) - (rank[b.status] ?? 9) || a.country.localeCompare(b.country));
      } else {
        rows = rows.slice().sort((a, b) => a.country.localeCompare(b.country));
      }
      return rows;
    }

    function dateExtent(rows) {
      const values = [];
      rows.forEach(row => {
        row.ranges.forEach(range => values.push(range[0], range[1]));
        row.source_ranges.forEach(range => values.push(range[0], range[1]));
        if (row.expected) values.push(row.expected);
      });
      values.sort();
      const fallback = formatDate(new Date());
      return { min: values[0] || fallback, max: values[values.length - 1] || fallback };
    }

    function visibleDates(rows) {
      const extent = dateExtent(rows);
      let start = parseDate(extent.min);
      const end = parseDate(extent.max);
      if (hmRange.value !== 'all') {
        const days = Number(hmRange.value);
        const candidate = new Date(end);
        candidate.setUTCDate(candidate.getUTCDate() - days + 1);
        if (candidate > start) start = candidate;
      }
      const total = daysBetween(start, end) + 1;
      return Array.from({ length: total }, (_, index) => {
        const date = new Date(start);
        date.setUTCDate(date.getUTCDate() + index);
        return formatDate(date);
      });
    }

    function cellColor(row, date) {
      if (row.dateSet.has(date)) return colors.available;
      if (row.dataset === 'targeting' && row.sourceDateSet.has(date)) return colors.sourceOnly;
      if ((row.status === 'skipped_no_source' || row.status === 'empty_release') && row.sourceDateSet.size === 0) return colors.noSource;
      return colors.missing;
    }

    function drawXAxis(dates, cellW, chartW, axisH) {
      setCanvasSize(axisCanvas, axisCtx, chartW, axisH);
      axisCtx.fillStyle = colors.axisBg;
      axisCtx.fillRect(0, 0, chartW, axisH);
      axisCtx.fillStyle = colors.axisBand;
      axisCtx.fillRect(0, 0, chartW, 23);
      axisCtx.textBaseline = 'middle';
      axisCtx.textAlign = 'center';

      segments(dates, 'year').forEach(segment => {
        const x = segment.start * cellW;
        const width = segment.width * cellW;
        axisCtx.fillStyle = colors.majorLine;
        axisCtx.fillRect(x, 0, 1, axisH);
        if (width >= 32) {
          axisCtx.font = '12px -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif';
          axisCtx.fillStyle = colors.text;
          axisCtx.fillText(segment.key, x + width / 2, 11);
        }
      });

      segments(dates, 'month').forEach(segment => {
        const x = segment.start * cellW;
        const width = segment.width * cellW;
        const date = parseDate(`${segment.key}-01`);
        const month = date.getUTCMonth();
        const shouldLabel = width >= 28 || month % 3 === 0;
        axisCtx.fillStyle = month === 0 ? colors.majorLine : colors.line;
        axisCtx.fillRect(x, 23, month === 0 ? 2 : 1, axisH - 23);
        if (shouldLabel && width >= 16) {
          axisCtx.font = '11px -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif';
          axisCtx.fillStyle = colors.muted;
          axisCtx.fillText(month === 0 ? `${monthLabel(date)} ${date.getUTCFullYear()}` : monthLabel(date), x + width / 2, 41);
        }
      });
    }

    function drawLabels(rows, rowH, labelW, chartH) {
      setCanvasSize(labelCanvas, labelCtx, labelW, chartH);
      labelCtx.fillStyle = '#ffffff';
      labelCtx.fillRect(0, 0, labelW, chartH);
      labelCtx.font = '11px -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif';
      labelCtx.textBaseline = 'middle';
      rows.forEach((row, rowIndex) => {
        const y = rowIndex * rowH;
        if (rowIndex % 2 === 1) {
          labelCtx.fillStyle = colors.stripe;
          labelCtx.fillRect(0, y, labelW, rowH);
        }
        labelCtx.fillStyle = statusColor(row);
        labelCtx.fillRect(8, y + Math.max(2, rowH / 2 - 3), 6, 6);
        labelCtx.fillStyle = colors.text;
        labelCtx.fillText(row.country, 22, y + rowH / 2);
      });
    }

    function drawGrid(rows, dates, cellW, rowH, chartW, chartH) {
      setCanvasSize(canvas, ctx, chartW, chartH);
      ctx.fillStyle = '#ffffff';
      ctx.fillRect(0, 0, chartW, chartH);

      rows.forEach((row, rowIndex) => {
        const y = rowIndex * rowH;
        if (rowIndex % 2 === 1) {
          ctx.fillStyle = colors.stripe;
          ctx.fillRect(0, y, chartW, rowH);
        }
        dates.forEach((date, dateIndex) => {
          ctx.fillStyle = cellColor(row, date);
          ctx.fillRect(dateIndex * cellW, y + 1, Math.max(1, cellW - 1), rowH - 2);
        });
      });

      segments(dates, 'month').forEach(segment => {
        const month = parseDate(`${segment.key}-01`).getUTCMonth();
        ctx.fillStyle = month === 0 ? colors.majorLine : colors.line;
        ctx.fillRect(segment.start * cellW, 0, month === 0 ? 2 : 1, chartH);
      });

      for (let rowIndex = 10; rowIndex < rows.length; rowIndex += 10) {
        ctx.fillStyle = colors.line;
        ctx.fillRect(0, rowIndex * rowH, chartW, 1);
      }
    }

    function syncScrollChrome() {
      axisCanvas.style.transform = `translateX(${-scrollEl.scrollLeft}px)`;
      labelCanvas.style.transform = `translateY(${-scrollEl.scrollTop}px)`;
    }

    function drawHeatmap() {
      const keepLatestVisible = !drawState || scrollEl.scrollLeft + scrollEl.clientWidth >= scrollEl.scrollWidth - 24;
      const rows = selectedRows().map(row => ({
        ...row,
        dateSet: rangeDateSet(row.ranges),
        sourceDateSet: rangeDateSet(row.source_ranges)
      }));
      const dates = visibleDates(rows);
      const labelW = 72;
      const axisH = 58;
      const rowH = 10;
      const cellW = heatmapCellWidth(dates.length);
      const chartW = Math.max(1, dates.length * cellW);
      const chartH = Math.max(1, rows.length * rowH);
      const viewportH = Math.min(720, chartH);
      heatmapBody.style.height = `${viewportH}px`;
      labelViewport.style.height = `${viewportH}px`;
      scrollEl.style.maxHeight = `${viewportH}px`;

      drawXAxis(dates, cellW, chartW, axisH);
      drawLabels(rows, rowH, labelW, chartH);
      drawGrid(rows, dates, cellW, rowH, chartW, chartH);

      drawState = { rows, dates, rowH, cellW };
      syncScrollChrome();
      if (keepLatestVisible) {
        requestAnimationFrame(() => {
          scrollEl.scrollLeft = scrollEl.scrollWidth;
          syncScrollChrome();
        });
      }
      tip.textContent = `${rows.length} countries x ${dates.length} days for ${hmDataset.value} ${hmWindow.value}.`;
    }

    canvas.addEventListener('mousemove', event => {
      if (!drawState) return;
      const rect = canvas.getBoundingClientRect();
      const x = event.clientX - rect.left;
      const y = event.clientY - rect.top;
      const dateIndex = Math.floor(x / drawState.cellW);
      const rowIndex = Math.floor(y / drawState.rowH);
      if (dateIndex < 0 || rowIndex < 0 || dateIndex >= drawState.dates.length || rowIndex >= drawState.rows.length) {
        tip.textContent = `${drawState.rows.length} countries x ${drawState.dates.length} days for ${hmDataset.value} ${hmWindow.value}.`;
        return;
      }
      const row = drawState.rows[rowIndex];
      const date = drawState.dates[dateIndex];
      const state = row.dateSet.has(date) ? 'available' : (row.dataset === 'targeting' && row.sourceDateSet.has(date) ? 'source report only' : (row.status === 'skipped_no_source' ? 'no source' : 'missing'));
      const repos = (row.repo_sources && row.repo_sources.length) ? ` via ${row.repo_sources.join(', ')}` : '';
      tip.textContent = `${row.country} ${row.dataset} ${row.window} ${date}: ${state} (${row.status})${repos}`;
    });

    scrollEl.addEventListener('scroll', syncScrollChrome, { passive: true });

    [hmDataset, hmRange, hmDensity, hmSort].forEach(element => element.addEventListener('change', () => {
      if (element === hmDataset) updateWindowOptions();
      drawHeatmap();
    }));
    hmWindow.addEventListener('change', drawHeatmap);
    updateWindowOptions();
    drawHeatmap();
  </script>
</body>
</html>
"""
    rendered = (
        template.replace("__GENERATED_AT__", html.escape(manifest["generated_at"]))
        .replace("__REPORT_REPOS__", html.escape(", ".join(manifest["repos"].get("reports", []))))
        .replace("__SUMMARY__", render_summary(manifest))
        .replace("__GAP_SUMMARY__", render_gap_summary(manifest))
        .replace("__WORKFLOWS__", render_workflows(manifest))
        .replace("__ROW_LIMIT__", str(HTML_ROW_LIMIT))
        .replace("__ROWS__", render_rows(manifest))
        .replace("__HEATMAP_JSON__", heatmap_json)
    )
    path.write_text(rendered, encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", default="coverage")
    args = parser.parse_args()

    output_dir = Path(args.output)
    output_dir.mkdir(parents=True, exist_ok=True)
    client = GitHub(os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN"))

    manifest = build_manifest(client)
    (output_dir / "coverage.json").write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    write_csv(manifest["coverage"], output_dir / "coverage.csv")
    write_html(manifest, output_dir / "index.html")
    print(f"Generated {output_dir / 'coverage.json'}")
    print(f"Generated {output_dir / 'coverage.csv'}")
    print(f"Generated {output_dir / 'index.html'}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
