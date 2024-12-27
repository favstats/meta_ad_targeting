# Meta Ad Targeting

This repository is designed to streamline the retrieval and processing of targeting data from Meta's advertising platform. The hope is that this tool is useful for researchers, analysts, and organizations seeking to understand ad targeting strategies across different countries and timeframes. 

## Purpose
Meta's advertising platform only provides data for limited, predefined timeframes: the last 7, 30, and 90 days. Consequently, historical data is often inaccessible once it surpasses these time windows. This repository addresses this limitation by archiving and structuring historical data, enabling researchers and analysts to access datasets that would otherwise be unavailable.

This repository provides a framework to:
- Access and process data on political or commercial ad targeting.
- Analyze targeting data at a granular level by country, timeframe, and targeting criteria.
- Automate workflows for downloading and storing audience insights for the predefined periods (7, 30, or 90 days) and preserving this data for long-term analysis.

It integrates with the `metatargetr` package to fetch audience targeting data directly from Meta's GitHub releases.

## Key Features
- **Automated Data Retrieval:** `get_targeting_db` enables easy access to targeting datasets stored as `.parquet` files on GitHub.
- **Support for Multiple Countries:** Retrieve data across a wide range of countries using their ISO codes.
- **Custom Timeframes:** Specify timeframes (e.g., `LAST_30_DAYS`) to tailor the analysis to specific periods.
- **Data Cleaning and Integration:** Seamlessly combines data from different sources, such as Meta's reports, (PartyFacts)[https://partyfacts.herokuapp.com/] database into one unified dataset.

## Example Use Case
A typical workflow might involve:
1. Specifying a country (e.g., "DE" for Germany) and timeframe (e.g., 30 days).
2. Using the `get_targeting_db` function to fetch the latest targeting data:
   ```r
   latest_data <- get_targeting_db(the_cntry = "DE", tf = 30, ds = "2024-10-25")
