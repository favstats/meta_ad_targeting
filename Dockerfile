FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      dirmngr gnupg apt-transport-https ca-certificates software-properties-common \
      wget curl locales tzdata \
      libssl1.1 libcurl4-openssl-dev libxml2-dev libfontconfig1-dev \
      libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
      libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
      build-essential

RUN locale-gen en_US.UTF-8

# Modern way to add CRAN key (no apt-key)
RUN mkdir -p /etc/apt/keyrings && \
    wget -O /etc/apt/keyrings/cran.asc https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc && \
    echo "deb [signed-by=/etc/apt/keyrings/cran.asc] https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/" > /etc/apt/sources.list.d/cran.list

RUN apt-get update && \
    apt-get install -y --no-install-recommends r-base r-base-dev

RUN wget -qO- https://quarto.org/download/latest/quarto-linux-amd64.deb > /tmp/quarto.deb && \
    dpkg -i /tmp/quarto.deb || apt-get install -fy && \
    rm /tmp/quarto.deb

RUN echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' >> /etc/R/Rprofile.site