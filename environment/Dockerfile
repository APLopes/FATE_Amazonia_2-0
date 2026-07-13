# FATE-SEEG Compute Environment
# R 4.1.2 + required packages
# Based on rocker/r-ver for reliable R installation

FROM rocker/r-ver:4.1.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c( \
    "data.table", \
    "openxlsx", \
    "dplyr", \
    "reshape2", \
    "tidyr", \
    "ggplot2", \
    "scales", \
    "egg", \
    "svglite", \
    "plyr", \
    "viridis" \
  ), repos = "https://cran.r-project.org")'

RUN echo "FATE-SEEG environment ready."
