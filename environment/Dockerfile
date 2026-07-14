# hash:sha256:dad9380009d2c6378e3a4de99da45ec7b0a9573aab02bf5c7044c3d70cfdeb18
FROM registry.codeocean.com/codeocean/r-studio:r-4.5.1-1.2204.0-rstudio-server-2025.05.1-513-ubuntu22.04

ARG DEBIAN_FRONTEND=noninteractive

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

RUN Rscript -e 'install.packages(c( \
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
