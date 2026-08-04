FROM rocker/shiny:4.3.2

# Install system dependencies for spatial features (sf, GDAL, GEOS, PROJ)
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('bslib', 'ggplot2', 'plotly', 'leaflet', 'dplyr', 'tidyr', 'sf', 'jsonlite'), repos='https://cloud.r-project.org/')"

# Copy application files into shiny-server web root
COPY . /srv/shiny-server/

# Ensure proper permissions
RUN chown -R shiny:shiny /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
