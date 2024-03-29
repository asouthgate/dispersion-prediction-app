FROM rocker/shiny:4.0.5
# Install system requirements for index.R as needed
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libudunits2-dev \
    libgeos-dev \
    libgdal-dev \
    julia \
    libpq-dev \
    libproj-dev \
    postgresql \
    postgis \
    curl \
    slurmd \
    slurmctld \
    munge \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
ENV _R_SHLIB_STRIP_=true
#COPY Rprofile.site /etc/R
RUN install2.r --error --skipinstalled \
    shiny \
    forecast \
    jsonlite \
    ggplot2 \
    htmltools \
    plotly \
    bslib \
    DBI \
    ggplot2 \
    glue \
    grid \
    gridExtra \
    JuliaCall \
    leaflet \
    logger \
    mockery \
    mockr \
    R6 \
    raster \
    Rcpp \
    rpostgis \
    rgdal \
    sf \
    shiny \
    shinyBS \
    shinybusy \
    shinyjs \
    sp \
    stringr \
    terra \
    testthat \
    configr \
    uuid \
    shinycssloaders \
    future \
    vroom

RUN julia -e 'using Pkg; \
                Pkg.add("UpdateJulia"); \
                using UpdateJulia; \
                update_julia()'
RUN sudo -u shiny julia -e 'using Pkg; \
                Pkg.add("Circuitscape")'
COPY . /srv/shiny-server/batApp
COPY shiny-server.conf /etc/shiny-server
COPY R/slurm.conf /etc/slurm-llnl/
COPY start.sh /sbin/
USER root
EXPOSE 3838
ENV SHINY_LOG_STDERR=1
CMD /sbin/start.sh
