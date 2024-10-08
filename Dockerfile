# dockerfile for deploying shiny-r apps
FROM rocker/shiny:4.3.3

WORKDIR /srv

# install requirements for sf package
RUN apt-get -y update && apt-get install -y  libudunits2-dev libgdal-dev libgeos-dev libproj-dev && rm -rf /var/lib/apt/lists/*

# Caching Introduced here
COPY install.R /srv/
RUN Rscript install.R

COPY . /srv

ENTRYPOINT ["Rscript", "startApp.R"]
