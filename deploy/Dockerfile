FROM ghcr.io/integratedbreedingplatform/shinybrapps_base:dev

## Make the app and the container run on 3838
RUN echo "\noptions(shiny.port=3838, shiny.host='0.0.0.0')" >> /usr/local/lib/R/etc/Rprofile.site
EXPOSE 3838

COPY . /shinybrapps_pkg
RUN R -e "install.packages('/shinybrapps_pkg', repos = NULL, type = 'source', dependencies = T)"

## Run the app when the container starts
CMD ["R", "-e", "shinybrapps::run_test()"]
