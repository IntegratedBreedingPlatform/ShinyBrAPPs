FROM shinybrapps_base
COPY . /shinybrapps_pkg
RUN R -e "install.packages('/shinybrapps_pkg', repos = NULL, type = 'source', dependencies = T)"

## Run the app when the container starts
CMD ["R", "-e", "shinybrapps::run_test()"]