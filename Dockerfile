FROM r-base

## Make the app and the container run on 3838
RUN echo "local({options(shiny.port = 3838, shiny.host = '0.0.0.0')})" > /usr/lib/R/etc/Rprofile.site
EXPOSE 3838 

## Install debian libs
RUN apt-get update && apt-get install -y \
	libcurl4-openssl-dev \
	libssl-dev

## Install R deps
RUN R -e "install.packages('shiny', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('data.table', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RColorBrewer', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('e1071', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('mverouden/brapir-v1')"
RUN R -e "remotes::install_github('mverouden/brapir-v2')"
RUN R -e "install.packages('viridisLite', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gridExtra', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('statgenSTA', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('SpATS', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('AnalytixWare/ShinySky')"
RUN R -e "install.packages('shinyBS', dependencies=TRUE, repos='http://cran.rstudio.com/')"

## Install shinybrapps
# Note: presently the image is constructed from within the package directory because the repo "IntegratedBreedingPlatform/ShinyBrAPPs" is private. If it becomes public, it will be possible to construct the image from anywhere via "install_gihub()"
COPY . /shinybrapps_pkg
RUN R -e "install.packages('/shinybrapps_pkg', repos = NULL, type = 'source', dependencies = T)"

## Run the app when the container starts
CMD ["R", "-e", "shinybrapps::run_test()"]

