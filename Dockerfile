FROM rocker/r-ver:4.4.1

## Make the app and the container run on 3838
RUN echo "\noptions(shiny.port=3838, shiny.host='0.0.0.0')" >> /usr/local/lib/R/etc/Rprofile.site
EXPOSE 3838 

## Install debian libs
RUN apt-get update && apt-get install -y \
	binutils \
	libxml2-dev \
	libssl-dev \
	libcurl4-openssl-dev \
	libgdal-dev \
	libfontconfig1-dev \
	pandoc \
	cmake

## Install R deps
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyBS', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('data.table', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RColorBrewer', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('e1071', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('viridisLite', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gridExtra', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('statgenSTA', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('SpATS', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('varhandle', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggpubr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggExtra', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('mverouden/brapir-v1')"
RUN R -e "install.packages('rclipboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('openxlsx', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gtools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinybusy', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('aliceboizet/brapir-v2@fix-perf-observationunits')"
RUN R -e "install.packages('statgenGxE', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggnewscale', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('metan', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_gitlab('alice.boizet/brapir', host = 'https://gitlab.cirad.fr')"
RUN R -e "install.packages('stringmagic', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('officer', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('cowplot', repos='http://cran.rstudio.com/')"


## Install shinybrapps
# Note: presently the image is constructed from within the package directory because the repo "IntegratedBreedingPlatform/ShinyBrAPPs" is private. If it becomes public, it will be possible to construct the image from anywhere via "install_gihub()"
COPY . /shinybrapps_pkg
RUN R -e "install.packages('/shinybrapps_pkg', repos = NULL, type = 'source', dependencies = T)"

## Run the app when the container starts
CMD ["R", "-e", "shinybrapps::run_test()"]

