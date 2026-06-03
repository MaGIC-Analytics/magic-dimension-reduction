FROM rocker/shiny-verse:4.5.3
LABEL authors="Alex Lemenze" \
    description="Docker image for MaGIC Dimensionality Reduction Tool"

RUN apt-get update && apt-get install -y \
    sudo libhdf5-dev build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev libv8-dev libsodium-dev libglpk40 libproj-dev libgdal-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('BiocManager','shiny','shinythemes','shinycssloaders', 'shinyWidgets','devtools','DT','tidyverse','data.table','RColorBrewer','colourpicker','Rtsne','tsne','umap','plotly','remotes'),repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('proj4','ggalt'),dependencies=T)"
RUN R -e "devtools::install_github('hrbrmstr/ggalt')"
RUN R -e "remotes::install_github('kevinblighe/PCAtools')"
RUN R -e "install.packages('ggforce',repos='http://cran.rstudio.com/')"

COPY ./app /srv/shiny-server/
COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 8080
USER shiny
CMD ["/usr/bin/shiny-server"]
