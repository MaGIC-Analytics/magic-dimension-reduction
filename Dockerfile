FROM rocker/shiny:4.2.0
LABEL authors="Alex Lemenze" \
    description="Docker image containing the MaGIC Modules Template."

RUN apt-get update && apt-get install -y \ 
    sudo libhdf5-dev build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev libv8-dev libsodium-dev libglpk40 libproj-dev libgdal-dev

RUN R -e "install.packages(c('BiocManager','shiny','shinythemes','shinycssloaders', 'shinyWidgets','devtools','DT','tidyverse','data.table','RColorBrewer','colourpicker','Rtsne','tsne','umap','plotly','remotes'),repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('proj4','ggalt'),dependencies=T)"
RUN R -e "devtools::install_github('hrbrmstr/ggalt')"
RUN R -e "remotes::install_github('kevinblighe/PCAtools')"

COPY ./app /srv/shiny-server/
COPY shiny-customized.config /etc/shiny-server/shiny-server.conf
RUN sudo chown -R shiny:shiny /srv/shiny-server
EXPOSE 8080

USER shiny
CMD ["/usr/bin/shiny-server"]