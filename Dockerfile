FROM rocker/shiny:latest
MAINTAINER Ben Moore "ben@blm.io"

COPY . /srv/shiny-server/.

RUN Rscript -e "install.packages( \
    c('shiny', 'DT', 'dplyr', 'leaflet', \
      'ggplot2', 'htmltools', 'zoo'), clean=T, quiet=T)"

EXPOSE 3838

ENTRYPOINT ["/usr/bin/shiny-server.sh"]