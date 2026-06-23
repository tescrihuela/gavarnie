FROM rocker/r-ver:4.3.2

RUN Rscript -e "install.packages(c('rsconnect', 'rjson', 'shiny', 'bs4Dash', 'leaflet', 'dplyr', 'htmltools', 'sf'))"
COPY . /app
WORKDIR /app

CMD ["Rscript", "deploy.R"]
