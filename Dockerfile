FROM rocker/geospatial

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

#RUN install2.r \
#    tidyverse \
#    sf \
#    tigris \
#    tmap \
#    shinydashboard \
#    shinycssloaders

COPY . /src
WORKDIR /src

RUN Rscript -e "devtools::install()"

# Load datasets into cache, will need to remove to get latest data
# RUN Rscript -e "US.covid.dashboard::download_datasets()"

CMD ["Rscript", "-e", "US.covid.dashboard::runApp()"]
