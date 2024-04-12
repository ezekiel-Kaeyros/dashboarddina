# Base R Shiny image
FROM rocker/shiny

# Installation de l'openjdk
RUN apt-get update && apt-get install -y openjdk-8-jdk

# Installation des dépendances R spécifiées
RUN R -e "install.packages(c('dplyr', 'ggplot2', 'glue', 'htmlwidgets', 'httr', 'jsonlite', 'keyring', 'LDAvis', 'leaflet', 'leaflet.minicharts', 'lubridate', 'magrittr', 'mongolite', 'plotly', 'quanteda', 'reactable', 'readxl', 'rhino', 'rjson', 'Rmpfr', 'servr', 'sf', 'shiny', 'shiny.fluent', 'shiny.router', 'shinyjs', 'shinymanager', 'shinythemes', 'slam', 'stats', 'stringr', 'tidyr', 'tm', 'topicmodels', 'upstartr', 'wordcloud', 'wordcloud2'))"

# Make a directory in the container
WORKDIR /app

# Copy your files into the container
COPY . /app

# Installation de libglpk40 et libsecret-1-0
RUN apt-get update && apt-get install -y libglpk40 libsecret-1-0

# Installation des dépendances système pour les packages R
RUN apt-get update && apt-get install -y libudunits2-dev libproj-dev libgdal-dev libgeos-dev libgsl-dev

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 8180)"]
