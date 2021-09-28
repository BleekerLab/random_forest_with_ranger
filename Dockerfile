FROM rocker/tidyverse:3.6.0

LABEL author="m.galland@uva.nl" \
      description="A Docker image for a Random Forest analysis based on Ranger" \
      usage="docker run bleekerlab/randomforest:latest" \
      url="https://github.com/BleekerLab/random_forest_with_ranger/" \
      rversion="3.6.0"


# add R scripts 
WORKDIR /home/
COPY ["./random_forest_master_script.R",  "/home/"]
COPY ["./custom_functions/",  "/home/"]

# R packages. Specific versions are used here
RUN R -e "install.packages('devtools', repos = 'http://mran.revolutionanalytics.com/snapshot/2021-01-01/')"  \
      && R -e "devtools::install_version('ranger', version = '0.12.1')" \
      && R -e "devtools::install_github('hadley/assertthat')" \
      && R -e "devtools::install_version('optparse', version = '1.6.4')" \
      && R -e "devtools::install_version('patchwork', version = '1.0.0')" \
      && R -e "devtools::install_version('rlang', version = '0.4.10')" \
      && R -e "devtools::install_version('gtools', version = '3.8.2')" 

ENTRYPOINT ["Rscript", "/home/random_forest_master_script.R"]