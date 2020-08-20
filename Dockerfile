FROM rocker/tidyverse:3.6.0

LABEL author="m.galland@uva.nl" \
      description="A Docker image for a Random Forest analysis based on MUVR" \
      usage="docker run bleekerlab/randomforest:latest" \
      url="https://github.com/BleekerLab/random_forest_with_muvr" \
      rversion="3.6.0"


# R packages. 
RUN R -e "install.packages('doParallel', version = '1.0.14')" \
      && R -e "install.packages('optparse')" \
      && R -e "devtools::install_git('https://gitlab.com/CarlBrunius/MUVR.git')" 

# add R scripts 
WORKDIR /home/
COPY ["./random_forest_with_muvr.R",  "/home/"]

ENTRYPOINT ["Rscript", "/home/random_forest_with_muvr.R"]


