#!/bin/bash
CRAN_REPO="https://cloud.r-project.org/"
# Rscript -e "install.packages(c('dplyr', 'lintr', 'shiny', 'shinycssloaders', 'pracma', 'fractaldim', 'openxlsx', 'shinyjs', 'Benchmarking', 'shinydashboard', 'shinyalert', 'DT', 'TSA', 'longmemo', 'plyr', 'shinyFiles', 'tools', 'htmltools', 'markdown'), repos='$CRAN_REPO')"
Rscript -e "install.packages('./libraries/ptsuite_1.0.0.tar.gz', repos = NULL, type = 'source')"
echo "Instalação concluída."