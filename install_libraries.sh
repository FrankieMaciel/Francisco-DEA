#!/bin/bash
CRAN_REPO="https://cloud.r-project.org/"
Rscript -e "install.packages(c('lintr', 'shiny', 'shinycssloaders', 'pracma', 'fractaldim', 'openxlsx', 'shinyjs', 'Benchmarking', 'shinydashboard', 'shinyalert', 'DT', 'TSA', 'longmemo', 'plyr', 'shinyFiles', 'tools', 'htmltools', 'markdown'), repos='$CRAN_REPO')"
Rscript -e "install.packages('/path/to/ptsuite.tar.gz', repos = NULL, type = 'source')"
echo "Instalação concluída."