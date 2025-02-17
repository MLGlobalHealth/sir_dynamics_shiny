FROM r-base:4.2.2 AS base

RUN Rscript -e 'install.packages(c("shiny","bslib","dplyr","ggplot2","tidyr","odin"), repos="https://cran.r-project.org")'

COPY . .

ENTRYPOINT ["Rscript", "-e"]

FROM base AS complicated
EXPOSE 5000
CMD ["shiny::runApp('complicated_models', host='0.0.0.0', port=5000)"]

FROM base AS simple
EXPOSE 5000
CMD ["shiny::runApp('sir_dynamics', host='0.0.0.0', port=5000)"]


