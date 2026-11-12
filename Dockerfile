FROM rstudio/plumber:latest

RUN R -e "install.packages('ranger', repos='https://cloud.r-project.org')"

WORKDIR /app

COPY api.R /app/api.R
COPY data/Melbourne_housing_FULL.csv /app/data/Melbourne_housing_FULL.csv

EXPOSE 8000

CMD ["/app/api.R"]