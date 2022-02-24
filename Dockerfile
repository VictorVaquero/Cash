#FROM rocker/r-ver:3.6.3
#RUN apt-get update && apt-get install -y  libicu-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
#RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
#RUN R -e 'install.packages("remotes")'
#RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.5.0")'
#RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
#RUN Rscript -e 'remotes::install_version("cachem",upgrade="never", version = "1.0.5")'
#RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
#RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
#RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.5.2")'
#RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
#RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'
#RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.7")'
#RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.1")'
#RUN Rscript -e 'remotes::install_version("xml2",upgrade="never", version = "1.3.2")'
#RUN Rscript -e 'remotes::install_version("assertthat",upgrade="never", version = "0.2.1")'
#RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.6.0")'
#RUN Rscript -e 'remotes::install_version("memoise",upgrade="never", version = "2.0.0")'
#RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.18")'
#RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "1.4.0")'
#RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.7.10")'
#RUN Rscript -e 'remotes::install_version("forcats",upgrade="never", version = "0.5.1")'
#RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.3")'
#RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.0.3")'
#RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2")'
#RUN mkdir /build_zone
#ADD . /build_zone
#WORKDIR /build_zone
#RUN R -e 'remotes::install_local(upgrade="never")'
#RUN rm -rf /build_zone
#EXPOSE 80
#CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');Cash::run_app()"

FROM ubuntu:focal

ENV RENV_VERSION 0.15.1

# Update all
RUN apt-get update

# Install R
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get install -y r-base=3.6.3-2
#RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site

# Install renv to keep track and install R packages
# TODO: Check https://environments.rstudio.com/docker https://rstudio.github.io/renv/articles/docker.html
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"


# Ready all directories
RUN mkdir -p /srv/shiny-server/Cash
WORKDIR /srv/shiny-server
RUN useradd shiny

WORKDIR /srv/shiny-server/Cash

# Install all R packages
COPY renv.lock renv.lock
RUN apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev
RUN R -e 'renv::restore()'

# Install Rshiny server
RUN apt-get install -y gdebi-core wget
RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.17.973-amd64.deb
RUN gdebi --n shiny-server-1.5.17.973-amd64.deb

# Move shiny app
COPY . .

# Change ownership
RUN chown -R shiny:shiny ..
RUN chmod g+w ..
RUN chmod g+s ..
RUN chown -R shiny:shiny /var/lib/shiny-server

# Run server
EXPOSE 3838
USER shiny
ENTRYPOINT ["/usr/bin/shiny-server", "/etc/shiny-server/shiny-server.conf"]
