#R base image
FROM rocker/r-ver:latest

MAINTAINER "Gordon Grabert"

# system libraries of general use
## install debian packages
RUN \
    --mount=type=cache,target=/var/cache/apt \
    apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## renv.lock file
COPY renv.lock ./renv.lock
## app folder
COPY ./inst/my_app ./app

# Install libnlopt-dev
RUN apt-get update && apt-get install -y libnlopt-dev
# install renv & restore packages
# RUN Rscript -e 'install.packages("renv")'
# RUN Rscript -e 'renv::restore()'

RUN Rscript -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_github("scibiome/meteor")'

# expose port
EXPOSE 3838

# Set the RSTUDIO_PANDOC environment variable
ENV RSTUDIO_PANDOC=/usr/lib/rstudio/bin/pandoc

# run app on container start
#CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
CMD ["R", "-e", "MeTEor::meteor()"]

ENV IS_IN_CONTAINER="TRUE"

# miniconda
ENV PATH="/root/miniconda3/bin:${PATH}"
ARG PATH="/root/miniconda3/bin:${PATH}"
RUN apt-get update

RUN apt-get install -y wget && rm -rf /var/lib/apt/lists/*

RUN wget \
    https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
    && mkdir /root/.conda \
    && bash Miniconda3-latest-Linux-x86_64.sh -b \
    && rm -f Miniconda3-latest-Linux-x86_64.sh
RUN conda --version

RUN pip3 install pandas==1.5.2 --upgrade
RUN conda install -c conda-forge -y mprod-package
RUN pip3 install numpy==1.23.0 --upgrade
