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
    libssl-dev \
    libglpk40

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean && apt-get install -y wget && \
    rm -rf /var/lib/apt/lists/*

# Install libnlopt-dev
RUN apt-get update && apt-get install -y libnlopt-dev


# Install pandoc

# Download and install Pandoc
RUN wget https://github.com/jgm/pandoc/releases/download/2.17.1/pandoc-2.17.1-linux-amd64.tar.gz && \
    tar xvzf pandoc-2.17.1-linux-amd64.tar.gz --strip-components 1 -C /usr/local && \
    rm pandoc-2.17.1-linux-amd64.tar.gz

# Install MeTEor

RUN Rscript -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_github("scibiome/meteor")'

# expose port
EXPOSE 3838

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
# RUN pip3 install numpy==1.23.0 --upgrade

# run app on container start
CMD ["Rscript", "-e", "MeTEor::meteor()"]
