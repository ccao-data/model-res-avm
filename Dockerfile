FROM rocker/r-ver:4.3.1

# Set the working directory to root
WORKDIR /

# Use PPM for binary installs
ENV RENV_CONFIG_REPOS_OVERRIDE "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
ENV RENV_PATHS_LIBRARY renv/library

# Install system dependencies
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
        libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev git \
        libudunits2-dev python3-dev python3-pip libgdal-dev libgeos-dev \
        libproj-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev pandoc \
        curl gdebi-core && \
    rm -rf /var/lib/apt/lists/*

RUN curl -o quarto-linux-amd64.deb -L \
    https://github.com/quarto-dev/quarto-cli/releases/download/v1.3.450/quarto-1.3.450-linux-amd64.deb && \
    gdebi -n quarto-linux-amd64.deb

# Install pipeline Python dependencies globally
RUN pip install --no-cache-dir aiobotocore[boto3] boto3 dvc[s3]

# Copy R bootstrap files into the image
COPY renv.lock .
COPY renv/profiles/reporting/renv.lock reporting-renv.lock
COPY .Rprofile .
COPY renv/ renv/

# Install R dependencies
RUN Rscript -e 'renv::restore()'
RUN Rscript -e 'renv::restore(lockfile = "reporting-renv.lock")'

# Set the working directory to the app dir
WORKDIR /model-res-avm/

# Copy the directory into the container
COPY ./ .

# Copy R dependencies into the app directory
RUN rm -Rf model-res-avm/renv && \
    mv renv model-res-avm/

CMD dvc pull && dvc repro
