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
        libproj-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev pandoc && \
    rm -rf /var/lib/apt/lists/*

# Install pipeline Python dependencies globally
RUN pip install aiobotocore[boto3] boto3 dvc[s3]

# Copy R bootstrap files into the image
COPY renv.lock .
COPY .Rprofile .
COPY renv/ renv/

# Install R dependencies
RUN Rscript -e 'renv::restore()'

# Set the working directory to the app dir
WORKDIR /model-res-avm/

# Copy the directory into the container
COPY ./ .

# Copy R dependencies into the app directory
RUN rm -Rf model-res-avm/renv && \
    mv renv model-res-avm/

CMD dvc pull && dvc repro
