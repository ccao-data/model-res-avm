FROM rocker/r-ver:4.4.1

# Set the working directory to setup. Uses a dedicated directory instead of
# root since otherwise renv will try to scan every subdirectory
WORKDIR /setup

# Use PPM for binary installs
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/jammy/latest"
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE
ENV RENV_PATHS_LIBRARY=renv/library
ENV RENV_PATHS_CACHE=/setup/cache

# Install system dependencies
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
        libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev git \
        libudunits2-dev python3-dev python3-pip python3-venv libgdal-dev \
        libgeos-dev libproj-dev libfontconfig1-dev libharfbuzz-dev \
        libfribidi-dev pandoc curl gdebi-core \
        libglpk-dev libglpk40 && \
    rm -rf /var/lib/apt/lists/*

# Install Quarto
RUN curl -o quarto-linux-amd64.deb -L \
    https://github.com/quarto-dev/quarto-cli/releases/download/v1.6.39/quarto-1.6.39-linux-amd64.deb
RUN gdebi -n quarto-linux-amd64.deb

# Install pipeline Python dependencies globally
RUN pip install --no-cache-dir dvc[s3]

# Copy R bootstrap files into the image
COPY renv.lock .Rprofile DESCRIPTION requirements.txt ./
COPY renv/profiles/reporting/renv.lock reporting-renv.lock
COPY renv/profiles/dev/renv.lock dev-renv.lock
COPY renv/ renv/

# Install R dependencies. Restoring renv first ensures that it's
# using the same version as recorded in the lockfile
RUN Rscript -e 'renv::restore(packages = "renv"); renv::restore()'
RUN Rscript -e 'renv::restore(lockfile = "reporting-renv.lock")'
RUN Rscript -e 'renv::restore(lockfile = "dev-renv.lock")'

# Set the working directory to the model directory
WORKDIR /model-res-avm/

# Copy the directory into the container
COPY ./ .

# Copy R dependencies into the model directory
RUN rm -Rf /model-res-avm/renv && \
    mv /setup/renv /model-res-avm/renv

CMD ["sh", "-c", "dvc pull && dvc repro"]
