FROM rocker/r-ver:4.3.1

# Use PPM for binary installs
ENV RENV_CONFIG_REPOS_OVERRIDE "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
ENV RENV_PATHS_LIBRARY renv/library

# Install system dependencies
RUN apt-get update && apt-get install --no-install-recommends -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev git \
    libudunits2-dev python3-dev python3-pip

# Install pipenv for Python dependencies
RUN pip install pipenv

# Install renv for R dependencies
RUN Rscript -e "install.packages('renv')"

# Copy pipenv files into the image. The reason this is a separate step from
# the later step that adds files from the working directory is because we want
# to avoid having to reinstall dependencies every time a file in the directory
# changes, as Docker will bust the cache of every layer following a layer that
# needs to change
COPY Pipfile .
COPY Pipfile.lock .

# Install Python dependencies
RUN pipenv install --system --deploy

# Copy R lockfile into the image
COPY renv.lock .

# Install R dependencies
RUN Rscript -e 'renv::restore()'

# Copy the directory into the container
ADD ./ model-res-avm/
WORKDIR model-res-avm/
