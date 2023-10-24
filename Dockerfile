FROM rocker/r-ver:4.3.1

ENV RENV_CONFIG_REPOS_OVERRIDE "https://cloud.r-project.org/"

# Configure renv and pip for caching
ENV RENV_PATHS_CACHE /root/.cache/R
ENV RENV_PATHS_LIBRARY renv/library
ENV PIPENV_CACHE_DIR /root/.cache/pip

# Install system dependencies
RUN apt-get update && apt-get install --no-install-recommends -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev git \
    libudunits2-dev python3-dev python3-pip

# Install pipenv for Python dependencies
RUN pip install pipenv

# Install renv for R dependencies
RUN Rscript -e "install.packages('renv')"

# Copy pipenv files into the container. The reason this is a separate step from
# the later step that adds files from the working directory is because we want
# to avoid having to reinstall dependencies every time a file in the directory
# changes, as Docker will bust the cache of every layer following a layer that
# needs to change
COPY Pipfile .
COPY Pipfile.lock .

# Install Python dependencies
RUN --mount=type=cache,target=/root/.cache/pip,from=pip_cache pipenv install --deploy --system

# Copy R lockfile
COPY renv.lock .

# Install R dependencies
RUN --mount=type=cache,target=/root/.cache/R,from=r_cache Rscript -e 'renv::restore()'

# Copy the directory into the container
ADD ./ model-res-avm/
WORKDIR model-res-avm/
