FROM rocker/r-ver:4.3.1

# Use PPM for binary installs
ENV RENV_CONFIG_REPOS_OVERRIDE "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
ENV RENV_PATHS_LIBRARY renv/library

# Install system dependencies
RUN apt-get update && apt-get install --no-install-recommends -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev git \
    libudunits2-dev python3-dev python3-pip libgdal-dev libgeos-dev \
    libproj-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev pandoc

# Install pipenv for Python dependencies
RUN pip install pipenv

# Copy pipenv files into the image. The reason this is a separate step from
# the later step that adds files from the working directory is because we want
# to avoid having to reinstall dependencies every time a file in the directory
# changes, as Docker will bust the cache of every layer following a layer that
# needs to change
COPY Pipfile .
COPY Pipfile.lock .

# Install Python dependencies
RUN pipenv install --system --deploy

# Copy R bootstrap files into the image
COPY renv.lock .
COPY renv/profiles/reporting/renv.lock reporting-renv.lock
COPY .Rprofile .
COPY renv/ renv/

# Install R dependencies
RUN Rscript -e 'renv::restore()'
RUN Rscript -e 'renv::restore(lockfile = "reporting-renv.lock")'

# Copy the directory into the container
ADD ./ model-res-avm/

# Copy R dependencies into the app directory
RUN rm -Rf model-res-avm/renv
RUN mv renv model-res-avm/

# Set the working directory to the app dir
WORKDIR model-res-avm/

CMD dvc pull && dvc repro
