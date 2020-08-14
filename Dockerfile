# TEMPLATE DOCKERFILE FOR CCAO ETL PROCESSES

### SETUP ###

# Use the R image as a base
# Currently locked at 3.6.1 because anything above 3.6.1 uses a new version
# of openssl that breaks SQL server connections
FROM rocker/r-ver:3.6.1

# Arguments that get passed to apt to install linux dependencies. Formatted as
# strings with libraries separated by a space
ARG APT_DEPS="libcurl4-gnutls-dev libssl-dev tdsodbc unixodbc unixodbc-dev wget libxml2-dev"

# Create and set the default working dir for our process. This is where
# code will live and execute from
WORKDIR /tmp

# Set the command that runs at startup. This tells our app to run app.R
# when the container is launched
CMD ["Rscript", "--no-init-file", "--no-environ", "run.R"]


### DEPENDENCIES ###

# Install R linux dependencies and utilities. These are needed by some common
# R libraries as backends. You may have to add to this list if certain packages
# require linux libraries (for example, the R package sf requires libgdal-dev)
RUN apt-get update && apt-get install --no-install-recommends -y \
    $(echo $APT_DEPS) \
    && apt-get clean && apt-get autoremove --purge -y \
    && rm -rf /var/lib/apt/lists/* /tmp/*

# Install ODBC drivers for MS SQL. These are used to establish a connection
# to CCAO's existing SQL server
RUN wget --no-verbose https://packages.microsoft.com/debian/9/prod/pool/main/m/msodbcsql17/msodbcsql17_17.4.2.1-1_amd64.deb -O /tmp/msodbc.deb \
    && ACCEPT_EULA=Y apt-get install /tmp/msodbc.deb


### R PACKAGE INSTALL ###

# Copy ONLY the files needed to install dependencies, since these are unlikely
# to change. Copying all of our files would bust the cache if we had updated
# any of our code. Cache will bust when renv.lock (the package manifest) is
# altered, meaning adding new packages requires a full cache rebuild
COPY .Rprofile renv.lock /tmp/
COPY renv/activate.R /tmp/renv/activate.R

# Install R packages. The renv package is used to version lock to specific R packages
# renv.lock contains a list of all packages needed to run the application,
# and it will install all these packages by running the command renv::restore()
RUN Rscript -e 'renv::settings$use.cache(FALSE); renv::restore()'


### COPY CODE ###

# Copy configuration files from our repo to their expected locations. These
# files point ODBC to the correct drivers 
COPY config/odbcinst.ini /etc/

# Copy all files and subdirectories from our repository into the working dir
# This command ignores the files listed in .dockerignore
COPY . /tmp/


### LABELLING ###

# Build arguments used to label the container, these variables are predefined
# as part of GitLab. They get passed to the container as build-args in the
# .gitlab-ci.yml file. These arguments only exist when building the container
ARG VCS_NAME
ARG VCS_URL
ARG VCS_REF
ARG VCS_REF_SHORT
ARG VCS_VER
ARG VCS_ID
ARG VCS_NAMESPACE

# Environmental variables that are passed to the container. These variables
# exist inside each app and can be called from R. 
ENV VCS_NAME=$VCS_NAME
ENV VCS_URL=$VCS_URL
ENV VCS_REF=$VCS_REF
ENV VCS_REF_SHORT=$VCS_REF_SHORT
ENV VCS_VER=$VCS_VER
ENV VCS_ID=$VCS_ID
ENV VCS_NAMESPACE=$VCS_NAMESPACE

# Create labels for the container. These are standardized labels defined by
# opencontainers.org specs
LABEL maintainer "Dan Snow <dsnow@cookcountyassessor.com>"
LABEL com.centurylinklabs.watchtower.enable="true"
LABEL org.opencontainers.image.title=$VCS_NAME
LABEL org.opencontainers.image.source=$VCS_URL
LABEL org.opencontainers.image.revision=$VCS_REF
LABEL org.opencontainers.image.version=$VCS_VER
