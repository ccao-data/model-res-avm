FROM rocker/r-ver:4.6.1

# Set the working directory to setup. Uses a dedicated directory instead of
# root since otherwise renv will try to scan every subdirectory
WORKDIR /setup

# Use PPM for binary installs
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/jammy/latest"
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE
ENV RENV_PATHS_LIBRARY=renv/library
ENV RENV_PATHS_CACHE=/setup/cache

# Install some system dependencies using the official rocker scripts pre-built
# into the base image
RUN /rocker_scripts/install_python.sh 3.14
RUN /rocker_scripts/install_quarto.sh 1.6.39
RUN /rocker_scripts/install_pandoc.sh 3.1.3
RUN /rocker_scripts/install_geospatial.sh
RUN /rocker_scripts/install_tidyverse.sh

# Install additional system dependencies
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
        git \
        curl \
        gdebi-core \
        libglpk40 \
        cmake && \
    rm -rf /var/lib/apt/lists/*

# Install pipeline Python dependencies globally.
# Allow breaking system packages to support global installation
RUN pip install --no-cache-dir --break-system-packages dvc[s3]

# Copy R bootstrap files into the image
COPY renv.lock .Rprofile DESCRIPTION ./
COPY renv/profiles/reporting/renv.lock reporting-renv.lock
COPY renv/profiles/dev/renv.lock dev-renv.lock
COPY renv/ renv/

# Install stringi from source because its binary is linked to an incompatible
# version of ICU. Mounting a secret GitHub token is helpful to prevent rate
# limiting from the GitHub API, since our renv lockfiles contain GitHub sources
# and renv pings them on every call
RUN --mount=type=secret,id=github_token,env=GITHUB_PAT \
    Rscript -e 'renv::install("stringi@1.8.7", type = "source", repos = c(CRAN = "https://cloud.r-project.org"))'

# Install sf from source because its binary is linked to an incompatible
# version of PROJ
RUN --mount=type=secret,id=github_token,env=GITHUB_PAT \
    Rscript -e 'renv::install("sf@1.1-2", type = "source", repos = c(CRAN = "https://cloud.r-project.org"))'

# Restore R dependencies from lockfiles
RUN --mount=type=secret,id=github_token,env=GITHUB_PAT Rscript -e 'renv::restore()'
RUN --mount=type=secret,id=github_token,env=GITHUB_PAT Rscript -e 'renv::restore(lockfile = "reporting-renv.lock")'
RUN --mount=type=secret,id=github_pat,env=GITHUB_PAT Rscript -e 'renv::restore(lockfile = "dev-renv.lock")'

# Set the working directory to the model directory
WORKDIR /model-res-avm/

# Copy the directory into the container
COPY ./ .

# Copy R dependencies into the model directory
RUN rm -Rf /model-res-avm/renv && \
    mv /setup/renv /model-res-avm/renv

CMD ["sh", "-c", "dvc pull && dvc repro"]
