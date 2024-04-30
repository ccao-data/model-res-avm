FROM pytorch/pytorch:2.3.0-cuda12.1-cudnn8-runtime

# Install system dependencies
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
        libx11-dev

# Copy Python requirements file into the image
COPY requirements.txt ./

# Install Python requirements
RUN pip install -U -r requirements.txt

# Set the working directory to the model directory
WORKDIR /model-res-avm/

# Copy the directory into the container
COPY ./ .

# Run comps algorithm
CMD python3 python/comps.py
