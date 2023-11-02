FROM rocker/r-ver:4.3.1

RUN apt update && apt install -y curl unzip groff

RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" && \
    unzip awscliv2.zip && \
	./aws/install

CMD Rscript -e "install.packages(c('aws.s3', 'aws.ec2metadata')); library(aws.s3); aws.s3::put_object('/bin/bash', 's3://ccao-model-results-us-east-1/testfile')"
