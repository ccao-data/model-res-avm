# Design doc: Model deployment pipeline

This doc represents a proposal for a simple CI/CD pipeline that we can use to deploy residential models and run experiments on them.

## Background

At a high level, our **existing process** for experimenting with changes to our models looks like this:

* Models run on an on-prem server
* Data scientists trigger model runs by SSHing into the server and running shell commands from cloned copies of this repo
* Model artifacts are saved using DVC
* Data scientists commit corresponding changes to model code after their experiment runs prove successful  

This process has the advantage of being simple, easy to maintain, and cheap; as a result it has been useful to our team during the recent past when we only had a few data scientists on staff and they needed to focus most of their effort on building a new model from the ground up. However, some of its **limitations** are becoming apparent as our team scales up and begins to expand our focus:

* Our on-prem server only has enough resources to run one model at a time, so only one data scientist may be running modeling experiments at a given time
  * Further, our server has no notion of a job queue, so a data scientist who is waiting to run a model must notice that a previous run has completed and initiate their run manually 
* Our on-prem server does not have a GPU, so it can't make use of GPU-accelerated libraries like XGBoost
* Model runs are decoupled from version control changes, so data scientists have to remember to commit their changes correctly
* Results of model runs are not easily accessible to PR reviewers

The design described below aims to remove these limitations while retaining as much simplicity, maintainability, and affordability as possible.

## Requirements

At a high level, a model deployment pipeline should:

* Integrate with our existing cloud infrastructure (GitHub and AWS)
* Trigger model runs from pull request branches
* Require code authors to approve model runs before they are initiated
* Run the model on ephemeral, cheap, and isolated cloud infrastructure
* Run multiple model runs simultaneously on separate hardware
* Report model statistics back to the pull request that triggered a run

## Design

Here is a rough sketch of a new model deployment pipeline:

* Define a new workflow, `run-model.yaml`, that runs on:
  * Every commit to every pull request against the main branch
  * The `workflow_dispatch` event
* Set up the workflow so that it deploys to the `staging` environment and requires [manual approval](https://docs.github.com/en/actions/using-workflows/triggering-a-workflow#using-environments-to-manually-trigger-workflow-jobs)
* Use the [`configure-aws-credentials`](https://github.com/aws-actions/configure-aws-credentials) action to authenticate with AWS
* Run Terraform to make sure an AWS Batch job queue and job definition exist for the PR
* Build and push a new Docker image to ECR
* Use the AWS CLI to [submit a job](https://docs.aws.amazon.com/cli/latest/reference/batch/submit-job.html) to the Batch queue
* Use the AWS CLI to [poll the job status](https://awscli.amazonaws.com/v2/documentation/api/latest/reference/batch/describe-jobs.html) until it has a terminal status (`SUCCEEDED` or `FAILED`)
  * Once the job has at least a `RUNNING` status, use the `jobStreamName` parameter to print a link to its logs
* _TK: How should we format output for pushing back to the PR? Is it enough to push the results doc to S3 and print a link in the workflow output, or do we need an integration that can actually post a comment on the PR, à la Codecov?_

## Tasks

We will create GitHub issues for the following tasks:

* Add Docker image definition for the model
* Add GitHub workflow to deploy and run an AWS Batch job on commits to PRs
* _TK: Output reporting improvements?_
