# Design doc: Model deployment pipeline

This doc represents a proposal for a CI/CD pipeline for running our residential model.

## Requirements

At a high level, a model deployment pipeline must:

* Integrate with our cloud tools (GitHub and AWS)
* Trigger model runs from pull request branches
* Run the model on ephemeral, cheap, and isolated cloud infrastructure
* Report model statistics back to the pull request branch that triggered a run

## Design

Adapted from: https://aws.amazon.com/blogs/opensource/github-actions-aws-fargate

* Define a new workflow, `run-model.yaml`, that runs on:
  * Every commit to every pull request against the main branch
  * `workflow_dispatch`
* Set up the workflow so that it deploys to the `staging` environment and requires [manual approval](https://docs.github.com/en/actions/using-workflows/triggering-a-workflow#using-environments-to-manually-trigger-workflow-jobs)
* Use the [`configure-aws-credentials`](https://github.com/aws-actions/configure-aws-credentials) action to authenticate with AWS
* Build and push a new docker image to ECR
* Run Terraform to make sure an AWS Batch job queue and job definition exist for the PR
* Use the AWS CLI to [submit a job](https://docs.aws.amazon.com/cli/latest/reference/batch/submit-job.html) to the Batch queue
* Use the AWS CLI to [poll the job status](https://awscli.amazonaws.com/v2/documentation/api/latest/reference/batch/describe-jobs.html) until it has a terminal status (`SUCCEEDED` or `FAILED`)
  * Once the job has at least a `RUNNING` status, use the `jobStreamName` parameter to print a link to its logs
* TK: Output?

## Tasks

* Add container image definition for the model
* Add GitHub workflow to deploy and run an AWS Batch job on commits to PRs
* TK: Show better output?
