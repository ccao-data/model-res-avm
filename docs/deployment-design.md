# Design doc: Model deployment pipeline

This doc represents a proposal for a simple CI/CD pipeline that we can use to deploy residential models and run experiments on them.

## Background

At a high level, our **existing process** for experimenting with changes to our models looks like this:

* Models run on an on-prem server
* Data scientists trigger model runs by SSHing into the server and running R scripts from cloned copies of this repo
* Model inputs and parameters are hashed for reproducibility using DVC for [data versioning](https://dvc.org/doc/start/data-management/data-versioning)
* R scripts are run in the correct sequence using DBC for [data pipelines](https://dvc.org/doc/start/data-management/data-pipelines)
* Data scientists commit corresponding changes to model code after their experiment runs prove successful  

This process has the advantage of being simple, easy to maintain, and cheap; as a result it has been useful to our team during the recent past when we only had a few data scientists on staff and they needed to focus most of their effort on building a new model from the ground up. However, some of its **limitations** are becoming apparent as our team scales up and begins to expand our focus:

* Our on-prem server only has enough resources to run one model at a time, so only one data scientist may be running modeling experiments at a given time
  * Further, our server has no notion of a job queue, so a data scientist who is waiting to run a model must notice that a previous run has completed and initiate their run manually 
* Our on-prem server does not have a GPU, so it can't make use of GPU-accelerated libraries like XGBoost
* Model runs are decoupled from changes to code and data in version control, so data scientists have to remember to commit their changes correctly
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

### Running the model

Here is a rough sketch of a new model deployment pipeline:

* Define a new workflow, `run-model.yaml`, that runs on:
  * Every commit to every pull request
  * The `workflow_dispatch` event
* Set up the workflow so that it deploys to the `staging` environment and requires [manual approval](https://docs.github.com/en/actions/using-workflows/triggering-a-workflow#using-environments-to-manually-trigger-workflow-jobs)
* Define a job, `build-docker-image`, to build and push a new Docker image for the model code to GitHub Container Registry
  * Cache the build using `renv.lock` as the key
* Define a job to run the model (implementation details in the following sections)
 
See the following sections for two options for how we can run the model itself.

#### Option 1: Use CML self-hosted runners

CML's [self-hosted runners](https://cml.dev/doc/self-hosted-runners#allocating-cloud-compute-resources-with-cml) claim to provide an abstraction layer on top of GitHub Actions and AWS EC2 that would allow us to launch a spot EC2 instance and use it as a GitHub Actions runner. If it works as advertised and is easy to debug, it would allow us to spin up infrastructure for running the model with very little custom code.

The steps involved here include:

* Define a job, `launch-runner`, to start an AWS spot EC2 instance using [`cml runner`](https://cml.dev/doc/self-hosted-runners#allocating-cloud-compute-resources-with-cml)
  * Set sensible defaults for the [instance options](https://cml.dev/doc/ref/runner#options), but allow them to be overridden via workflow inputs
* Define a job, `run-model`, to run the model on the EC2 instance created by CML
  * Set the `runs-on` key for the job to point at the runner
    * This will cause steps defined in the job to run on the remote runner
  * Run the model using `dvc pull` and `dvc repro`

#### Option 2: Write custom code to run model jobs on AWS Batch

If CML does not work as advertised, we can always implement our own version of its functionality. Define the following steps in a `run-model` job:

* Run Terraform to make sure an AWS Batch job queue and job definition exist for the PR
   * The job definition should define the code that will be used to run the model itself, e.g. `dvc pull` and `dvc repro`
* Use the AWS CLI to [submit a job](https://docs.aws.amazon.com/cli/latest/reference/batch/submit-job.html) to the Batch queue
* Use the AWS CLI to [poll the job status](https://awscli.amazonaws.com/v2/documentation/api/latest/reference/batch/describe-jobs.html) until it has a terminal status (`SUCCEEDED` or `FAILED`)
  * Once the job has at least a `RUNNING` status, use the `jobStreamName` parameter to print a link to its logs
 
### Reporting model performance

We would like to move toward the following pattern for evaluating model performance:

1. Use an Quarto doc stored in the repo to analyze/diagnose/display single model performance. This will be created for each model run and will be sent as a link in the SNS notification at the end of a run.
2. Use Tableau for cross-model comparison, using the same dashboards as previous years.

Step 1 will require us to update `05-finalize.R` to generate the Quarto doc, upload it to S3, and adjust the SNS message body to include a link to it.

### Caching intermediate data

Caching intermediate data would allow us to only run model stages whose code, data, or dependencies have changed since the last model run. This has the potential to reduce the amount of compute we use and speed up experimentation.

Remote caching is currently [not natively supported by DVC](https://github.com/iterative/dvc/issues/5665#issuecomment-811087810), but it is theoreitcally possible if we were to pull the [cache directory](https://dvc.org/doc/user-guide/project-structure/internal-files#structure-of-the-cache-directory) from S3 storage before each run and push it after successful runs. However, this pattern would also require updating the `dvc.lock` file on every run, so it's likely too fragile to be worth implementing right now.

### Metrics and experiments

While DVC now offers built-in tools for [metrics](https://dvc.org/doc/start/data-management/metrics-parameters-plots) and [experiments](https://dvc.org/doc/start/experiments), these features do not yet seem to offer any functionality above and beyond what we have already built into the modeling scripts, so we should wait to switch to them until we can identify a limitation of our current scripts that would be resolved faster by switching to DVC for metrics or experimentation.  

## Tasks

We will create GitHub issues for the following tasks:

* Add Docker image definition for the model
* Add GitHub workflow to deploy and run the model on commits to PRs
   * Note that there is a prototype of a similar workflow [on GitLab](https://gitlab.com/ccao-data-science---modeling/models/ccao_res_avm/-/blob/master/.gitlab-ci.yml?ref_type=heads) that may be useful
   * Spike the CML self-hosted runner solution first, and move on to the custom solution if CML runners don't work as advertised
* Update the Finalize step to generate a Quarto doc and link to it in SNS notification
