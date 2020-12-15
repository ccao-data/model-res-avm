Table of Contents
================

  - [Model Overview](#model-overview)
      - [How It Works](#how-it-works)
      - [Features Used](#features-used)
      - [Sales Used](#sales-used)
  - [Choices Made](#choices-made)
  - [Ongoing Issues](#ongoing-issues)
  - [Major Changes From V1](#major-changes-from-v1)
  - [FAQs](#faqs)
  - [Technical Details](#technical-details)
  - [Replication/Usage](#replicationusage)
      - [Installation](#installation)
      - [Usage/Files](#usagefiles)
      - [Troubleshooting](#troubleshooting)
  - [Process of Installing and Running Residental
    Model](#process-of-installing-and-running-residental-model)
      - [Steps of Installation and
        Running](#steps-of-installation-and-running)
          - [Step 1: Clone the repo, Setting Repo as working directory
            in
            RStudio](#step-1-clone-the-repo-setting-repo-as-working-directory-in-rstudio)
          - [Step 2: In RStuio, Install Package Renv and Run
            renv::restore()](#step-2-in-rstuio-install-package-renv-and-run-renvrestore)
          - [Step 3: Ask for Input data and hyperparameter files from
            Dan](#step-3-ask-for-input-data-and-hyperparameter-files-from-dan)
          - [Step 4: Run Model.R from ccao\_res\_avm repo (creates the
            model and evaluates its performance on a test
            set)](#step-4-run-model.r-from-ccao_res_avm-repo-creates-the-model-and-evaluates-its-performance-on-a-test-set)
          - [Step 5: Run Valuation.R (gets the predicted values for all
            the properties we need to
            assess)](#step-5-run-valuation.r-gets-the-predicted-values-for-all-the-properties-we-need-to-assess)
          - [Step 6: Run reports: model\_report.Rmd &
            Valuation\_report.Rmd for model performance
            evaluation.](#step-6-run-reports-model_report.rmd-valuation_report.rmd-for-model-performance-evaluation.)
      - [Common Bugs:](#common-bugs)
          - [Step 2: Install Package Renv and Run
            renv::restore()](#step-2-install-package-renv-and-run-renvrestore)
          - [Step 4: Run Model.R from ccao\_res\_avm
            repo](#step-4-run-model.r-from-ccao_res_avm-repo)

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Model Overview

The duty of the Cook County Assessor’s Office is to value property in a
fair, accurate, and transparent way. The Assessor is committed to
transparency throughout the assessment process, and as such, this
repository contains:

  - [Nearly all code used to determine Cook County residential property
    values](./)
  - [Rationale for different modeling, feature, and code decisions that
    affect assessed property values](#choices-made)
  - [An outline of ongoing data quality issues that affect assessed
    property values](#ongoing-issues)
  - [Instructions to replicate our model and results using open
    data](#installation)

The repository itself contains code for the Computer Assisted Mass
Appraisal (CAMA) system used to generate initial assessed values for all
single-family and multi-family residential properties in Cook County.
This system is effectively an advanced statistical/machine learning
model (hereafter referred to as “the model”) which uses previous sales
to generate predicted values (assessments) for unsold properties.

Note that data extraction/preparation, feature engineering, and data
validation for this model are handled in a [separate
repository](https://gitlab.com/ccao-data-science---modeling/processes/etl_res_data).
Values for [residential
condominiums](https://gitlab.com/ccao-data-science---modeling/models/ccao_condo_avm)
and [commercial
apartments](https://gitlab.com/ccao-data-science---modeling/models/commercial-apartments-automated-valuation-model)
are determined by separate models.

### How It Works

The goal of the model is to answer the question “What would the sale
price of every home be if it had sold last year?” To answer this
question, we use a two-step process:

1.  **Modeling**: First, we use the code in this repository to train an
    advanced machine learning model. The model predicts the sale price
    (fair market value) of unsold properties using the known sale price
    of similar and nearby properties. Training the model involves
    iteratively updating a mathematical function to recognize patterns
    in the data. The output of this step is a model object which can be
    used to predict any property’s sale price given a [set of
    characteristics (such as location, number of bedrooms,
    etc.)](#features-used).

2.  **Valuation**: Second, we use the model created in step one to
    predict values for all residential properties in Cook County. We
    then train a secondary, much simpler model (which we call the
    post-modeling adjustment model) to correct for any systemic bias
    introduced by the first model. Finally, we combine the first and
    second model to produce initial assessed property values - the ones
    printed on Residential Reassessment Notices that are mailed when
    properties are reassessed. However, note that values produced by the
    model and mailed values may not be identical, as there are
    additional rounds of automated and human review between modeling and
    mailing.

The full residential modeling pipeline, from raw data to mailing
assessed values to taxpayers, looks something like:

``` mermaid
graph LR
    as400[("AS-400/<br>Mainframe")]
    mirror("Data mirrored from<br>County mainframe")
    ext_data(("External data<br>(Census data,<br>geospatial)"))
    etl_pinlocations("Ext. data joined<br>to property data")
    sql[("SQL<br>database")]
    etl_res_data("Data extracted<br>and cleaned")
    data_train["Training<br>(sales) data"]
    data_ass["All property<br>data"]

    model_step1("Model training<br>(Step 1 above)")
    model_step2("Valuation<br>(Step 2 above)")
    model{"Trained<br>Model"}
    final_vals["Final predicted/<br>assessed values"]
    desk_review("Hand review<br>and correction")

    click etl_res_data "https://gitlab.com/ccao-data-science---modeling/processes/etl_res_data"
    click etl_pinlocations "https://gitlab.com/ccao-data-science---modeling/processes/etl_pinlocations"

    classDef Link fill:#90a9e8;
    class etl_res_data Link;
    class etl_pinlocations Link;

    as400 --> mirror
    mirror --> sql
    ext_data --> etl_pinlocations
    etl_pinlocations --> sql
    sql --> etl_res_data
    etl_res_data --> data_train
    etl_res_data --> data_ass
    data_train --> model_step1
    model_step1 --> model
    model --> model_step2
    data_ass --> model_step2
    model_step2 --> final_vals
    final_vals --> desk_review
    desk_review --> as400
```

### Features Used

The residential model uses a variety of individual and aggregate
features to determine a property’s assessed value. We’ve tested a long
list of possible features over time, including [walk
score](https://gitlab.com/ccao-data-science---modeling/models/ccao_res_avm/-/blob/9407d1fae1986c5ce1f5434aa91d3f8cf06c8ea1/output/test_new_variables/county_walkscore.html),
[crime
rate](https://gitlab.com/ccao-data-science---modeling/models/ccao_res_avm/-/blob/9407d1fae1986c5ce1f5434aa91d3f8cf06c8ea1/output/test_new_variables/chicago_crimerate.html),
[school
districts](https://gitlab.com/ccao-data-science---modeling/models/ccao_res_avm/-/blob/9407d1fae1986c5ce1f5434aa91d3f8cf06c8ea1/output/test_new_variables/county_school_boundaries_mean_encoded.html),
and many others. The features in the table below are the ones that made
the cut. They’re the right combination of easy to understand and impute,
powerfully predictive, and well-behaved. Most of them are in use in the
model as of 2020-12-15.

| Feature Name                               | Category       | Type        | Possible Values                                                                                 |
| :----------------------------------------- | :------------- | :---------- | :---------------------------------------------------------------------------------------------- |
| Age                                        | Characteristic | numeric     |                                                                                                 |
| Central Air Conditioning                   | Characteristic | categorical | Central A/C, No Central A/C                                                                     |
| Apartments                                 | Characteristic | categorical | Two, Three, Four, Five, Six, None                                                               |
| Attic Finish                               | Characteristic | categorical | Living Area, Partial, None                                                                      |
| Attic Type                                 | Characteristic | categorical | Full, Partial, None                                                                             |
| Bedrooms                                   | Characteristic | numeric     |                                                                                                 |
| Building Square Feet                       | Characteristic | numeric     |                                                                                                 |
| Basement                                   | Characteristic | categorical | Full, Slab, Partial, Crawl                                                                      |
| Basement Finish                            | Characteristic | categorical | Formal Rec Room, Apartment, Unfinished                                                          |
| Wall Material                              | Characteristic | categorical | Frame, Masonry, Frame + Masonry, Stucco                                                         |
| Full Baths                                 | Characteristic | numeric     |                                                                                                 |
| Fireplaces                                 | Characteristic | numeric     |                                                                                                 |
| Garage 1 Area                              | Characteristic | categorical | Yes, No                                                                                         |
| Garage 1 Attached                          | Characteristic | categorical | Yes, No                                                                                         |
| Garage 1 Material                          | Characteristic | categorical | Frame, Masonry, Frame + Masonry, Stucco                                                         |
| Garage 1 Size                              | Characteristic | categorical | 1 cars, 1.5 cars, 2 cars, 2.5 cars, 3 cars, 3.5 cars, 0 cars, 4 cars                            |
| Half Baths                                 | Characteristic | numeric     |                                                                                                 |
| Land Square Feet                           | Characteristic | numeric     |                                                                                                 |
| Central Heating                            | Characteristic | categorical | Warm Air Furnace, Hot Water Steam, Electric Heater, None                                        |
| Number of Commercial Units                 | Characteristic | numeric     |                                                                                                 |
| Other Heating                              | Characteristic | categorical | Floor Furnace, Unit Heater, Stove, Solar, None                                                  |
| Porch                                      | Characteristic | categorical | Frame Enclosed, Masonry Enclosed, None                                                          |
| Roof Material                              | Characteristic | categorical | Shingle + Asphalt, Tar + Gravel, Slate, Shake, Tile, Other                                      |
| Rooms                                      | Characteristic | numeric     |                                                                                                 |
| Cathedral Ceiling                          | Characteristic | categorical | Yes, No                                                                                         |
| Design Plan                                | Characteristic | categorical | Architect, Stock Plan                                                                           |
| Type of Residence                          | Characteristic | categorical | 1 Story, 2 Story, 3 Story +, Split Level, 1.5 Story, 1.6 Story, 1.7 Story, 1.8 Story, 1.9 Story |
| Use                                        | Characteristic | categorical | Single-Family, Multi-Family                                                                     |
| Median Income                              | Economic       | numeric     |                                                                                                 |
| Tax Rate                                   | Economic       | numeric     |                                                                                                 |
| Number of Foreclosures in Town Per Month   | Economic       | numeric     |                                                                                                 |
| Number of Foreclosures in Town Per Quarter | Economic       | numeric     |                                                                                                 |
| FEMA Floodplain                            | Geospatial     | logical     |                                                                                                 |
| O’Hare Noise                               | Geospatial     | logical     |                                                                                                 |
| Road Proximity \< 100 Feet                 | Geospatial     | logical     |                                                                                                 |
| Road Proximity 101 - 300 Feet              | Geospatial     | logical     |                                                                                                 |
| Flood Risk Direction                       | Geospatial     | numeric     |                                                                                                 |
| Flood Risk Factor                          | Geospatial     | numeric     |                                                                                                 |
| Elementary/Middle School                   | Geospatial     | character   |                                                                                                 |
| High School                                | Geospatial     | character   |                                                                                                 |
| Large Home                                 | Indicator      | logical     |                                                                                                 |
| Large Lot                                  | Indicator      | logical     |                                                                                                 |
| Garage Indicator                           | Indicator      | logical     |                                                                                                 |
| Neighborhood Code                          | Meta           | character   |                                                                                                 |
| Township Code                              | Meta           | character   |                                                                                                 |
| Sale Year                                  | Time           | numeric     |                                                                                                 |
| Sale Quarter                               | Time           | numeric     |                                                                                                 |
| Sale Month of Year                         | Time           | numeric     |                                                                                                 |
| Sale Quarter of Year                       | Time           | numeric     |                                                                                                 |

### Sales Used

## Choices Made

  - Model type
  - Features to include/exclude
  - How to trim the sales sample
  - post-modeling adjustments

## Ongoing Issues

  - Data integrity (wrong characteristics, bad sales, lack of chars)
  - Low-value properties
  - Multi-codes
  - Multi-family
  - Land valuation

## Major Changes From V1

  - Whole county
  - lightgbm
  - tidymodels
  - split codebase
  - dependency management

## FAQs

# Technical Details

Modeling is implemented using the
[Tidymodels](https://www.tidymodels.org/) framework for R.

LightGBM + glmnet (elasticnet)

lgbm integrated with treesnip + custom code

preprocessing in recipes

Tuned according to lgbm docs

Minimized on RMSE

Other models tried

# Replication/Usage

## Installation

## Usage/Files

## Troubleshooting

# Process of Installing and Running Residental Model

-----

## Steps of Installation and Running

#### Step 1: Clone the repo, Setting Repo as working directory in RStudio

#### Step 2: In RStuio, Install Package Renv and Run renv::restore()

#### Step 3: Ask for Input data and hyperparameter files from Dan

Params file goes output/params folder: \* cat\_params.rds, \*
xgb\_params.rds, \* lgbm\_params.rds, \* Model\_timings.rds

Input files goes to input folder: \* nbhdstats.parquet, \*
assmntdata.parquet, \* modeldata.parquet

#### Step 4: Run Model.R from ccao\_res\_avm repo (creates the model and evaluates its performance on a test set)

#### Step 5: Run Valuation.R (gets the predicted values for all the properties we need to assess)

#### Step 6: Run reports: model\_report.Rmd & Valuation\_report.Rmd for model performance evaluation.

## Common Bugs:

#### Step 2: Install Package Renv and Run renv::restore()

Potential Problesm Encountered:

1)  Error: Failed to retrieve package ‘treesnip’ Solution:

<!-- end list -->

  - Manullay install treesnip with instruction:
    <https://github.com/curso-r/treesnip>

<!-- end list -->

2)  Error: WARNING: Rtools is required to build R packages, but is not
    currently installed.

Solution: - Install R Tools:
<https://cran.r-project.org/bin/windows/Rtools/>

3)  Error: (converted from warning) package ‘parsnip’ was built under R
    version 4.0.3

Solution: - try manually installing the dev version of parsnip from
github: <https://github.com/tidymodels/parsnip>

#### Step 4: Run Model.R from ccao\_res\_avm repo
