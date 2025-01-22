Table of Contents
================

- [Prior Models](#prior-models)
- [Model Overview](#model-overview)
  - [How It Works](#how-it-works)
  - [Choices Made](#choices-made)
    - [Model Selection](#model-selection)
    - [Framework Selection](#framework-selection)
    - [Hyperparameter Selection](#hyperparameter-selection)
    - [Features Used](#features-used)
    - [Data Used](#data-used)
    - [Post-Modeling](#post-modeling)
  - [Major Changes from Previous
    Versions](#major-changes-from-previous-versions)
    - [`assessment-year-2021`](#assessment-year-2021)
    - [`assessment-year-2022`](#assessment-year-2022)
    - [`assessment-year-2023`](#assessment-year-2023)
    - [`assessment-year-2024`](#assessment-year-2024)
- [Ongoing Issues](#ongoing-issues)
  - [Data Quality and Integrity](#data-quality-and-integrity)
  - [Heterogeneity and Extremes](#heterogeneity-and-extremes)
- [FAQs](#faqs)
- [Usage](#usage)
  - [Running the Model Locally (All
    Users)](#running-the-model-locally-all-users)
    - [Installation](#installation)
    - [Running](#running)
  - [Running the Model on AWS Batch (CCAO Staff
    Only)](#running-the-model-on-aws-batch-ccao-staff-only)
    - [Executing a Run](#executing-a-run)
    - [Deleting Test Runs](#deleting-test-runs)
  - [Parameters](#parameters)
  - [Output](#output)
  - [Getting Data](#getting-data)
  - [System Requirements](#system-requirements)
  - [Managing R Dependencies](#managing-r-dependencies)
    - [Profiles and Lockfiles](#profiles-and-lockfiles)
    - [Using Lockfiles for Local
      Development](#using-lockfiles-for-local-development)
    - [Updating Lockfiles](#updating-lockfiles)
  - [Troubleshooting](#troubleshooting)
- [License](#license)
- [Contributing](#contributing)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Prior Models

This repository contains code, data, and documentation for the Cook
County Assessor’s residential reassessment model. Information about
prior year models can be found at the following links:

| Year(s)     | Triad(s) | Method                                      | Language / Framework       | Link                                                                                               |
|-------------|----------|---------------------------------------------|----------------------------|----------------------------------------------------------------------------------------------------|
| 2009 - 2017 | All      | Linear regression per township              | SPSS                       | [Link](https://gitlab.com/ccao-data-science---modeling/ccao_sf_cama_dev/-/tree/master/code.legacy) |
| 2018        | City     | Linear regression per township              | N/A                        | Not available. Values provided by vendor                                                           |
| 2019        | North    | Linear regression or GBM model per township | R (Base)                   | [Link](https://gitlab.com/ccao-data-science---modeling/ccao_sf_cama_dev)                           |
| 2020        | South    | Linear regression or GBM model per township | R (Base)                   | [Link](https://gitlab.com/ccao-data-science---modeling/ccao_sf_cama_dev)                           |
| 2021        | City     | County-wide LightGBM model                  | R (Tidyverse / Tidymodels) | [Link](https://github.com/ccao-data/model-res-avm/tree/2021-assessment-year)                       |
| 2022        | North    | County-wide LightGBM model                  | R (Tidyverse / Tidymodels) | [Link](https://github.com/ccao-data/model-res-avm/tree/2022-assessment-year)                       |
| 2023        | South    | County-wide LightGBM model                  | R (Tidyverse / Tidymodels) | [Link](https://github.com/ccao-data/model-res-avm/tree/2023-assessment-year)                       |
| 2024        | City     | County-wide LightGBM model                  | R (Tidyverse / Tidymodels) | [Link](https://github.com/ccao-data/model-res-avm/tree/2024-assessment-year)                       |

# Model Overview

The duty of the Cook County Assessor’s Office is to value property in a
fair, accurate, and transparent way. The Assessor is committed to
transparency throughout the assessment process. As such, this document
contains:

- [A brief walkthrough of the overall process used to generate
  residential assessed values](#how-it-works)
- [Rationale for different modeling, feature, and code decisions that
  affect assessed values](#choices-made)
- [An outline of ongoing data quality issues that affect assessed
  values](#ongoing-issues)
- [Instructions to replicate our valuation process and
  results](#installation)

The repository itself contains the [code](./pipeline) for the Automated
Valuation Model (AVM) used to generate initial assessed values for
single- and multi-family residential properties in Cook County. This
system is effectively an advanced machine learning model (hereafter
referred to as “the model”). It uses previous sales to generate
estimated sale values (assessments) for all properties.

## How It Works

The ultimate goal of the model is to answer the question, “What would
the sale price of every Cook County home be if it had sold last year?”

To answer this question, the model estimates the sale price (fair market
value) of unsold properties using the known sale price of similar and
nearby properties. Training the model involves iteratively updating a
mathematical function to recognize patterns in sales data, which
includes both [property characteristics (such as square footage, number
of bedrooms, etc.) and additional factors such as location,
environmental variables (flood risk, noise), and market
trends](#features-used).

The full residential modeling pipeline - from raw data to final values -
consists of 7 stages. Visually, the pipeline looks approximately like
the flowchart below.

``` mermaid
graph LR
    aws[("AWS")]
    ingest("Ingest")
    train("Train")
    assess("Assess")
    evaluate("Evaluate")
    interpret("Interpret")
    finalize("Finalize")
    upload("Upload")
    export("Export")

    ingest --> train
    train --> assess
    train --> interpret
    assess --> evaluate
    evaluate --> finalize
    interpret --> finalize
    finalize --> upload
    finalize --> export
    upload --> aws
    aws --> ingest
    aws --> export
```

All inputs and outputs are stored on AWS S3 using a unique run
identifier. Each stage in the modeling pipeline corresponds to an
individual R script. These scripts can be run independently (as a
stand-alone script) or as part of the overall pipeline (with
[DVC](#using-dvc)) as long as the dependencies for the stage exist.

> :warning: NOTE: For a full technical breakdown of each stage,
> including dependencies, outputs, parameters, and more, see
> [dvc.yaml](./dvc.yaml)

0.  **Ingest**: Pull prepared data from the CCAO’s Athena database. This
    data is divided into [2 primary datasets](#data-used), one for
    training and one for assessment. NOTE: This stage is only run
    as-needed, since the input data does not change for each model run.

1.  **Train**: Train the model using sales data. This involves splitting
    the input data into train/test sets and performing cross-validation
    to determine the optimal set of hyperparameters. The primary output
    of this stage is a trained model object.

2.  **Assess**: Use the trained model to estimate values for all
    residential properties. Values are [adjusted if
    necessary](#post-modeling) and then aggregated to the PIN level. The
    primary output of this stage is a data frame of PIN-level assessed
    values.

3.  **Evaluate**: Measure the performance of the model using the
    held-out test set and an assessor-specific ratio study method.
    Performance statistics include standard machine learning metrics
    (RMSE, MAE, MAPE) as well as assessor-specific metrics (COD, PRD,
    PRB, MKI). This stage calculates metrics for different levels of
    geography with (and without) property class breakouts. The primary
    output of this stage is a data frame of aggregate performance
    statistics.

4.  **Interpret**: Calculate three major explanatory outputs:

    - SHAP values for all the estimated values from the assess stage.
      These are the *per feature* contribution to the predicted value
      for an *individual observation* (usually a single PIN)
    - Aggregate feature importance for the entire model, using the
      built-in LightGBM method
    - An experimental set of comparable property sales, based loosely on
      the method described [in this
      vignette](https://ccao-data.github.io/lightsnip/articles/finding-comps.html)

5.  **Finalize**: Save run timings and metadata. Render the following
    Quarto documents:

    - An overall model report detailing model performance, effects, and
      quality control tests
    - For PINs of interest, individual PIN-level reports detailing the
      characteristics, SHAP values, and results for a given PIN

6.  **Upload**: Upload all output objects to AWS (S3). All model outputs
    for every model run are stored in perpetuity in S3. Each run’s
    performance can be visualized using the CCAO’s internal Tableau
    dashboards. NOTE: This stage is only run internally, since it
    requires access to the CCAO Data AWS account.

7.  **Export**: Export assessed values to Desk Review spreadsheets for
    Valuations, as well as a delimited text format for upload to the
    system of record (iasWorld). NOTE: This stage is only run when a
    final model is selected. It is not run automatically or as part of
    the main pipeline.

## Choices Made

Despite its reputation as an easy-to-use panacea, machine learning
actually involves a number of choices and trade-offs which are not
always transparent or well-justified. Seemingly inane decisions by
algorithm creators and data scientists [can introduce systemic
bias](https://www.scientificamerican.com/article/how-nist-tested-facial-recognition-algorithms-for-racial-bias/)
into results.

To counter this, we’ve listed the major choices we’ve made about our
modeling process below, as well as the rationale behind each decision.
We feel strongly that these choices lead to optimal results given the
trade-offs involved, but we’re [absolutely open to suggestions and
criticism](#contributing).

### Model Selection

We use [LightGBM](https://lightgbm.readthedocs.io/en/latest/) for our
primary valuation model. LightGBM is a [GBDT (gradient-boosting decision
tree)](https://arogozhnikov.github.io/2016/06/24/gradient_boosting_explained.html)
framework created and maintained by Microsoft. It has [an excellent R
API](https://cran.r-project.org/web/packages/lightgbm/index.html) and
has been around since 2016.

We [tried a number of other model types and
frameworks](https://github.com/ccao-data/model-res-avm/issues/31),
including regularized linear models,
[XGBoost](https://xgboost.readthedocs.io/en/latest/),
[CatBoost](https://catboost.ai/), random forest, shallow neural
networks, and support vector machines. We even tried ensemble methods
such as [model
stacking](https://github.com/ccao-data/model-res-avm/commit/77de50dce86986f8d442f05c161438933c097958).
We chose LightGBM because it has the right mix of trade-offs for our
needs. Specifically, LightGBM is:

- [Well-documented](https://lightgbm.readthedocs.io/en/latest/). The
  docs contain good explanations of LightGBM’s features and useful
  troubleshooting sections.
- Highly accurate. It consistently beat other methods in accuracy, as
  [measured by RMSE (root mean squared error) using a test set](#faqs).
- Extremely fast. It trained faster than other model types by a nearly
  2:1 margin using our data (CPU training only).
- [Capable of natively handling categorical
  features](https://lightgbm.readthedocs.io/en/latest/Advanced-Topics.html#categorical-feature-support).
  This is extremely important as a large amount of our property data is
  categorical (type of roof, neighborhood, etc.). Other methods, such as
  XGBoost, require feature transformation such as one-hot encoding to
  use categorical data.
- Widely used in housing-specific machine learning models and
  competitions.
- Simpler to use and implement than ensemble methods or neural networks,
  which can involve lots of fiddling and configuration.
- Easy to diagnose problems with, as it has built-in feature importance
  and contribution methods.

The downsides of LightGBM are that it is:

- Relatively difficult to explain compared to simpler models such as
  linear regression.
- Not particularly well-integrated into
  [Tidymodels](https://www.tidymodels.org/), the R framework we use for
  machine learning. See [Framework Selection](#framework-selection).
- Painful to train, since it has a large number of hyperparameters.
- Prone to over-fitting if not trained carefully, unlike other methods
  such as random forest.

For a more in-depth report on the performance and accuracy trade-offs
between LightGBM and XGBoost specific to our use case, please see our
[Model
Benchmark](https://github.com/ccao-data/report-model-benchmark?tab=readme-ov-file#model-benchmark)
repository.

### Framework Selection

We use [Tidymodels](https://www.tidymodels.org/) as our primary
machine-learning framework. Tidymodels is a set of R packages that work
well together and with the [Tidyverse](https://www.tidyverse.org/).
These packages abstract away complicated machine-learning logic and
allow us to focus on improving our data and models.

Additionally, Tidymodels is:

- [Well-documented](https://www.tmwr.org/). There are resources for
  quickly learning the Tidymodels approach as well as complete
  documentation for each Tidymodels package.
- [Under very active development](https://github.com/tidymodels).
  Developers are quick to respond to issues and feature requests.
- Quick to teach, since a lot of complicated code is abstracted away.
- [Extensible](https://www.tidymodels.org/learn/develop/models/). The
  API allows for easy integration of additional model types. See
  [Lightsnip](#lightsnip).
- Verbose. It tends to warn you about common machine-learning footguns
  and has excellent error handling and messages.

Some downsides to Tidymodels are that it is:

- Relatively new. While its API is mature, there are still bugs in core
  packages.
- [Under active development](https://github.com/tidymodels). Packages
  and features change fairly quickly, so we need to constantly update
  code to stay current.

##### Lightsnip

We’ve create a custom R package called
[Lightsnip](https://github.com/ccao-data/lightsnip) to better integrate
LightGBM with Tidymodels and unlock some of its more advanced features,
including:

- Early stopping, which reduces training time by stopping based on a
  holdout validation set
- Additional [hyperparameters](#hyperparameter-selection), particularly
  those related to categorical features
- The ability to link certain hyperparameters which typically move in
  tandem, such as `num_leaves` and `max_depth`

Lightsnip also ensures that the link between the model engine (LightGBM)
and the model framework (Tidymodels) is stable. It lets us quickly
respond to any upstream changes while maintaining the set of features we
need.

### Hyperparameter Selection

[Hyperparameters](https://en.wikipedia.org/wiki/Hyperparameter_(machine_learning))
define the structure and trade-offs of models. They must be
well-specified in order for a model to be accurate and useful. LightGBM
has a large number of tunable parameters, but we tune only a small
proportion, including:

| LightGBM Parameter                                                                                 | CV Search Range | Parameter Description                                                                                        |
|:---------------------------------------------------------------------------------------------------|:----------------|:-------------------------------------------------------------------------------------------------------------|
| [num_iterations](https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_iterations)         | 100 - 2500      | Total number of trees/iterations. Final value is dependent on CV and early stopping.                         |
| [learning_rate](https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning_rate)           | 0.001 - 0.398   | Speed of training per iteration. Higher usually means faster convergence, but possibly higher overall error. |
| [max_bin](https://lightgbm.readthedocs.io/en/latest/Parameters.html#max_bin)                       | 50 - 512        | Maximum number of bins used to bucket continuous features                                                    |
| [num_leaves](https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_leaves)                 | 32 - 2048       | Maximum number of leaves in each tree. Main parameter to control model complexity.                           |
| [add_to_linked_depth](https://ccao-data.github.io/lightsnip/reference/train_lightgbm.html)         | 1 - 7           | Amount to add to `max_depth` if linked to `num_leaves`. See `max_depth`.                                     |
| [feature_fraction](https://lightgbm.readthedocs.io/en/latest/Parameters.html#feature_fraction)     | 0.3 - 0.7       | The random subset of features selected for a tree, as a percentage.                                          |
| [min_gain_to_split](https://lightgbm.readthedocs.io/en/latest/Parameters.html#min_gain_to_split)   | 0.001 - 10000   | The minimum gain needed to create a split.                                                                   |
| [min_data_in_leaf](https://lightgbm.readthedocs.io/en/latest/Parameters.html#min_data_in_leaf)     | 2 - 400         | The minimum data in a single tree leaf. Important to prevent over-fitting.                                   |
| [max_cat_threshold](https://lightgbm.readthedocs.io/en/latest/Parameters.html#max_cat_threshold)   | 10 - 250        | Maximum number of split points for categorical features                                                      |
| [min_data_per_group](https://lightgbm.readthedocs.io/en/latest/Parameters.html#min_data_per_group) | 2 - 400         | Minimum number of observations per categorical group                                                         |
| [cat_smooth](https://lightgbm.readthedocs.io/en/latest/Parameters.html#cat_smooth)                 | 10 - 200        | Categorical smoothing. Used to reduce noise.                                                                 |
| [cat_l2](https://lightgbm.readthedocs.io/en/latest/Parameters.html#cat_l2)                         | 0.001 - 100     | Categorical-specific L2 regularization                                                                       |
| [lambda_l1](https://lightgbm.readthedocs.io/en/latest/Parameters.html#lambda_l1)                   | 0.001 - 100     | L1 regularization                                                                                            |
| [lambda_l2](https://lightgbm.readthedocs.io/en/latest/Parameters.html#lambda_l2)                   | 0.001 - 100     | L2 regularization                                                                                            |

These parameters are tuned using [Bayesian hyperparameter
optimization](https://www.tidymodels.org/learn/work/bayes-opt/), which
iteratively searches the parameter space based on the previous parameter
tuning results. We use Bayesian tuning instead of grid search or random
search because it trains faster and results in nearly identical final
parameters.

Model accuracy for each parameter combination is measured on a
validation set using [rolling-origin
cross-validation](https://www.tmwr.org/resampling.html#rolling). Final
model accuracy is measured on a test set of the [most recent 10% of
sales](#data-used) in our training sample. For final model candidates,
we also measure model accuracy on a random (rather than time-based) test
set to ensure the model generalizes well.

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
powerfully predictive, and well-behaved.

| Feature Name                                                                | Variable Name                                         | Description                                                                                                                                           | Category       | Type        | Possible Values (Encoded) | Possible Values (Semantic)                                           |
|:----------------------------------------------------------------------------|:------------------------------------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------|:------------|:--------------------------|:---------------------------------------------------------------------|
| Percent Population Age, Under 19 Years Old                                  | acs5_percent_age_children                             | Percent of the people 17 years or younger                                                                                                             | ACS5           | numeric     |                           |                                                                      |
| Percent Population Age, Over 65 Years Old                                   | acs5_percent_age_senior                               | Percent of the people 65 years or older                                                                                                               | ACS5           | numeric     |                           |                                                                      |
| Median Population Age                                                       | acs5_median_age_total                                 | Median age for whole population                                                                                                                       | ACS5           | numeric     |                           |                                                                      |
| Percent Households Family, Married                                          | acs5_percent_household_family_married                 | Percent of households that are family, married                                                                                                        | ACS5           | numeric     |                           |                                                                      |
| Percent Households Nonfamily, Living Alone                                  | acs5_percent_household_nonfamily_alone                | Percent of households that are non-family, alone (single)                                                                                             | ACS5           | numeric     |                           |                                                                      |
| Percent Population Education, High School Degree                            | acs5_percent_education_high_school                    | Percent of people older than 25 who attained a high school degree                                                                                     | ACS5           | numeric     |                           |                                                                      |
| Percent Population Education, Bachelor Degree                               | acs5_percent_education_bachelor                       | Percent of people older than 25 who attained a bachelor’s degree                                                                                      | ACS5           | numeric     |                           |                                                                      |
| Percent Population Education, Graduate Degree                               | acs5_percent_education_graduate                       | Percent of people older than 25 who attained a graduate degree                                                                                        | ACS5           | numeric     |                           |                                                                      |
| Percent Population Income, Below Poverty Level                              | acs5_percent_income_below_poverty_level               | Percent of people above the poverty level in the last 12 months                                                                                       | ACS5           | numeric     |                           |                                                                      |
| Median Income, Household in Past Year                                       | acs5_median_income_household_past_year                | Median income per household in the past 12 months                                                                                                     | ACS5           | numeric     |                           |                                                                      |
| Median Income, Per Capita in Past Year                                      | acs5_median_income_per_capita_past_year               | Median income per capita in the past 12 months                                                                                                        | ACS5           | numeric     |                           |                                                                      |
| Percent Population Income, Received SNAP in Past Year                       | acs5_percent_income_household_received_snap_past_year | Percent of households that received SNAP in the past 12 months                                                                                        | ACS5           | numeric     |                           |                                                                      |
| Percent Population Employment, Unemployed                                   | acs5_percent_employment_unemployed                    | Percent of people 16 years and older unemployed                                                                                                       | ACS5           | numeric     |                           |                                                                      |
| Median Occupied Household, Total, Year Built                                | acs5_median_household_total_occupied_year_built       | Median year built for all occupied households                                                                                                         | ACS5           | numeric     |                           |                                                                      |
| Median Occupied Household, Renter, Gross Rent                               | acs5_median_household_renter_occupied_gross_rent      | Median gross rent for only renter-occupied units                                                                                                      | ACS5           | numeric     |                           |                                                                      |
| Percent Occupied Households, Owner                                          | acs5_percent_household_owner_occupied                 | Percent of households that are owner-occupied                                                                                                         | ACS5           | numeric     |                           |                                                                      |
| Year Built                                                                  | char_yrblt                                            | Year the property was constructed                                                                                                                     | Characteristic | numeric     |                           |                                                                      |
| Central Air Conditioning                                                    | char_air                                              | Indicator for central air                                                                                                                             | Characteristic | categorical | 1, 2                      | Central A/C, No Central A/C                                          |
| Apartments                                                                  | char_apts                                             | Number of apartments for class 211 and 212 properties                                                                                                 | Characteristic | categorical | 1, 2, 3, 4, 5, 6          | Two, Three, Four, Five, Six, None                                    |
| Attic Finish                                                                | char_attic_fnsh                                       | Attic finish                                                                                                                                          | Characteristic | categorical | 1, 2, 3                   | Living Area, Partial, None                                           |
| Attic Type                                                                  | char_attic_type                                       | Attic type                                                                                                                                            | Characteristic | categorical | 1, 2, 3                   | Full, Partial, None                                                  |
| Bedrooms                                                                    | char_beds                                             | Number of bedrooms in the building                                                                                                                    | Characteristic | numeric     |                           |                                                                      |
| Building Square Feet                                                        | char_bldg_sf                                          | Square footage of the building, as measured from the exterior                                                                                         | Characteristic | numeric     |                           |                                                                      |
| Basement Type                                                               | char_bsmt                                             | Basement type                                                                                                                                         | Characteristic | categorical | 1, 2, 3, 4                | Full, Slab, Partial, Crawl                                           |
| Basement Finish                                                             | char_bsmt_fin                                         | Basement finish                                                                                                                                       | Characteristic | categorical | 1, 2, 3                   | Formal Rec Room, Apartment, Unfinished                               |
| Property Class                                                              | char_class                                            | Card-level property type and/or use                                                                                                                   | Characteristic | character   |                           |                                                                      |
| Exterior Wall Material                                                      | char_ext_wall                                         | Exterior wall construction                                                                                                                            | Characteristic | categorical | 1, 2, 3, 4                | Frame, Masonry, Frame + Masonry, Stucco                              |
| Full Baths                                                                  | char_fbath                                            | Number of full bathrooms                                                                                                                              | Characteristic | numeric     |                           |                                                                      |
| Fireplaces                                                                  | char_frpl                                             | Number of fireplaces                                                                                                                                  | Characteristic | numeric     |                           |                                                                      |
| Garage 1 Attached                                                           | char_gar1_att                                         | Indicator for garage attached                                                                                                                         | Characteristic | categorical | 1, 2                      | Yes, No                                                              |
| Garage 1 Ext. Wall Material                                                 | char_gar1_cnst                                        | Garage exterior wall construction                                                                                                                     | Characteristic | categorical | 1, 2, 3, 4                | Frame, Masonry, Frame + Masonry, Stucco                              |
| Garage 1 Size                                                               | char_gar1_size                                        | Garage size (number of cars)                                                                                                                          | Characteristic | categorical | 1, 2, 3, 4, 5, 6, 7, 8    | 1 cars, 1.5 cars, 2 cars, 2.5 cars, 3 cars, 3.5 cars, 0 cars, 4 cars |
| Half Baths                                                                  | char_hbath                                            | Number of half baths                                                                                                                                  | Characteristic | numeric     |                           |                                                                      |
| Land Square Feet                                                            | char_land_sf                                          | Square footage of the land (not just the building) of the property                                                                                    | Characteristic | numeric     |                           |                                                                      |
| Central Heating                                                             | char_heat                                             | Interior heating type                                                                                                                                 | Characteristic | categorical | 1, 2, 3, 4                | Warm Air Furnace, Hot Water Steam, Electric Heater, None             |
| Number of Commercial Units                                                  | char_ncu                                              | Number of commercial units                                                                                                                            | Characteristic | numeric     |                           |                                                                      |
| Porch                                                                       | char_porch                                            | Porch type                                                                                                                                            | Characteristic | categorical | 0, 1, 2                   | None, Frame Enclosed, Masonry Enclosed                               |
| Roof Material                                                               | char_roof_cnst                                        | Roof material / construction                                                                                                                          | Characteristic | categorical | 1, 2, 3, 4, 5, 6          | Shingle + Asphalt, Tar + Gravel, Slate, Shake, Tile, Other           |
| Rooms                                                                       | char_rooms                                            | Number of total rooms in the building (excluding baths)                                                                                               | Characteristic | numeric     |                           |                                                                      |
| Cathedral Ceiling                                                           | char_tp_dsgn                                          | Deprecated                                                                                                                                            | Characteristic | categorical | 1, 2                      | Yes, No                                                              |
| Type of Residence                                                           | char_type_resd                                        | Type of residence                                                                                                                                     | Characteristic | categorical | 1, 2, 3, 4, 5, 9.9        | 1 Story, 2 Story, 3 Story +, Split Level, 1.5 Story, Missing         |
| Recent Renovation                                                           | char_recent_renovation                                | Indicates whether or not a property was renovated within the last 3 years                                                                             | Characteristic | logical     |                           |                                                                      |
| Longitude                                                                   | loc_longitude                                         | X coordinate in degrees (global longitude)                                                                                                            | Location       | numeric     |                           |                                                                      |
| Latitude                                                                    | loc_latitude                                          | Y coordinate in degrees (global latitude)                                                                                                             | Location       | numeric     |                           |                                                                      |
| Census Tract GEOID                                                          | loc_census_tract_geoid                                | 11-digit ACS/Census tract GEOID                                                                                                                       | Location       | character   |                           |                                                                      |
| First Street Factor                                                         | loc_env_flood_fs_factor                               | First Street flood factor The flood factor is a risk score, where 10 is the highest risk and 1 is the lowest risk                                     | Location       | numeric     |                           |                                                                      |
| School Elementary District GEOID                                            | loc_school_elementary_district_geoid                  | School district (elementary) GEOID                                                                                                                    | Location       | character   |                           |                                                                      |
| School Secondary District GEOID                                             | loc_school_secondary_district_geoid                   | School district (secondary) GEOID                                                                                                                     | Location       | character   |                           |                                                                      |
| CMAP Walkability Score (No Transit)                                         | loc_access_cmap_walk_nta_score                        | CMAP walkability score for a given PIN, excluding transit walkability                                                                                 | Location       | numeric     |                           |                                                                      |
| CMAP Walkability Total Score                                                | loc_access_cmap_walk_total_score                      | CMAP walkability score for a given PIN, including transit walkability                                                                                 | Location       | numeric     |                           |                                                                      |
| Municipality Name                                                           | loc_tax_municipality_name                             | Taxing district name, as seen on Cook County tax bills                                                                                                | Location       | character   |                           |                                                                      |
| Township Code                                                               | meta_township_code                                    | Cook County township code                                                                                                                             | Meta           | character   |                           |                                                                      |
| Neighborhood Code                                                           | meta_nbhd_code                                        | Assessor neighborhood code                                                                                                                            | Meta           | character   |                           |                                                                      |
| Number of sales within previous N years of sale/lien date                   | meta_sale_count_past_n_years                          | Number of sales within previous N years of sale/lien date                                                                                             | Meta           | numeric     |                           |                                                                      |
| Property Tax Bill Aggregate Rate                                            | other_tax_bill_rate                                   | Tax bill rate for the taxing district containing a given PIN                                                                                          | Other          | numeric     |                           |                                                                      |
| School District (Elementary) GreatSchools Rating                            | other_school_district_elementary_avg_rating           | Average GreatSchools rating of elementary schools within the district of a given PIN                                                                  | Other          | numeric     |                           |                                                                      |
| School District (Secondary) GreatSchools Rating                             | other_school_district_secondary_avg_rating            | Average GreatSchools rating of secondary schools within the district of a given PIN                                                                   | Other          | numeric     |                           |                                                                      |
| Active Homeowner Exemption                                                  | ccao_is_active_exe_homeowner                          | Parcel has an active homeowner exemption                                                                                                              | Other          | logical     |                           |                                                                      |
| Number of Years Active Homeowner Exemption                                  | ccao_n_years_exe_homeowner                            | Number of years parcel has had an active homeowner exemption                                                                                          | Other          | numeric     |                           |                                                                      |
| Standard Deviation Distance From Parcel Centroid to Vertices (Feet)         | shp_parcel_centroid_dist_ft_sd                        | Standard deviation of the distance from each major parcel vertex to the parcel centroid                                                               | Parcel Shape   | numeric     |                           |                                                                      |
| Standard Deviation Parcel Edge Length (Feet)                                | shp_parcel_edge_len_ft_sd                             | Standard deviation of the edge length between parcel vertices                                                                                         | Parcel Shape   | numeric     |                           |                                                                      |
| Standard Deviation Parcel Interior Angle (Degrees)                          | shp_parcel_interior_angle_sd                          | Standard deviation of the interior angles of the parcel polygon                                                                                       | Parcel Shape   | numeric     |                           |                                                                      |
| Ratio of Parcel Area to Minimum Rotated Bounding Rectangle                  | shp_parcel_mrr_area_ratio                             | Ratio of the parcel’s area to the area of its minimum rotated bounding rectangle                                                                      | Parcel Shape   | numeric     |                           |                                                                      |
| Ratio of Parcel Minimum Rotated Bounding Rectangle Longest to Shortest Side | shp_parcel_mrr_side_ratio                             | Ratio of the longest to the shortest side of the parcel’s minimum rotated bounding rectangle                                                          | Parcel Shape   | numeric     |                           |                                                                      |
| Number of Parcel Vertices                                                   | shp_parcel_num_vertices                               | The number of vertices of the parcel                                                                                                                  | Parcel Shape   | numeric     |                           |                                                                      |
| Number of PINs in Half Mile                                                 | prox_num_pin_in_half_mile                             | Number of PINs within half mile                                                                                                                       | Proximity      | numeric     |                           |                                                                      |
| Number of Bus Stops in Half Mile                                            | prox_num_bus_stop_in_half_mile                        | Number of bus stops within half mile                                                                                                                  | Proximity      | numeric     |                           |                                                                      |
| Number of Foreclosures Per 1000 PINs (Past 5 Years)                         | prox_num_foreclosure_per_1000_pin_past_5_years        | Number of foreclosures per 1000 PINs, within half mile (past 5 years)                                                                                 | Proximity      | numeric     |                           |                                                                      |
| Average School Rating in Half Mile                                          | prox_avg_school_rating_in_half_mile                   | Average school rating of schools within half mile                                                                                                     | Proximity      | numeric     |                           |                                                                      |
| Total Airport Noise DNL                                                     | prox_airport_dnl_total                                | Estimated DNL for a PIN, assuming a baseline DNL of 50 (“quiet suburban”) and adding predicted noise from O’Hare and Midway airports to that baseline | Proximity      | numeric     |                           |                                                                      |
| Nearest Bike Trail Distance (Feet)                                          | prox_nearest_bike_trail_dist_ft                       | Nearest bike trail distance (feet)                                                                                                                    | Proximity      | numeric     |                           |                                                                      |
| Nearest Cemetery Distance (Feet)                                            | prox_nearest_cemetery_dist_ft                         | Nearest cemetery distance (feet)                                                                                                                      | Proximity      | numeric     |                           |                                                                      |
| Nearest CTA Route Distance (Feet)                                           | prox_nearest_cta_route_dist_ft                        | Nearest CTA route distance (feet)                                                                                                                     | Proximity      | numeric     |                           |                                                                      |
| Nearest CTA Stop Distance (Feet)                                            | prox_nearest_cta_stop_dist_ft                         | Nearest CTA stop distance (feet)                                                                                                                      | Proximity      | numeric     |                           |                                                                      |
| Nearest Hospital Distance (Feet)                                            | prox_nearest_hospital_dist_ft                         | Nearest hospital distance (feet)                                                                                                                      | Proximity      | numeric     |                           |                                                                      |
| Lake Michigan Distance (Feet)                                               | prox_lake_michigan_dist_ft                            | Distance to Lake Michigan shoreline (feet)                                                                                                            | Proximity      | numeric     |                           |                                                                      |
| Nearest Metra Route Distance (Feet)                                         | prox_nearest_metra_route_dist_ft                      | Nearest Metra route distance (feet)                                                                                                                   | Proximity      | numeric     |                           |                                                                      |
| Nearest Metra Stop Distance (Feet)                                          | prox_nearest_metra_stop_dist_ft                       | Nearest Metra stop distance (feet)                                                                                                                    | Proximity      | numeric     |                           |                                                                      |
| Nearest Park Distance (Feet)                                                | prox_nearest_park_dist_ft                             | Nearest park distance (feet)                                                                                                                          | Proximity      | numeric     |                           |                                                                      |
| Nearest Railroad Distance (Feet)                                            | prox_nearest_railroad_dist_ft                         | Nearest railroad distance (feet)                                                                                                                      | Proximity      | numeric     |                           |                                                                      |
| Nearest University Distance (Feet)                                          | prox_nearest_university_dist_ft                       | Nearest university distance (feet)                                                                                                                    | Proximity      | numeric     |                           |                                                                      |
| Nearest Vacant Land Parcel Distance (Feet)                                  | prox_nearest_vacant_land_dist_ft                      | Nearest vacant land (class 100) parcel distance (feet)                                                                                                | Proximity      | numeric     |                           |                                                                      |
| Nearest Water Distance (Feet)                                               | prox_nearest_water_dist_ft                            | Nearest water distance (feet)                                                                                                                         | Proximity      | numeric     |                           |                                                                      |
| Nearest Golf Course Distance (Feet)                                         | prox_nearest_golf_course_dist_ft                      | Nearest golf course distance (feet)                                                                                                                   | Proximity      | numeric     |                           |                                                                      |
| Nearest Highway Distance (Feet)                                             | prox_nearest_road_highway_dist_ft                     | Distance to nearest highway road                                                                                                                      | Proximity      | numeric     |                           |                                                                      |
| Nearest Arterial Road Distance (Feet)                                       | prox_nearest_road_arterial_dist_ft                    | Distance to nearest arterial road                                                                                                                     | Proximity      | numeric     |                           |                                                                      |
| Nearest Collector Road Distance (Feet)                                      | prox_nearest_road_collector_dist_ft                   | Distance to nearest collector road                                                                                                                    | Proximity      | numeric     |                           |                                                                      |
| Average Daily Traffic Count on Nearest Highway                              | prox_nearest_road_highway_daily_traffic               | Daily traffic of nearest highway road                                                                                                                 | Proximity      | numeric     |                           |                                                                      |
| Average Daily Traffic Count on Nearest Arterial Road                        | prox_nearest_road_arterial_daily_traffic              | Daily traffic of nearest arterial road                                                                                                                | Proximity      | numeric     |                           |                                                                      |
| Average Daily Traffic Count on Nearest Collector Road                       | prox_nearest_road_collector_daily_traffic             | Daily traffic of nearest collector road                                                                                                               | Proximity      | numeric     |                           |                                                                      |
| Nearest New Construction (Feet)                                             | prox_nearest_new_construction_dist_ft                 | Nearest new construction distance (feet)                                                                                                              | Proximity      | numeric     |                           |                                                                      |
| Nearest Major Stadium (Feet)                                                | prox_nearest_stadium_dist_ft                          | Nearest stadium distance (feet)                                                                                                                       | Proximity      | numeric     |                           |                                                                      |
| Sale Year                                                                   | time_sale_year                                        | Sale year calculated as the number of years since 0 B.C.E                                                                                             | Time           | numeric     |                           |                                                                      |
| Sale Day                                                                    | time_sale_day                                         | Sale day calculated as the number of days since January 1st, 1997                                                                                     | Time           | numeric     |                           |                                                                      |
| Sale Quarter of Year                                                        | time_sale_quarter_of_year                             | Character encoding of quarter of year (Q1 - Q4)                                                                                                       | Time           | character   |                           |                                                                      |
| Sale Month of Year                                                          | time_sale_month_of_year                               | Character encoding of month of year (Jan - Dec)                                                                                                       | Time           | character   |                           |                                                                      |
| Sale Day of Year                                                            | time_sale_day_of_year                                 | Numeric encoding of day of year (1 - 365)                                                                                                             | Time           | numeric     |                           |                                                                      |
| Sale Day of Month                                                           | time_sale_day_of_month                                | Numeric encoding of day of month (1 - 31)                                                                                                             | Time           | numeric     |                           |                                                                      |
| Sale Day of Week                                                            | time_sale_day_of_week                                 | Numeric encoding of day of week (1 - 7)                                                                                                               | Time           | numeric     |                           |                                                                      |
| Sale After COVID-19                                                         | time_sale_post_covid                                  | Indicator for whether sale occurred after COVID-19 was widely publicized (around March 15, 2020)                                                      | Time           | logical     |                           |                                                                      |

We maintain a few useful resources for working with these features:

- Once you’ve [pulled the input data](#getting-data), you can inner join
  the data to the CSV version of the data dictionary
  ([`docs/data-dict.csv`](./docs/data-dict.csv)) to filter for only the
  features that we use in the model.
- You can browse our [data
  catalog](https://ccao-data.github.io/data-architecture/#!/overview) to
  see more details about these features, in particular the [residential
  model input
  view](https://ccao-data.github.io/data-architecture/#!/model/model.ccao_data_athena.model.vw_card_res_input)
  which is the source of our training data.
- You can use the [`ccao` R package](https://ccao-data.github.io/ccao/)
  or its [Python equivalent](https://ccao-data.github.io/ccao/python/)
  to programmatically convert variable names to their human-readable
  versions
  ([`ccao::vars_rename()`](https://ccao-data.github.io/ccao/reference/vars_rename.html))
  or convert numerically-encoded variables to human-readable values
  ([`ccao::vars_recode()`](https://ccao-data.github.io/ccao/reference/vars_recode.html).
  The [`ccao::vars_dict`
  object](https://ccao-data.github.io/ccao/reference/vars_dict.html) is
  also useful for inspecting the raw crosswalk that powers the rename
  and recode functions.

#### Data Sources

We rely on numerous third-party sources to add new features to our data.
These features are used in the primary valuation model and thus need to
be high-quality and error-free. A non-exhaustive list of features and
their respective sources includes:

| Feature                                           | Data Source                                                                                                                                                                                                                                                                                              |
|---------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Tax rate                                          | Cook County Clerk’s Office                                                                                                                                                                                                                                                                               |
| Airport noise                                     | Noise monitoring stations via the Chicago Department of Aviation                                                                                                                                                                                                                                         |
| Road proximity                                    | Buffering [OpenStreetMap](https://www.openstreetmap.org/#map=10/41.8129/-87.6871) motorway, trunk, and primary roads                                                                                                                                                                                     |
| Flood risk and direction                          | [First Street](https://firststreet.org/risk-factor/flood-factor/) flood data                                                                                                                                                                                                                             |
| All Census features                               | [ACS 5-year estimates](https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2018/5-year.html) for each respective year                                                                                                                                        |
| Elementary school district or attendance boundary | [Cook County school district boundaries](https://datacatalog.cookcountyil.gov/GIS-Maps/Historical-ccgisdata-Elementary-School-Tax-Distric/an6r-bw5a) and [CPS attendance boundaries](https://data.cityofchicago.org/Education/Chicago-Public-Schools-Elementary-School-Attendanc/7edu-z2e8)              |
| High school district or attendance boundary       | [Cook County high school district boundaries](https://datacatalog.cookcountyil.gov/GIS-Maps/Historical-ccgisdata-High-School-Tax-Dist-2016/h3xu-azvs) and [CPS high school attendance boundaries](https://data.cityofchicago.org/Education/Chicago-Public-Schools-High-School-Attendance-Boun/y9da-bb2y) |
| Walkability                                       | The [Chicago Metropolitan Agency for Planning’s](https://www.cmap.illinois.gov/) ON TO 2050 [Walkability Scores](https://datahub.cmap.illinois.gov/datasets/CMAPGIS::walkability-2018/about)                                                                                                             |
| Subdivision, unincorporated areas, SSAs, etc.     | Cook County GIS                                                                                                                                                                                                                                                                                          |
| PUMA Housing Index                                | [DePaul Institute for Housing Studies](https://www.housingstudies.org/)                                                                                                                                                                                                                                  |
| School Ratings                                    | [GreatSchools.org](https://greatschools.org/), aggregated to the district level                                                                                                                                                                                                                          |
| Distance to CTA, PACE, Metra                      | Each agency’s respective [GTFS feed](https://gtfs.org/), which contains the location of stops and lines                                                                                                                                                                                                  |

#### Features Excluded

Many people have intuitive assumptions about what drives the value of
their home, so we often receive the question, “Is X taken into account
when valuing my property?” Here’s a list of commonly-asked-about
features which are *not* in the model, as well as rationale for why
they’re excluded:

| Feature                                                | Reason It’s Excluded                                                                                                                                                                                                                                           |
|--------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Property condition                                     | We track property condition, but over 98% of the properties in our data have the same condition, meaning it’s not tracked effectively and there’s not enough variation for it to be predictive of sale price.                                                  |
| Crime                                                  | Crime is highly correlated with features that are already in the model, such as income and neighborhood, so it doesn’t add much predictive power. Additionally, it is difficult to reliably aggregate crime data from all of Cook County.                      |
| Interior features such as kitchen quality or amenities | Our office can only access the outside of buildings; we can’t reliably observe interior property characteristics beyond what is available through building permits.                                                                                            |
| Blighted building or eyesore in my neighborhood        | If a specific building or thing affects sale prices in your neighborhood, this will already be reflected in the model through [neighborhood fixed effects](https://en.wikipedia.org/wiki/Fixed_effects_model).                                                 |
| Pictures of property                                   | We don’t have a way to reliably use image data in our model, but we may include such features in the future.                                                                                                                                                   |
| Comparable properties                                  | The model will automatically find and use comparable properties when producing an estimate. However, the model *does not* explicitly use or produce a set of comparable properties.                                                                            |
| Flood indicator                                        | Between the First Street flood risk and direction data, distance to water, and precise latitude and longitude for each parcel, the contribution of [FEMA flood hazard data](https://hazards.fema.gov/femaportal/prelimdownload/) to the model approached zero. |

### Data Used

The model uses two primary data sets that are constructed by the [ingest
stage](./pipeline/00-ingest.R), as well as a few secondary data sets for
valuation. These data sets are included in the [`input/`](input/)
directory for the purpose of replication.

#### Primary Data

- [`training_data`](#getting-data) - Includes residential sales from
  ***the 9 years prior to the next assessment date***, which gives us a
  sufficient amount of data for accurate prediction without including
  outdated price information. This is the data used to train and
  evaluate the model. Its approximate size is 400K rows with 100
  features.
- [`assessment_data`](#getting-data) - Includes all residential
  properties (sold and unsold) which need assessed values. This is the
  data the final model is used on. Its approximate size is 1.1 million
  rows with 100 features.

These data sets contain only *residential single- and multi-family
properties*. Single-family includes property
[classes](https://prodassets.cookcountyassessor.com/s3fs-public/form_documents/classcode.pdf)
202, 203, 204, 205, 206, 207, 208, 209, 210, 234, 278, and 295.
Multi-family includes property classes 211 and 212. Bed and breakfast
properties (class 218 and 219) are considered single-family for the sake
of modeling, but are typically valued later by hand. Other residential
properties, such as condominiums (class 299 and 399) are valued [using a
different model](https://github.com/ccao-data/model-condo-avm).

##### Using `training_data`

Models need data in order to be trained and measured for accuracy.
Modern predictive modeling typically uses three data sets:

1.  A training set, used to train the parameters of the model itself.
2.  A validation set, used to choose a hyperparameter combination that
    optimizes model accuracy.
3.  A test set, used to measure the performance of the trained, tuned
    model on unseen data.

`training_data` is used to create these data sets. It is subdivided
using a technique called out-of-time testing.

###### Figure 1: Out-of-Time Testing

Out-of-time testing explicitly measures the model’s ability to predict
recent sales. It holds out the most recent 10% of sales as a test set,
while the remaining 90% of the data is split into training and
validation sets.

![](docs/figures/oot_sampling.png)

###### Figure 2: Rolling-Origin Resampling

The training data is further subdivided using a technique called
rolling-origin resampling. For this method, a fixed window of time is
used to increment the size of the training set, while the validation set
is always 10% of sales immediately following the training set. This
helps cross-validation determine which hyperparameters will perform best
when predicting future sales.

![](docs/figures/rolling_origin.png)

###### Figure 3: Final Training

Once we’re satisfied with the model’s performance on recent sales, we
retrain the model using the full sales sample (all rows in
`training_data`). This gives the final model more (and more recent)
sales to learn from.

![](docs/figures/final_sampling.png)

##### Using `assessment_data`

Finally, the model, trained on the full sales sample from
`training_data`, can be used to predict assessed values for all
residential properties. To do this, we set the “sale date” of all
properties in `assessment_data` to Jan 1st of the assessment year, then
use the final model to predict what the sale price would be on that
date.

These sale prices are our initial prediction for what each property is
worth. They eventually become the assessed value sent to taxpayers after
some further adjustments (see [Post-Modeling](#post-modeling)) and hand
review.

#### Secondary Data

The pipeline also uses a few secondary data sets in the valuation
process. These data sets are included in [`input/`](./input) but are not
actually used by the model itself. They include:

- [`char_data`](#getting-data) - The complete `assessment_data` set as
  well as the same data for the previous year. This data is used for
  automated model performance reporting rather than valuation.
- [`complex_id_data`](#getting-data) - Complex identifiers for class 210
  and 295 town/rowhomes. Intended to group like units together to ensure
  that nearly identical units in close proximity receive the same
  assessed value. This is accomplished with a “fuzzy grouping” strategy
  that allows slightly dissimilar characteristics.
- [`hie_data`](#getting-data) - Home improvement exemption data used to
  evaluate whether the pipeline correctly updates card-level
  characteristics triggered by the expiration of home improvement
  exemptions.
- [`land_site_rate_data`](#getting-data) - Fixed, PIN-level land values
  for class 210 and 295 units. Provided by the Valuations department.
  Not always used, so may be 0 rows for certain years.
- [`land_nbhd_rate_data`](#getting-data) - Fixed \$/sqft land rates by
  assessor neighborhood for residential property classes except 210
  and 295. Provided by the Valuations department.

#### Representativeness

There’s a common saying in the machine learning world: “garbage in,
garbage out.” This is a succinct way to say that training a predictive
model with bad, unrepresentative, or biased data leads to bad results.

To help mitigate the bad data problem and ensure accurate prediction, we
do our best to ensure that the sales data used to train the model is
representative of the actual market and universe of properties. We
accomplish this in two ways.

##### 1. Sales Validation

We use a heuristics-based approach to drop non-arms-length sales, remove
outliers, and manually flag certain suspect sales. This approach was
developed in partnership with the [Mansueto
Institute](https://miurban.uchicago.edu/). As of 2023, the sales
validation code can be found in a dedicated repository at
[ccao-data/model-sales-val](https://github.com/ccao-data/model-sales-val).
Please visit that repository for more information.

##### 2. Balance Tests

We also perform basic balance tests to determine if the universe of
properties sold is analogous to the universe of all properties. The code
for these tests can be found under [`reports/`](reports/). The goal of
the tests is to see if any characteristics are significantly predictive
of sale status, and the tests generally take the form of a logistic
regression with the following specification:

    sold_in_last_2_years = β₀ + βₙcharacteristics + βₙlocation_fixed_effects + ... + ε

There a few caveats with this approach and with balance testing in
general:

1.  There could be statistically significant omitted variables that
    differentiate sold from unsold. Things like `recently_painted` or
    `full_kitchen_renovation` are good examples. We don’t collect these
    data points, so it could be the case that sold properties are more
    “sale-ready” in these unknown terms.
2.  There could be significant variation by geography in the
    representativeness of the sales. In other words, certain areas could
    have non-representative sales whose predictive effect on
    `sold_in_last_2_years` is washed out due to mis- or under-specified
    geographic sampling.

### Post-Modeling

In addition to the first-pass modeling done by LightGBM, the CCAO also
performs a set of simple adjustments on the initial predicted values
from the `assess` stage. These adjustments are internally called
“post-modeling,” and are responsible for correcting minor deficiencies
in the initial predictions. Specifically, post-modeling will:

1.  Aggregate values for multi-card properties to the PIN level, then
    disaggregate them back to the card level. A check is used to ensure
    that the PIN-level assessed value is not significantly greater than
    the prior year’s value. This is needed because often back buildings
    (ADUs, secondary buildings) will receive a much higher initial value
    than they are actually worth (since they are not differentiated as
    ADUs by the model).

2.  Ensure that nearly identical properties are identically valued. For
    some property classes, such as 210 and 295s, we manually adjust
    values such that all identical properties in the same complex
    receive the same predicted value. This is accomplished by replacing
    individual predicted values with the average predicted value for the
    complex.

3.  Round PIN-level values (typically to the nearest \$1,000). This is
    done to indicate that model values are *estimates*, not precise
    values.

These adjustments have been collectively approved by the senior
leadership of the CCAO. They are designed to limit the impact of data
integrity issues, prevent regressivity in assessment, and ensure that
people with nearly identical properties receive the same value.

## Major Changes from Previous Versions

### [`assessment-year-2021`](https://github.com/ccao-data/model-res-avm/tree/2021-assessment-year)

This repository represents a significant departure from the old
[residential modeling
codebase](https://gitlab.com/ccao-data-science---modeling/ccao_sf_cama_dev)
used to create assessed values in 2019 and 2020. As the CCAO’s Data
department has grown, we’ve been able to dedicate more resources to
building models, applications, and other tools. As a result, we’ve made
the following major changes to the residential modeling codebase:

- Reduced the size of the codebase substantially from around 16,000
  lines of R code. This was accomplished by moving complicated data
  handling to our [internal R
  package](https://github.com/ccao-data/ccao) and abstracting away
  machine learning logic to [Tidymodels](https://www.tidymodels.org/).
- Unified modeling for the entire county. Prior iterations of the
  residential model used individual models for each township. This was
  difficult to implement and track and performed worse than a single
  large model. The new model can value any residential property in the
  county, is significantly faster to train, and is much easier to
  replicate.
- Split the residential codebase into separate models for
  [single/multi-family](https://github.com/ccao-data/model-res-avm) and
  [condominiums](https://github.com/ccao-data/model-condo-avm).
  Previously, these models were combined in the same scripts, leading to
  a lot of complications and unnecessary overhead. Separating them makes
  it much easier to understand and diagnose each model.
- Switched to using LightGBM as our primary valuation model. LightGBM is
  essentially the most bleeding-edge machine learning framework widely
  available that isn’t a neural network. Prior to using LightGBM, we
  used linear models or R’s gbm package. Prior to 2018, the CCAO used
  linear models in SPSS for residential valuations.
- Improved dependency management via
  [renv](https://rstudio.github.io/renv/articles/renv.html). Previously,
  users trying replicate our model needed to manually install a list of
  needed R packages. By switching to renv, we’ve vastly reduced the
  effort needed to replicate our modeling environment, see the
  [installation section](#installation) below.

### [`assessment-year-2022`](https://github.com/ccao-data/model-res-avm/tree/2022-assessment-year)

- Moved previously separate processes into this repository and improved
  their integration with the overall modeling process. For example, the
  [etl_res_data](https://gitlab.com/ccao-data-science---modeling/processes/etl_res_data)
  process was moved to [pipeline/00-ingest.R](pipeline/00-ingest.R),
  while the process to [finalize model
  values](https://gitlab.com/ccao-data-science---modeling/processes/finalize_model_values)
  was moved to [pipeline/07-export.R](pipeline/07-export.R).
- Added [DVC](https://dvc.org/) support/integration. This repository
  uses DVC in 2 ways:
  1.  All input data in [`input/`](input/) is versioned, tracked, and
      stored using DVC. Previous input data sets are stored in
      perpetuity on S3.
  2.  [DVC
      pipelines](https://dvc.org/doc/user-guide/project-structure/pipelines-files)
      are used to sequentially run R pipeline scripts and track/cache
      inputs and outputs.
- All model runs are now saved in perpetuity on S3. Each model’s outputs
  are saved as Parquet files which can be queried using Amazon Athena.
- Offloaded model reporting entirely to Tableau. This repository no
  longer produces markdown-based model outcome reports.
- Improved model accuracy significantly while reducing training time.
  This is largely due to the use of
  [Lightsnip](https://github.com/ccao-data/lightsnip) and the inclusion
  of many [new features](#features-used).
- Added per feature, per property contributions via LightGBM’s built-in
  SHAP methods.
- Reorganized the codebase into explicit pipeline stages, each of which
  can be run independently or via DVC.
- Added GitHub [CI integration](./.github), which ensures that any model
  changes don’t result in significant output changes.

### [`assessment-year-2023`](https://github.com/ccao-data/model-res-avm/tree/2023-assessment-year)

- Added updated [sales flagging and validation
  scripts](./py/flagging.py) in partnership with the Mansueto Institute.
  See [Representativeness](#representativeness).
- [Rewrote](https://github.com/ccao-data/model-res-avm/commit/1932f0ddd577b4c750940c7ed802136b1fabfeb9)
  the assessment stage for speed and improved accuracy when valuing
  prorated and multi-card PINs.
- Added new [feature
  importance](https://lightgbm.readthedocs.io/en/latest/R/reference/lgb.importance.html)
  output table, which shows the gain, frequency, and cover for each
  model run.
- Added [model QC and balance testing reports](./reports) for ad-hoc
  analysis of model inputs.
- Updated multi-card heuristic to only apply to PINs with 2 cards
  (improvements on the same parcel).
- Updated [townhome complex valuation
  method](https://github.com/ccao-data/model-res-avm/commit/98283e36c851d14a770ccb33cbe1fec0557451e4)
  to prevent “chaining” via fuzzy grouping.
- Updated CV implementation so that
  [Lightsnip](https://github.com/ccao-data/lightsnip) and Tidymodels
  share the same validation set: Lightsnip for early stopping,
  Tidymodels for Bayesian optimization.
- Dropped explicit spatial lag generation in the ingest stage.
- Lots of other bugfixes and minor improvements.

### [`assessment-year-2024`](https://github.com/ccao-data/model-res-avm/tree/2024-assessment-year)

- Moved sales validation to a dedicated repository located at
  [ccao-data/model-sales-val](https://github.com/ccao-data/model-sales-val).
- Infrastructure improvements
  - Added
    [`build-and-run-model`](https://github.com/ccao-data/model-res-avm/actions/workflows/build-and-run-model.yaml)
    workflow to run the model using GitHub Actions and AWS Batch.
  - Added
    [`delete-model-run`](https://github.com/ccao-data/model-res-avm/actions/workflows/delete-model-runs.yaml)
    workflow to delete test run artifacts in S3 using GitHub Actions.
  - Updated [pipeline/05-finalize](pipeline/05-finalize.R) step to
    render a performance report using Quarto and factored S3/SNS
    operations out into [pipeline/06-upload.R](pipeline/06-upload.R).
- Added additional [regressivity metrics
  (MKI)](https://researchexchange.iaao.org/jptaa/vol17/iss2/2/) to
  measure model performance.
- Switched cross-validation to
  [V-fold](https://rsample.tidymodels.org/reference/vfold_cv.html)
  instead of time-based.
- Added new model features: corner lots, distance to vacant
  land/university/secondary roads, homeowner exemption indicator and
  length of exemption, number of recent sales, class.
- Added linear baseline model for comparison against LightGBM to
  [pipeline/01-train](pipeline/01-train.R).
- Added experimental comparable sales generation using LightGBM leaf
  nodes to [pipeline/04-interpret](pipeline/04-interpret.R).
- Refactored shared pipeline logic into [separate scripts](./R/setup.R)
  to simplify development and maintainability.
- Separated development/reporting dependencies from primary dependencies
  using [renv profiles](#profiles-and-lockfiles) to increase
  replicability.

# Ongoing Issues

The CCAO faces a number of ongoing issues which make modeling difficult.
Some of these issues are in the process of being solved; others are less
tractable. We list them here for the sake of transparency and to provide
a sense of the challenges we face.

### Data Quality and Integrity

We face a number of data-related challenges that are specific to our
office. These issues are largely the result of legacy data systems,
under-staffing, and the sheer number of properties in Cook County (over
1 million residential properties). We’re actively working to correct or
mitigate most of these issues.

##### Lack of Property Characteristics

Our office tracks around 40 characteristics of individual properties. Of
those 40, about 25 are [usable in modeling](#data-used). The remaining
15 characteristics are too sparse, too dirty, or too unbalanced to use.
Additionally, our data is missing features commonly used in property
valuation, such as:

- Property condition.
- Lot frontage.
- Land slope.
- Percentage of property above grade.
- Quality of finishes.
- Electrical and utility systems.
- Interior characteristics like finish quality, recent remodeling, or
  kitchen quality.
- Any information about pools.
- Information about location desirability or views.

This lack of characteristics contributes to larger errors when modeling,
as it becomes difficult to distinguish between individual properties.
For example, an extremely run-down mansion with otherwise high-value
characteristics (good location, large number of bedrooms) may be
significantly over-assessed, due to our model not accounting for
property condition.

##### Inaccurate Property Characteristics

The property characteristics we track can sometimes be incorrect or
outdated. The two major sources of characteristic errors are:

1.  Data entry or processing errors. Records collected by our office
    often need to digitized and mistakes happen. Fortunately, these
    types of errors are relatively rare.
2.  Characteristic update errors. There are a variety of systems that
    update the characteristics of properties in our system. Some of them
    can be slow to detect changes or otherwise unreliable.

These errors can cause under- *or* over-assessment. If you believe your
property has been misvalued due to a characteristic error or the
property characteristics recorded on our website are incorrect. Please
[contact our office](https://www.cookcountyassessor.com/contact) to file
a property characteristic appeal.

##### Non-Arms-Length Sales

It is difficult for our office to determine whether or not any given
property sale is
[arms-length](https://www.investopedia.com/terms/a/armslength.asp).
Non-arms-length sales, such as selling your home to a family member at a
discount, can bias the model and result in larger assessment errors. We
do our best to [remove non-arms-length sales](#representativeness), but
it’s nearly impossible to know for certain that every transaction is
valid.

##### Incentives Not to Disclose Accurate Information

The Cook County property tax system is complex and can sometimes create
perverse incentives.

For example, most property owners want their property taxes to be as low
as possible, and are thus disincentivized from reporting characteristic
errors which could raise their assessed value. Conversely, if a property
owner plans to sell their home on a listing website, then they have a
strong incentive (the highest possible sale price) to ensure the website
accurately reflects their property’s characteristics. Listing websites
know this and offer easy ways to self-update property attributes.

Falsely altering or not reporting property characteristics may change an
assessed value, but it also has negative consequences for neighbors and
similar properties. High sales on homes with incorrectly reported
characteristics can upwardly bias the model, resulting in
over-assessment for others.

### Heterogeneity and Extremes

In addition to the data challenges that are specific to our office, we
also face the same modeling issues as most assessors and machine
learning practitioners.

##### Housing Heterogeneity

Cook County is an extremely large and diverse housing market. It spans
millions of properties that vary widely in type, age, location, and
quality. In some regions of the county, sales are common; in other
regions, sales are sparse. Accurately estimating the price of such
different properties and regions is a complicated, challenging task.

This challenge is especially acute in areas with high housing
characteristic and price heterogeneity. For example, the Hyde Park
neighborhood in Chicago is home to the University of Chicago and has
large, multi-million-dollar houses near campus. However, sale prices
drop precipitously just a few blocks away, as one passes south of 63rd
street or west of I-90. This sort of sharp price discontinuity makes it
difficult to accurately assess properties, as models tend to “smooth”
such hard breaks unless geographic boundaries are explicitly defined.

Hyde Park is only one example, similarly unique situations exist
throughout the county. Our model *does* account for some of these
situations through neighborhood fixed effects and other location
factors. However, effectively modeling major drivers of heterogeneity is
an ongoing challenge.

##### High and Low-Value Properties

Mass appraisal models need lots of sales data in order to accurately
predict sale prices, but sales become more sparse toward either end of
the price spectrum. The vast majority of properties (over 90%) in Cook
County sell for between \$50K and \$2.5M. Predicting sale prices outside
of that range is difficult; there just aren’t enough representative
sales to train the model effectively.

This issue is particularly prevalent within certain geographies with
unevenly distributed sales. For example, in New Trier township the
average 2021 sale price was around \$1.2 million, compared to the whole
county average of around \$400K. Lower values sales closer to the county
average are rare in New Trier. Due to that rarity, lower value
properties in New Trier are more likely to be overvalued. The same
situation exists in reverse for lower value areas.

This problem isn’t limited to mass appraisal models; predictive models
in general are not good at predicting outliers. We may implement new
machine learning techniques or policies to deal with this issue in the
future.

# FAQs

**Q: My assessed value seems too low or too high. How do I fix it?**

There are over one million residential properties in Cook County
spanning a huge variety of locations, types, ages, and conditions. Mass
appraisal should produce fair valuations for most properties. But a mass
appraisal model isn’t going to accurately value every single property.
If you believe that the value produced by our model is inaccurate,
please [file an
appeal](https://www.cookcountyassessor.com/online-appeals) with our
office.

**Q: My home has been sold recently. Why isn’t my assessed value equal
to my sale price?**

Setting the assessed value of a home equal to the value of a recent sale
is called selective appraisal or sales chasing. Sales chasing can
artificially improve assessment performance statistics and bias
statistical models. Worse, it can bias assessment accuracy in favor of
recently sold properties, giving an unfair advantage to areas or
properties with high turnover. For more information, see [Appendix E of
the IAAO Standard on Ratio
Studies](https://www.iaao.org/media/standards/Standard_on_Ratio_Studies.pdf).

**Q: How are comparables used in the model?**

We don’t use sale or uniformity comparables for the purpose of modeling.
Our model works by automatically finding patterns in sales data and
extrapolating those patterns to predict prices; the model never
explicitly says, “Here is property X and here are Y similar properties
and their sale prices.”

We *do* use [comparables for other
things](https://www.cookcountyassessor.com/what-are-comparable-properties),
namely when processing appeals and when evaluating the model’s
performance. Note however that the comparables generated via
[\#106](https://github.com/ccao-data/model-res-avm/pull/106) are
*experimental* and are not currently used.

**Q: What are the most important features in the model?**

The importance of individual features in the model varies from place to
place. Some properties will gain \$50K in value from an additional
bedroom, while others will gain almost nothing. However, some factors do
stand out as more influential:

- Location. Two identical single-family homes, one in Wicker Park, the
  other in Markham, will not receive the same valuation. Location is the
  largest driver of county-wide variation in property value. This is
  accounted for in our model through a number of [location-based
  features](#features-used) such as school district, neighborhood,
  township, and others.
- Square footage. Larger homes tend to be worth more than smaller ones,
  though there are diminishing marginal returns.
- Number of bedrooms and bathrooms. Generally speaking, the more rooms
  the better, though again there are diminishing returns. The value
  added by a second bedroom is much more than the value added by a
  twentieth bedroom.

**Q: How much will one additional bedroom add to my assessed value?**

Our model is non-linear, meaning it’s difficult to say things like,
“Each additional square foot will increase this property’s value by
\$50,” as the relationship between price and individual features varies
from property to property.

We do calculate the contribution of each feature to each property’s
final value. For example, we can say things like, “Your close proximity
to Lake Michigan added \$5,000 to your home’s value.” We’re currently
working on a way to share those feature-level results with property
owners.

**Q: Why don’t you use a simple linear model?**

We decided that performance was more important than the easy
interpretability offered by linear models, and LightGBM tends to
outperform linear models on data with a large number of categorical
features, interactions, and non-linearities.

**Q: How do you measure model performance?**

Assessors tend to use [housing and assessment-specific
measurements](https://www.iaao.org/media/standards/Standard_on_Ratio_Studies.pdf)
to gauge the performance of their mass appraisal systems, including:

- [COD (Coefficient of
  Dispersion)](https://ccao-data.github.io/assessr/reference/cod.html)
- [PRD (Price-Related
  Differential)](https://ccao-data.github.io/assessr/reference/prd.html)
- [PRB (Price-Related
  Bias)](https://ccao-data.github.io/assessr/reference/prb.html)
- [MKI (Modified Kakwani
  Index)](https://ccao-data.github.io/assessr/reference/mki_ki.html)

More traditionally, we use R<sup>2</sup>, root-mean-squared-error
(RMSE), mean absolute error (MAE), and mean absolute percentage error
(MAPE) to gauge overall model performance and fit.

**Q: How often does the model change?**

We’re constantly making minor tweaks to improve the model’s accuracy,
speed, and usability. However, major changes to the model typically take
place during the downtime between reassessments, so about once per year.

# Usage

There are two ways of running the model:

- [On a local machine](#running-the-model-locally-all-users) (available
  to all users)
- [In the cloud via AWS
  Batch](#running-the-model-on-aws-batch-ccao-staff-only) (only
  available to CCAO staff)

## Running the Model Locally (All Users)

The code in this repository is written primarily in
[R](https://www.r-project.org/about.html). Please install the [latest
version of R](https://cloud.r-project.org/) (requires R version \>=
4.2.1) and [RStudio](https://posit.co/download/rstudio-desktop/) before
proceeding with the steps below.

If you’re on Windows, you’ll also need to install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) in order to
build the necessary packages. You may also want to (optionally) install
[DVC](https://dvc.org/doc/install) to pull data and run the pipeline.

We also publish a Docker image containing the model code and all of the
dependencies necessary to run it. If you’re comfortable using Docker,
you can skip the installation steps below and instead pull the image
from `ghcr.io/ccao-data/model-res-avm:master` to run the latest version
of the model.

### Installation

1.  Clone this repository using git, or simply download it using the
    button at the top of the page.
2.  Set your working directory to the local folder containing this
    repository’s files, either using R’s `setwd()` command or
    (preferably) using RStudio’s
    [projects](https://support.posit.co/hc/en-us/articles/200526207-Using-Projects).
3.  Install `renv`, R’s package manager, by running
    `install.packages("renv")`.
4.  Install all R package dependencies using `renv` by running
    `renv::restore()`. This step may take awhile. Linux users will
    likely need to install dependencies (via apt, yum, etc.) to build
    from source.
5.  (Optional) The `finalize` step of the model pipeline requires some
    additional dependencies for generating a model performance report.
    Install these additional dependencies by running
    `renv::restore(lockfile = "renv/profiles/reporting/renv.lock")`.
    These dependencies must be installed in addition to the core
    dependencies installed in step 4. If dependencies are not installed,
    the report will fail to generate and the pipeline stage will print
    the error message to the report file at `reports/performance.html`;
    the pipeline will continue to execute in spite of the failure.

For installation issues, particularly related to package installation
and dependencies, see [Managing R
dependencies](#managing-r-dependencies) and
[Troubleshooting](#troubleshooting).

### Running

#### Manually

To use this repository, simply open the [pipeline/](./pipeline)
directory and run the R scripts in order. Non-CCAO users can skip the
following stages:

- [`pipeline/00-ingest.R`](pipeline/00-ingest.R) - Requires access to
  CCAO internal AWS services to pull data. See [Getting
  Data](#getting-data) if you are a member of the public.
- [`pipeline/06-upload.R`](pipeline/06-upload.R) - Requires access to
  CCAO internal AWS services to upload model results.
- [`pipeline/07-export.R`](pipeline/07-export.R) - Only required for
  CCAO internal processes.

#### Using DVC

The entire end-to-end pipeline can also be run using
[DVC](https://dvc.org/). DVC will track the dependencies and parameters
required to run each stage, cache intermediate files, and store
versioned input data on S3.

To pull all the necessary input data based on the information in
`dvc.lock`, run:

``` bash
dvc pull
```

To run the entire pipeline (excluding the export stage), run:

``` bash
dvc repro
```

Note that each stage will run only if necessary i.e. the ingest stage
will *not* run if no parameters related to that stage have changed. To
force a stage to re-run, run:

``` bash
# Change ingest to any stage name
dvc repro -f ingest
```

To force the entire pipeline to re-run, run:

``` bash
dvc repro -f
```

The web of dependencies, outputs, parameters, and intermediate files is
defined via the [`dvc.yaml`](./dvc.yaml) file. See that file for more
information about each stage’s outputs, inputs/dependencies, and related
parameters (defined in [`params.yaml`](./params.yaml)).

## Running the Model on AWS Batch (CCAO Staff Only)

If you have write permissions for this repository (i.e. you are a member
of the CCAO Data Department), you can run the model in the cloud on AWS
Batch using GitHub Actions workflow runs.

### Executing a Run

#### Initialization

Model runs are initiated by the
[`build-and-run-model`](./.github/workflows/build-and-run-model.yaml)
workflow via [manual
dispatch](https://docs.github.com/en/actions/using-workflows/manually-running-a-workflow).

To run a model, use the **Run workflow** button on right side of the
`build-and-run-model` [Actions
page](https://github.com/ccao-data/model-res-avm/actions/workflows/build-and-run-model.yaml).

Runs are gated behind a [deploy
environment](https://docs.github.com/en/enterprise-cloud@latest/actions/deployment/targeting-different-environments/using-environments-for-deployment)
that requires approval from a `@ccao-data/core-team` member before the
model will run. The `build` job to rebuild a Docker image for the model
will always run, but the subsequent `run` job will not run unless a
core-team member approves it.

#### Monitoring

Runs can be monitored on AWS via CloudWatch as they execute in a Batch
job. Navigate to the run logs in the GitHub Actions console and look for
the `build-and-run-model / run` job. Find the
`Wait for Batch job to start and print link to AWS logs` step and expand
it to reveal a link to the CloudWatch logs for the run.

### Deleting Test Runs

Test runs of the model can be deleted using the
[`delete-model-runs`](./.github/workflows/build-and-run-model.yaml)
workflow. This workflow will delete all of the associated run artifacts
from S3. To delete one or more runs, copy their unique IDs
(e.g. `2024-01-01-foo-bar`) and paste them in the workflow dispatch
input box, with each run ID separated by a space
(e.g. `2024-01-01-foo-bar 2024-02-02-bar-baz`).

> :warning: NOTE: In order to protect production model run artifacts,
> the `delete-model-runs` workflow can only delete model runs for the
> upcoming assessment cycle (the current year from January-April, or the
> next year from May-December). The workflow will raise an error if you
> attempt to delete a model run outside the upcoming assessment cycle.
>
> In the off chance that you do in fact need to delete a test run from a
> previous assessment cycle, you can work around this limitation by
> moving model run artifacts to bucket prefixes representing the
> partition for the upcoming assessment year (e.g. `year=2024/`) and
> then proceed to delete the model run.

## Parameters

All control parameters, hyperparameters, toggles, etc. are stored in
[`params.yaml`](./params.yaml). Almost all modifications to the pipeline
are made via this file. It also contains a full description of each
parameter and its purpose.

Each R script has a set of associated parameters (tracked via
`dvc.yaml`). DVC will automatically detect changes in these parameters
and will re-run stages for which parameters have changed. Stages without
changed parameters or input data are cached and will be automatically
skipped by DVC.

## Output

The full model pipeline produces a large number of outputs. A full list
of these outputs and their purpose can be found in
[`misc/file_dict.csv`](misc/file_dict.csv). For public users, all
outputs are saved in the [`output/`](output/) directory, where they can
be further used/examined after a model run. For CCAO employees, all
outputs are uploaded to S3 via the [upload stage](pipeline/06-upload).
Uploaded Parquet files are converted into the following Athena tables:

#### Athena Tables

| Athena Table         | Observation Unit                   | Primary Key                                                                  | Description                                                                           |
|:---------------------|:-----------------------------------|:-----------------------------------------------------------------------------|:--------------------------------------------------------------------------------------|
| assessment_card      | card                               | year, run_id, township_code, meta_pin, meta_card_num                         | Assessment results at the card level AKA raw model output                             |
| assessment_pin       | pin                                | year, run_id, township_code, meta_pin                                        | Assessment results at the PIN level AKA aggregated and cleaned                        |
| comp                 | card                               | year, run_id, meta_pin, meta_card_num                                        | Comparables for each card (computed using leaf node assignments)                      |
| feature_importance   | predictor                          | year, run_id, model_predictor_all_name                                       | Feature importance values (gain, cover, and frequency) for the run                    |
| metadata             | model run                          | year, run_id                                                                 | Information about each run, including parameters, run ID, git info, etc.              |
| parameter_final      | model run                          | year, run_id                                                                 | Chosen set of hyperparameters for each run                                            |
| parameter_range      | parameter                          | year, run_id, parameter_name                                                 | Range of hyperparameters searched during CV tuning                                    |
| parameter_search     | model cv fold                      | year, run_id, configuration, fold_id                                         | Tidymodels tuning output from cross-validation                                        |
| performance          | geography \[by class\]             | year, run_id, stage, geography_type, geography_id, by_class, class           | Peformance metrics (optionally) broken out by class for different levels of geography |
| performance_quantile | geography \[by class\] by quantile | year, run_id, stage, geography_type, geography_id, by_class, class, quantile | Performance metrics by quantile within class and geography                            |
| shap                 | card                               | year, run_id, township_code, meta_pin, meta_card_num                         | SHAP values for each feature for each card in the assessment data                     |
| test_card            | card                               | year, meta_pin, meta_card_num                                                | Test set predictions at the card level                                                |
| timing               | model run                          | year, run_id                                                                 | Finalized time elapsed for each stage of the run                                      |

## Getting Data

The [data required](#data-used) to run these scripts is produced by the
[ingest stage](pipeline/00-ingest.R), which uses SQL pulls from the
CCAO’s Athena database as a primary data source. CCAO employees can run
the ingest stage or pull the latest version of the input data from our
internal DVC store using:

``` bash
dvc pull
```

Public users can download data for each assessment year using the links
below. Each file should be placed in the [`input/`](input/) directory
prior to running the model pipeline.

#### 2021

- [assmntdata.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2021/assmntdata.parquet)
- [modeldata.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2021/modeldata.parquet)

#### 2022

- [assessment_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2022/assessment_data.parquet)
- [complex_id_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2022/complex_id_data.parquet)
- [land_nbhd_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2022/land_nbhd_rate_data.parquet)
- [land_site_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2022/land_site_rate_data.parquet)
- [training_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2022/training_data.parquet)

#### 2023

- [assessment_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2023/assessment_data.parquet)
- [complex_id_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2023/complex_id_data.parquet)
- [land_nbhd_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2023/land_nbhd_rate_data.parquet)
- [land_site_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2023/land_site_rate_data.parquet)
- [training_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2023/training_data.parquet)

#### 2024

Due to a [data
issue](https://github.com/ccao-data/data-architecture/pull/334) with the
initial 2024 model run, there are actually *two* final 2024 models. The
run `2024-02-06-relaxed-tristan` was used for Rogers Park and West
townships only, while the run `2024-03-17-stupefied-maya` was used for
all subsequent City of Chicago townships.

The data issue caused some sales to be omitted from the
`2024-02-06-relaxed-tristan` training set, however the actual impact on
predicted values was *extremely* minimal. We chose to update the data
and create a second final model out of an abundance of caution, and,
given low transaction volume in 2023, to include as many arms-length
transactions in the training set as possible.

##### 2024-02-06-relaxed-tristan

- [assessment_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-02-06-relaxed-tristan/assessment_data.parquet)
- [char_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-02-06-relaxed-tristan/char_data.parquet)
- [complex_id_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-02-06-relaxed-tristan/complex_id_data.parquet)
- [hie_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-02-06-relaxed-tristan/hie_data.parquet)
- [land_nbhd_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-02-06-relaxed-tristan/land_nbhd_rate_data.parquet)
- [land_site_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-02-06-relaxed-tristan/land_site_rate_data.parquet)
- [training_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-02-06-relaxed-tristan/training_data.parquet)

##### 2024-03-17-stupefied-maya (final)

- [assessment_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-03-17-stupefied-maya/assessment_data.parquet)
- [char_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-03-17-stupefied-maya/char_data.parquet)
- [complex_id_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-03-17-stupefied-maya/complex_id_data.parquet)
- [hie_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-03-17-stupefied-maya/hie_data.parquet)
- [land_nbhd_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-03-17-stupefied-maya/land_nbhd_rate_data.parquet)
- [land_site_rate_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-03-17-stupefied-maya/land_site_rate_data.parquet)
- [training_data.parquet](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id=2024-03-17-stupefied-maya/training_data.parquet)

For other data from the CCAO, please visit the [Cook County Data
Portal](https://datacatalog.cookcountyil.gov/).

## System Requirements

Both
[Tidymodels](https://tune.tidymodels.org/articles/extras/optimizations.html#parallel-processing)
and
[LightGBM](https://lightgbm.readthedocs.io/en/latest/Parallel-Learning-Guide.html)
support parallel processing to speed up model training. However, the
current parallel implementation in Tidymodels is extremely
memory-intensive, as it needs to carry loaded packages and objects into
each worker process. As such, parallel processing in Tidymodels is
turned **off**, while parallel processing in LightGBM is turned **on**.
This means that models are fit sequentially, but each model fitting is
sped up using the parallel processing built-in to LightGBM. Note that:

- The total amount of RAM needed for overall model fitting is around
  6GB, though this is ultimately dependent on a number of LightGBM
  parameters.
- The number of threads is set via the
  [num_threads](https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_threads)
  parameter, which is passed to the model using the `set_args()`
  function from `parsnip`. By default, `num_threads` is equal to the
  full number of physical cores available. More (or faster) cores will
  decrease total training time.
- This repository uses the CPU version of LightGBM included with the
  [LightGBM R
  package](https://lightgbm.readthedocs.io/en/latest/R/index.html). If
  you’d like to use the GPU version you’ll need to [build it
  yourself](https://lightgbm.readthedocs.io/en/latest/R/index.html#installing-a-gpu-enabled-build).

## Managing R Dependencies

We use [renv](https://rstudio.github.io/renv/index.html) to manage R
dependencies. The main model dependencies are listed explicitly in the
`DESCRIPTION` file under the `Depends:` key. These dependencies are
installed automatically when you run `renv::restore()`.

### Profiles and Lockfiles

We use multiple renv lockfiles to manage R dependencies:

1.  **`renv.lock`** is the canonical list of dependencies that are used
    by the **core model pipeline**. Any dependencies that are required
    to run the model itself should be defined in this lockfile.
2.  **`renv/profiles/reporting/renv.lock`** is the canonical list of
    dependencies that are used to **generate model reports** in the
    `finalize` step of the pipeline. Any dependencies that are required
    to generate reports should be defined in this lockfile.
3.  **`renv/profiles/dev/renv.lock`** is the canonical list of
    dependencies that are used **for local development**, running the
    `ingest`, `export`, and `api` steps of the pipeline, and building
    the README. These dependencies are required only by CCAO staff and
    are not required to run the model itself.

Our goal in maintaining multiple lockfiles is to keep the list of
dependencies required to run the model as short as possible. This choice
adds overhead to the process of updating R dependencies, but incurs the
benefit of a more maintainable model over the long term.

### Using Lockfiles for Local Development

When working on the model locally, you’ll typically want to install
non-core dependencies *on top of* the core dependencies. To do this,
simply run `renv::restore(lockfile = "<path_to_lockfile")` to install
all dependencies from the lockfile.

For example, if you’re working on the `ingest` stage and want to install
all its dependencies, start with the main profile (run
`renv::activate()`), then install the `dev` profile dependencies on top
of it (run `renv::restore(lockfile = "renv/profiles/dev/renv.lock")`).

> :warning: WARNING: Installing dependencies from a dev lockfile will
> **overwrite** any existing version installed by the core one. For
> example, if `ggplot2@3.3.0` is installed by the core lockfile, and
> `ggplot2@3.2.1` is installed by the dev lockfile, renv will
> **overwrite** `ggplot2@3.3.0` with `ggplot2@3.2.1`.

### Updating Lockfiles

The process for **updating core model pipeline dependencies** is
straightforward:

1.  Add the dependency to the list of explicit dependencies under the
    `Depends:` key of the `DESCRIPTION` file
2.  Run `renv::install("<dependency_name>")`
3.  Run `renv::snapshot()` to update the core lockfile (the root
    `renv.lock`)

The process for updating \*dependencies for other lockfiles\*\* is more
complex, since it requires the use of a separate profile when running
renv commands. Determine the name of the profile you’d like to update
(`<profile_name>` in the code that follows) and run the following
commands:

1.  Run `renv::activate(profile = "<profile_name>")` to set the renv
    profile to `<profile_name>`
2.  Make sure that the dependency is defined in the `DESCRIPTION` file
    under the `Config/renv/profiles/<profile_name>/dependencies` key
3.  Run `renv::install("<dependency_name>")` to add or update the
    dependency as necessary
4.  Run `renv::snapshot()` to update the reporting lockfile with the
    dependencies defined in the `DESCRIPTION` file
5.  Run `renv::activate(profile = "default")` if you would like to
    switch back to the default renv profile

## Troubleshooting

The dependencies for this repository are numerous and not all of them
may install correctly. Here are some common install issues (as seen in
the R console) as well as their respective resolutions:

- Error:
  `WARNING: Rtools is required to build R packages, but is not currently installed`
  <br>Solution: Install the latest version of Rtools [from
  CRAN](https://cran.r-project.org/bin/windows/Rtools/), following the
  instructions listed.

- Error:
  `DLL '<package-name>' not found: maybe not installed for this architecture?`
  <br>Solution: Try installing the package manually with the
  `INSTALL_opts` flag set. See
  [here](https://github.com/rstudio/renv/issues/162#issuecomment-612380245)
  for an example.

# License

Distributed under the AGPL-3 License. See [LICENSE](./LICENSE) for more
information.

# Contributing

We welcome pull requests, comments, and other feedback via GitHub. For
more involved collaboration or projects, please see the [Developer
Engagement Program](https://github.com/ccao-data/people#external)
documentation on our group wiki.
