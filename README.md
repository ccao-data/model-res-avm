Table of Contents
================

  - [Model Overview](#model-overview)
      - [How It Works](#how-it-works)
      - [Choices Made](#choices-made)
          - [Model Selection](#model-selection)
          - [Hyperparameter Selection](#hyperparameter-selection)
          - [Features Used](#features-used)
          - [Data Used](#data-used)
          - [Postmodeling](#postmodeling)
          - [trim bounds](#trim-bounds)
      - [Ongoing Issues](#ongoing-issues)
      - [Major Changes From V1](#major-changes-from-v1)
      - [FAQs](#faqs)
  - [Technical Details](#technical-details)
  - [Replication/Usage](#replicationusage)
      - [Installation](#installation)
      - [Usage/Files](#usagefiles)
      - [Troubleshooting](#troubleshooting)
          - [Installation](#installation-1)

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

The full residential modeling pipeline, from raw data to final values,
looks approximately like the flowchart below. Note that **blue nodes are
clickable links to the code responsible for that step**.

``` mermaid
graph TB
    as400[("AS-400/<br>Mainframe")]
    mirror("Data mirrored from<br>County mainframe")
    ext_data(("External data<br>(Census data,<br>geospatial)"))
    etl_pinlocations("Ext. data joined<br>to property data")
    sql[("SQL<br>database")]
    etl_res_data("Data extracted<br>and cleaned")
    data_train["Raw training<br>(sales) data"]
    data_ass["Raw data<br>(all properties)"]

    prep1("Data preprocessing<br>for model")
    prep2("Data preprocessing<br>for assessment")
    model_step1("Model training<br>(Step 1 above)")
    model_step2("Valuation<br>(Step 2 above)")
    model{"Trained<br>Model"}
    final_vals["Final predicted/<br>assessed values"]
    desk_review("Hand review<br>and correction")

    click etl_res_data "https://gitlab.com/ccao-data-science---modeling/processes/etl_res_data"
    click etl_pinlocations "https://gitlab.com/ccao-data-science---modeling/processes/etl_pinlocations"
    click prep1 "https://gitlab.com/ccao-data-science---modeling/models/ccao_res_avm/-/blob/master/R/recipes.R#L5"
    click prep2 "https://gitlab.com/ccao-data-science---modeling/models/ccao_res_avm/-/blob/master/R/recipes.R#L5"

    classDef Link fill:#90a9e8;
    class etl_res_data Link;
    class etl_pinlocations Link;
    class prep1 Link;
    class prep2 Link;

    as400 --> mirror
    mirror --> sql
    ext_data --> etl_pinlocations
    etl_pinlocations --> sql
    sql --> etl_res_data
    etl_res_data --> data_train
    etl_res_data --> data_ass
    data_train --> prep1
    prep1 --> model_step1
    model_step1 --> model
    model --> model_step2
    data_ass --> prep2
    prep2 --> model_step2
    model_step2 --> final_vals
    final_vals --> desk_review
    desk_review -->|"Values uploaded after<br>hand review and correction"| as400
```

### Choices Made

Despite its growing reputation as an easy-to-use panacea, machine
learning actually involves a number of choices and trade-offs which are
not always transparent or well-justified. Seemingly inane decisions by
algorithm creators and data scientists [can introduce systemic
bias](https://www.scientificamerican.com/article/how-nist-tested-facial-recognition-algorithms-for-racial-bias/)
into results.

To counter this, we’ve listed the major choices we’ve made about our
modeling process below, as well as the rationale behind each decision.
We feel strongly that these choices lead to optimal results given the
trade-offs involved.

#### Model Selection

We use [LightGBM](https://lightgbm.readthedocs.io/en/latest/) for our
primary valuation model. LightGBM is a GBDT (gradient-boosting decision
tree) framework created and maintained by Microsoft. It was only very
recently [released officially for
R](https://cran.r-project.org/web/packages/lightgbm/index.html), but has
been around since 2016.

We tried a number of other model types and frameworks, including
regularized linear models,
[XGBoost](https://xgboost.readthedocs.io/en/latest/),
[CatBoost](https://catboost.ai/), random forest, shallow neural
networks, and support vector machines. We even tried ensemble methods
such as [model
stacking](https://gitlab.com/ccao-data-science---modeling/models/ccao_res_avm/-/commit/77de50dce86986f8d442f05c161438933c097958).
We chose LightGBM because it has the right mix of trade-offs for our
needs. Specifically, LightGBM is:

  - [Well-documented](https://lightgbm.readthedocs.io/en/latest/). The
    docs contain good explanations of LightGBM’s features and useful
    troubleshooting sections.
  - Extremely fast. It trained faster than other model types by a nearly
    2:1 margin using our data (CPU training only).
  - Highly accurate. It consistently beat other methods in accuracy, as
    measured by RMSE (root mean squared error) using a test set.
  - [Capable of natively handling categorical
    features](https://lightgbm.readthedocs.io/en/latest/Advanced-Topics.html#categorical-feature-support).
    This is extremely important as a large amount of our property data
    is categorical (type of roof, neighborhood, etc.). Other methods,
    such as XGBoost, require feature transformation such as one-hot
    encoding to use categorical data.
  - Widely used in housing-specific machine learning models and
    competitions.
  - Simpler to use and implement than ensemble methods or neural
    networks, which allows less room for error.
  - Easy to diagnose problems with, as it has built-in feature
    importance and contribution methods.

The downsides of LightGBM are that it is:

  - Relatively difficult to explain compared to simpler models such as
    linear regression.
  - Not particularly well-integrated into
    [Tidymodels](https://www.tidymodels.org/) (yet), the R framework we
    use for machine learning.
  - Painful to train, since it has a somewhat large number of
    hyperparameters.
  - Prone to over-fitting if not carefully trained, unlike other methods
    such as random forest.

Additionally, we run a regularized linear model (ElasticNet) to use as a
baseline comparison for LightGBM. LightGBM universally outperforms the
linear model, particularly in areas with high housing heterogeneity.

#### Hyperparameter Selection

Models must have well-specified
[hyperparameters](https://en.wikipedia.org/wiki/Hyperparameter_\(machine_learning\))
in order to be accurate and useful. LightGBM has a large number of
tunable parameters, but we train the most important six in our model.
These parameters are:

| LightGBM<br>Parameter                                                                               | Tidymodels<br>Equivalent | Parameter Description                                                                                                                                                                                                                          |
| --------------------------------------------------------------------------------------------------- | ------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [num\_leaves](https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_leaves)                 | num\_leaves              | The main parameter to control model complexity. Most important.                                                                                                                                                                                |
| [max\_depth](https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_leaves)                  | tree\_depth              | The maximum tree depth of the model.                                                                                                                                                                                                           |
| [min\_data\_in\_leaf](https://lightgbm.readthedocs.io/en/latest/Parameters.html#min_data_in_leaf)   | min\_n                   | The minimum data in a single tree leaf. Important to prevent over-fitting.                                                                                                                                                                     |
| [feature\_fraction](https://lightgbm.readthedocs.io/en/latest/Parameters.html#feature_fraction)     | mtry                     | The random subset of features selected for a tree, as a percentage. NOTE: treesnip [transforms this input](https://github.com/curso-r/treesnip/blob/29dab3e6f1c47dd6073bad3976b87ce4c6270184/R/lightgbm.R#L211) before passing it to LightGBM. |
| [min\_gain\_to\_split](https://lightgbm.readthedocs.io/en/latest/Parameters.html#min_gain_to_split) | loss\_reduction          | The minimum gain needed to create a split.                                                                                                                                                                                                     |
| [learning\_rate](https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning_rate)           | learn\_rate              | Higher learning rate means potentially faster training, depends on number of trees.                                                                                                                                                            |

These parameters are tuned using [Bayesian hyperparameter
optimization](https://www.tidymodels.org/learn/work/bayes-opt/), which
iteratively searches the parameter space based on the previous parameter
tuning results. We use Bayesian tuning instead of grid search or random
search because it trains far faster and results in nearly identical
final parameters.

The downside of Bayesian tuning is that it can waste time exploring a
useless part of the parameter space. In our case, LightGBM has [the
constraint](https://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html#tune-parameters-for-the-leaf-wise-best-first-tree)
that `num_leaves < 2^(max_depth)`. Unfortunately, there’s no way (yet)
to build this constraint into Tidymodels. We can partially solve this
issue by shrinking the possible parameter space by [hand-tuning minimum
and maximum parameter
values](https://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html).

Model accuracy for each parameter combination is measured on a
validation set using [5-fold
cross-validation](https://docs.aws.amazon.com/machine-learning/latest/dg/cross-validation.html).
Final model accuracy is measured on a test set of the most recent 10% of
sales in our training sample.

#### Features Used

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

##### Features Excluded

Many people have intuitive assumptions about what drives the value of
their home, so we often receive the question “Is X taken into account
when valuing my property?” Here’s a list of commonly asked about
features which are *not* in the model, as well as rationale for why
they’re excluded:

| Feature                                                | Reason It’s Excluded                                                                                                                                                                                                |
| ------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Property condition                                     | Over 98% of the properties in our data have the same condition, meaning it’s not tracked effectively and there’s not enough variation for it to be predictive of sale price.                                        |
| Being in an unincorporated area                        | [We’re working on it\!](https://gitlab.com/ccao-data-science---modeling/models/ccao_res_avm/-/issues/45)                                                                                                            |
| Crime                                                  | Crime is highly correlated with features that are already in the model, such as income and neighborhood, so it doesn’t add much predictive power. Additionally, it is difficult to reliably aggregate crime data.   |
| Interior features such as kitchen quality or amenities | Our office can only access the outside of buildings; we can’t reliably observe interior property characteristics beyond what is available through building permits.                                                 |
| Proximity to parks, the lake, the CTA, etc.            | These features are coming in the future\!                                                                                                                                                                           |
| Blighted building or eyesore in my neighborhood        | If a specific building or thing is affecting sale prices in your neighborhood, this will already be reflected in the model through [neighborhood fixed effects](https://en.wikipedia.org/wiki/Fixed_effects_model). |
| Pictures of property                                   | We don’t have a way to reliably use image data in our model, but we may include such features in the future.                                                                                                        |

#### Data Used

#### Postmodeling

#### trim bounds

### Ongoing Issues

  - Data integrity (wrong characteristics, bad sales, lack of chars)
  - Low-value properties
  - Multi-codes
  - Multi-family
  - Land valuation

### Major Changes From V1

  - Whole county
  - lightgbm
  - tidymodels
  - split codebase
  - dependency management

### FAQs

## Technical Details

Modeling is implemented using the
[Tidymodels](https://www.tidymodels.org/) framework for R.

LightGBM + glmnet (elasticnet)

lgbm integrated with treesnip + custom code

preprocessing in recipes

Tuned according to lgbm docs

Minimized on RMSE

Other models tried

## Replication/Usage

### Installation

The code in this repository is written primarily in
[R](https://www.r-project.org/about.html). Please install the [latest
version of R](https://cloud.r-project.org/) and
[RStudio](https://rstudio.com/products/rstudio/download/) before
proceeding with the steps below. If you’re on Windows, you’ll also need
to install [rtools40](https://cran.r-project.org/bin/windows/Rtools/) in
order to build packages.

1.  Clone this repository using git, or simply download using the button
    at the top of the page
2.  Set your working directory to the local folder containing this
    repository’s files, either using R’s `setwd()` command or using
    RStudio’s
    [projects](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects)
3.  Install `renv`, R’s package manager, by running
    `install.packages("renv")`
4.  Install all R package dependencies using `renv` by running
    `renv::restore()`
5.  Place modeling and assessment data parquet files in the `input/`
    directory within the repo
6.  Run the `model.R` script. This trains the model, evaluates a test
    set, and generates a report on the model’s peformance
7.  Run the `valuation.R` script. This creates a secondary adjustment
    model and generates predicted values for all properties we need to
    assess

### Usage/Files

### Troubleshooting

#### Installation

The dependencies for this repository are numerous and not all of them
may install correctly. Here are some common install issues (as seen in
the R console) as well as their respective resolutions:

  - Error: `Failed to retrieve package 'treesnip'` <br>Solution:
    Manually install treesnip [from
    GitHub](https://github.com/curso-r/treesnip), following the
    instructions listed

  - Error: `WARNING: Rtools is required to build R packages, but is not
    currently installed` <br>Solution: Install the latest version of
    Rtools [from CRAN](https://cran.r-project.org/bin/windows/Rtools/)
