---
name: Annual model checklist
about: steps take for pre - development - post modeling
title: Annual model checklist
labels: ''
assignees: ''
---

Each modeling season's folder has a planning doc. For one example see this: [Planning Docs](https://cookcounty.sharepoint.com/:f:/r/sites/Data-Assessor/Shared%20Documents/General/2026%20Initial%20Model%20Values/Planning%20Docs?csf=1&web=1&e=7IHmu4)

# Pre-Modeling

These stages should be completed before modeling season begins.

## Model Agnostic

- [ ] Update Sales-Val to reflect any changes in Sales – This should be done before we have access to the complete set of sales.
- [ ] Run an initial sales val run in November. Use this run to create a task list of issues that should be remedied before modeling season. Examples include:
  - [ ] Are the SD bounds and geography areas sensible?
  - [ ] Are we excluding too many sales in specific geographic regions?
- [ ] If valuations has capacity for our ongoing sale review collaboration:
  - [ ] Reiterate importance of exact data input so that our [transformation script](https://github.com/ccao-data/data-architecture/blob/master/etl/scripts-ccao-data-warehouse-us-east-1/sale/sale-flag_review.R) functions properly
  - [ ] Determine which sales to send. In 2025 we opted for current triad, current year sales and we ranked the sales based on algorithmic sales vals' standard deviation numbers. Whichever sales had the highest SD distance from the mean (per township) were prioritized for review
  - [ ] Set a deadline for them to return whatever they can review to us
  - [ ] Ingest sale review labels into our data lake
- [ ] Make sure sales are as up to date as possible — ultimately will need sales through the end of the year. The final necessary sales typically arrive by mid January. If we don't get sales by this date, we will have trouble delivering the model on time.
  - Stakeholder/point of contact: Valuations Sale Review manager

### Data Ingest / Refresh

The following [readme](https://github.com/ccao-data/data-architecture/blob/master/etl/README.md) provides guidance on how to run the ETL scripts.

- [ ] Run ETL scripts to get new feature data.
  - Key data arrival dates are **January** for Census data and **December** for Parcel data.
- [ ] Complete the [Checklist](https://cookcounty.sharepoint.com/:x:/r/sites/Data-Assessor/_layouts/15/Doc.aspx?sourcedoc=%7BF4732426-8A8E-4C63-9211-89E12C9AB1E4%7D&file=2026%20Modeling%20Data%20Refresh.xlsx&action=default&mobileredirect=true) for ingest / refresh
  - After completing the data refresh, make sure to run the model feature report to ensure nothing unexpected has happened.
- [ ] Update the [Land rates](https://github.com/ccao-data/data-architecture/blob/5dcb6dc79b42ae1bc4a834bcd28ea851e525256f/etl/scripts-ccao-data-warehouse-us-east-1/ccao/ccao-land-land_nbhd_rate.R) ETL script to account for new land rates


**Internal data requests:**

> Example: [ResModeling_2026Deadlines.pptx](https://cookcounty.sharepoint.com/:p:/r/sites/Data-Assessor/Shared%20Documents/General/2026%20Initial%20Model%20Values/Planning%20Docs/ResModeling_2026Deadlines.pptx?d=w9ad2102166bd4b94a1c6615837346ba5&csf=1&web=1&e=rMPn6d)

- [ ] Meet with stakeholders and agree on delivery dates for any data that we need (sales, reviewed sales, land rates) and our key deliverable (the model output and associated email) as well as any intermittent deliverables (sales for review). – **November**
- [ ] Check in with Res-Val to see if there are any requested changes to the desk review workbook. – **November**
- [ ] IasWorld Sales – Director of Special Valuations – **January**
- [ ] Sale Review– Valuations Sale Review manager – **December** 
- [ ] Land Rates – Chief Management Officer – **January**

### Condo specifics

- [ ] September condo chars update:
  - [ ] Develop a list of condo unit chars that we suggest Data Integrity should update. Good units to update have missing data or data that is suspected to be incorrect (e.g., statistically low square footage). See: [enterprise-intelligence/issues/330](https://github.com/ccao-data/enterprise-intelligence/issues/330)
  - [ ] Ingest the spreadsheet returned by Data Integrity: [data-architecture/issues/920](https://github.com/ccao-data/data-architecture/issues/920)
  - Note: Not sure if this is technically mission critical, as the model will run correctly with old unit chars that can be forward-filled.


## Res Model

- [ ] Create a milestone which groups together priorities for the upcoming modeling season. An example is [here](https://github.com/ccao-data/model-res-avm/milestone/2?closed=1).
- [ ] Update `Params.yaml` in the following locations:
  - **Assessment**
    - Year
    - Date
    - Triad
    - Data_year
    - Working_year
  - **Input**
    - Min_sale_year
    - Max_sale_year
  - **Model**
    - Seed (doesn't matter, just as a legacy practice)
  - **Ratio_study**
    - Far_year
    - Near_year
- [ ] Run [model_feature report](https://github.com/ccao-data/model-res-avm/blob/master/reports/model_features/model_features.qmd) via GitHub Actions.
  - Pay attention to any changes to or from NA values and key features such as schools, location (neighborhood, x-y coordinates), and characteristics (sq footage, rooms)
- [ ] Run the model and update DVC hashes to represent the newly ingested data. This must be done with a local run rather than GitHub Actions. This involves both pushing the DVC changes through `dvc push` as well as noting the changed values in the `params.yaml` file. See this pull request for updated [params](https://github.com/ccao-data/model-condo-avm/pull/125/changes).


## Condo Model

- [ ] Create a milestone which groups together priorities for the upcoming modeling season. An example is [here](https://github.com/ccao-data/model-condo-avm/milestone/1).
- [ ] Update `Params.yaml` in the following locations:
  - **Assessment**
    - Year
    - Date
    - Triad
    - Data_year
    - Working_year
  - **Input**
    - Min_sale_year
    - Max_sale_year
  - **Model**
    - Seed (doesn't matter, just as a legacy practice)
  - **Ratio_study**
    - Far_year
    - Near_year
- [ ] Run [model_feature report](https://github.com/ccao-data/model-condo-avm/blob/master/reports/model_features/model_features.qmd) via GitHub Actions.
  - Pay attention to any changes to or from NA values and key features such as schools, location (neighborhood, x-y coordinates), and characteristics (sq footage, rooms)
- [ ] Run the model and update DVC hashes to represent the newly ingested data. This must be done with a local run rather than GitHub Actions. This involves both pushing the DVC changes through `dvc push` as well as noting the changed values in the `params.yaml` file. See this pull request for updated [params](https://github.com/ccao-data/model-condo-avm/pull/125/changes).


---

# Post- Modeling Checklist

## High priority

High priority tasks must be complete before the model deadlines:

### Res model

- [ ] Make sure the final model has SHAPs and comps. If it doesn't, run one more model with SHAPs and comps enabled
- [ ] Tag the final model as `final` using the [`tag-model-runs` workflow](https://github.com/ccao-data/model-res-avm/actions/workflows/tag-model-runs.yaml)
- [ ] Export desk review workbooks for the [res model](https://github.com/ccao-data/model-res-avm/) using the `export` pipeline stage and upload them to OneDrive
- [ ] Export iasWorld upload files for the res model using the `export` pipeline stage and upload them to OneDrive
- [ ] Follow the instructions in the [model API README](https://github.com/ccao-data/api-res-avm/) to add the new final res model and set it as the default
- [ ] Export API workbooks for the res model using the `api` pipeline stage and upload them to OneDrive
  - This often requires adding or removing features from the workbook template to match the request format for the annual model, so get started on it a few days ahead of time
- [ ] Upload the performance report for the final res model to OneDrive
- [ ] Send the res model email with the subject line `$YEAR Initial Model Values (Residential)`

### Condo model

- [ ] Make sure the final model has SHAPs. If it doesn't, run one more model with them enabled
- [ ] Tag the final model as `final` using the [`tag-model-runs` workflow](https://github.com/ccao-data/model-res-avm/actions/workflows/tag-model-runs.yaml)
- [ ] Export desk review workbooks for [the condo model](https://github.com/ccao-data/model-condo-avm/) and upload them to OneDrive
- [ ] Export iasWorld upload files for the condo model and upload them to OneDrive
- [ ] Upload performance report for the final condo model to OneDrive
- [ ] Send condo model email with the subject line `$YEAR Initial Model Values (Condos)`

## Low priority

Low priority tasks must be complete eventually, but are not time-sensitive:

- [ ] Update the `model.final_model` seed in [`data-architecture`](https://github.com/ccao-data/data-architecture/) to include metadata for the res and condo models
- [ ] Make sure the `vars_dict` data in [`ccao`](https://github.com/ccao-data/ccao/) is up-to-date for new features
  - If you add any features to this dictionary that are used in either model, make sure to re-knit the README for models that use the feature
- [ ] Make sure any new features are up on the open data portal 
- [ ] Update `pinval` resources in [`data-architecture`](https://github.com/ccao-data/data-architecture/) to support this year's model
  - [ ] Double check the [`all_predictors`](https://github.com/ccao-data/data-architecture/blob/master/dbt/macros/all_predictors.sql) macro to make sure that the `pinval` views that use it are selecting any new features from this year's model
  - [ ] Add new assessment, SHAP, and comp rows to the [`pinval.model_run` seed](https://github.com/ccao-data/data-architecture/blob/master/dbt/seeds/pinval/pinval.model_run.csv)
- [ ] Take a pass through the res model README to make sure it's up to date
  - [ ] Update the "Major Changes from Previous Versions" section to include any major changes from this year
  - [ ] Double-check that the "Features Used" table includes all features and has no missing descriptions
  - [ ] Make sure the "Prior Models" section has a pointer to last year's model
  - [ ] Update the "Getting Data" section with links to this year's final data
  - [ ] Fetch the final data used to train the model using DVC
    - [ ] Verify each file's hash against the hash recorded in `model.metadata`
    - [ ] Upload the data to the CCAO's public S3 bucket
    - [ ] Make each file in the S3 bucket public using an ACL
    - [ ] Create a link for each file under the appropriate year in the README
    - [ ] Update the [`model.training_data`](https://github.com/ccao-data/data-architecture/blob/master/dbt/models/model/model.training_data.py) incremental model by running `dbt run --select model.training_data`
- [ ] Take a pass through the condo model README to make sure it's up to date
  - [ ] Double-check that the "Features Used" table includes all features and has no missing descriptions
  - [ ] Make sure the "Prior Models" section has a pointer to last year's model
  - [ ] Fetch the final data used to train the model using DVC (same sub-steps as res)
- [ ] Merge the annual feature branch for the res model into main, then [tag the commit](https://github.com/ccao-data/model-res-avm/tags)
- [ ] Once the annual res model feature branch is merged into main, re-knit README for the condo model so that the column identifying features that are unique to the condo model in the "Features Used" table is correct
- [ ] Merge the annual condo model feature branch into main, then tag the commit to match the res model
