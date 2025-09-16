---
name: Finalize annual model
about: Necessary steps to take to finalize an annual model
title: Finalize annual model
labels: ''
assignees: ''
---

# Checklist

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
