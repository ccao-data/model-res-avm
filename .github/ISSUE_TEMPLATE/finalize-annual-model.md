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

- [ ] Make sure the final model has SHAPs and comps, and if not, run one more model with SHAPs and comps enabled
- [ ] Tag the final model as `final` using the [`tag-model-runs` workflow](https://github.com/ccao-data/model-res-avm/actions/workflows/tag-model-runs.yaml)
- [ ] Export desk review workbooks for the [res model](https://github.com/ccao-data/model-res-avm/) using the `export` pipeline stage and upload them to OneDrive
- [ ] Export iasWorld upload files for the res model using the `export` pipeline stage and upload them to OneDrive
- [ ] Follow the instructions in the [model API README](https://github.com/ccao-data/api-res-avm/) to add the new final res model and set it as the default
- [ ] Export API workbooks for the res model using the `api` pipeline stage and upload them to OneDrive
  * This often requires adding or removing features from the workbook template to match the request format for the annual model, so get started on it a few days ahead of time
- [ ] Upload performance report for the final res model to OneDrive
- [ ] Send res model email

### Condo model

- [ ] Make sure the final model has SHAPs, and if not, run one more model with them enabled
- [ ] Tag the final model as `final` using the [`tag-model-runs` workflow](https://github.com/ccao-data/model-res-avm/actions/workflows/tag-model-runs.yaml)
- [ ] Export desk review workbooks for [the condo model](https://github.com/ccao-data/model-condo-avm/) and upload them to OneDrive
- [ ] Export iasWorld upload files for the condo model and upload them to OneDrive
- [ ] Upload performance report for the final condo model to OneDrive
- [ ] Send condo model email

## Low priority

Low priority tasks must be complete eventually, but are not time-sensitive:

- [ ] Update the `model.final_model` seed in [`data-architecture`](https://github.com/ccao-data/data-architecture/) to include metadata for the res and condo models
- [ ] Make sure the `vars_dict` data in [`ccao`](https://github.com/ccao-data/ccao/) is up to date for new features
  * If you add any features to this dictionary that are used in either model, make sure to reknit the README for models that use the feature
- [ ] Take a pass through the res model README to make sure it's up to date
  - [ ] Update the "Major Changes from Previous Versions" section to include any major changes from this year
  - [ ] Double-check that the "Features Used" table includes all features and has no missing descriptions
  - [ ] Make sure the "Prior Models" section has a pointer to last year's model
- [ ] Take a pass through the condo model README to make sure it's up to date
  - [ ] Double-check that the "Features Used" table includes all features and has no missing descriptions
  - [ ] Make sure the "Prior Models" section has a pointer to last year's model
- [ ] Merge the annual feature branch for the res model into main, then [tag the commit](https://github.com/ccao-data/model-res-avm/tags)
- [ ] Once the annual res model feature branch is merged into main, re-knit README for the condo model so that the column identifying features that are unique to the condo model in the "Features Used" table is correct
- [ ] Merge the annual condo model feature branch into main, then tag the commit to match the res model
