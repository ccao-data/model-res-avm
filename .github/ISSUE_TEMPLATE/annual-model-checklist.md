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

- [ ] Run an initial sales val run in November. Use this run to create a task list of any issues that should be remedied before modeling season. Examples include:
  - [ ] Are the standard deviation bounds and geography areas sensible?
  - [ ] Are we excluding too many sales overall or in specific geographies?
If valuations has capacity for our ongoing sale review collaboration:
  - [ ] Reiterate importance of exact data input so that our [transformation script](https://github.com/ccao-data/data-architecture/blob/master/etl/scripts-ccao-data-warehouse-us-east-1/sale/sale-flag_review.R) functions properly
  - [ ] Determine which sales to send. In 2025 we opted for current triad, current year sales and we ranked the sales based on algorithmic sales vals' standard deviation numbers. Whichever sales had the highest SD distance from the mean (per township) were prioritized for review
  - [ ] Set a deadline for them to return whatever they can review to us
  - [ ] Ingest sale review labels into our data lake
- [ ] Make sure sales are as up to date as soon possible — ultimately will need sales through the end of the year. The final necessary sales typically arrive by mid January. If we don't get sales by this date, we will have trouble delivering the model on time.
  - Stakeholder/point of contact: Valuations Sale Review manager
- [ ] Skim PRs that have been merged after the last final model to get a sense of what changed in the pipeline. Be sure to check that these changes are reflected in reports, Desk Review and API Workbooks.

### Data Ingest / Refresh

The following [readme](https://github.com/ccao-data/data-architecture/blob/master/etl/README.md) provides guidance on how to run the ETL scripts.

- [ ] Run ETL scripts to get new feature data.
  - Key data arrival dates are **January** for Census data and **December** for Parcel spatial data.
- [ ] Complete the [Checklist](https://cookcounty.sharepoint.com/:x:/r/sites/Data-Assessor/_layouts/15/Doc.aspx?sourcedoc=%7BF4732426-8A8E-4C63-9211-89E12C9AB1E4%7D&file=2026%20Modeling%20Data%20Refresh.xlsx&action=default&mobileredirect=true) for ingest / refresh
  - After completing the data refresh, make sure to run the model feature report to ensure nothing unexpected has happened.
- [ ] Update the [Land rates](https://github.com/ccao-data/data-architecture/blob/5dcb6dc79b42ae1bc4a834bcd28ea851e525256f/etl/scripts-ccao-data-warehouse-us-east-1/ccao/ccao-land-land_nbhd_rate.R) ETL script to account for new land rates


**Internal data requests:**

> [!NOTE]
> Example:  [ResModeling_2026Deadlines.pptx](https://cookcounty.sharepoint.com/:p:/r/sites/Data-Assessor/Shared%20Documents/General/2026%20Initial%20Model%20Values/Planning%20Docs/ResModeling_2026Deadlines.pptx?d=w9ad2102166bd4b94a1c6615837346ba5&csf=1&web=1&e=rMPn6d)

- [ ] Meet with stakeholders and agree on delivery dates for any data that we need (sales, reviewed sales, land rates) and our key deliverable (the model output and associated email) as well as any intermittent deliverables (sales for review). – **November**
- [ ] Check in with Res-Val to see if there are any requested changes to the desk review workbook. – **November**
- [ ] IasWorld Sales – Director of Special Valuations – **January**
- [ ] Sale Review – Valuations Sale Review manager – **December**
- [ ] Land Rates – Chief Management Officer – **January**

### Condo specific data requests

September condo chars update:

  - [ ] Develop a list of condo unit chars that we suggest Data Integrity should update. Good units to update have missing data or data that is suspected to be incorrect (e.g., statistically low square footage). See: [enterprise-intelligence/issues/330](https://github.com/ccao-data/enterprise-intelligence/issues/330)
  - [ ] Ingest the spreadsheet returned by Data Integrity: [data-architecture/issues/920](https://github.com/ccao-data/data-architecture/issues/920)


## Res Model

- [ ] Create a milestone which groups together priorities for the upcoming modeling season. An example is [here](https://github.com/ccao-data/model-res-avm/milestone/2?closed=1).
- [ ] Update `params.yaml` in the following locations:
  - **Assessment**
    - `year`
    - `date`
    - `triad`
    - `data_year`
    - `working_year`
  - **Input**
    - `min_sale_year`
    - `max_sale_year`
  - **Model**
    - `seed` (doesn't matter, just as a legacy practice)
  - **Ratio_study**
    - `far_year`
    - `near_year`
- [ ] Run [model_feature report](https://github.com/ccao-data/model-res-avm/blob/master/reports/model_features/model_features.qmd) via GitHub Actions.
  - Pay attention to any changes to or from NA values and key features such as schools, location (neighborhood, x-y coordinates), and characteristics (square footage, number of rooms)
- [ ] Run the model and update DVC hashes to represent the newly ingested data. This must be done with a local run rather than GitHub Actions. This involves both pushing the DVC changes through `dvc push` as well as noting the changed values in the `params.yaml` file. See this pull request for updated [params](https://github.com/ccao-data/model-condo-avm/pull/125/changes).


## Condo Model

- [ ] Create a milestone which groups together priorities for the upcoming modeling season. An example is [here](https://github.com/ccao-data/model-condo-avm/milestone/1).
- [ ] Update `params.yaml` in the following locations:
  - **Assessment**
    - `year`
    - `date`
    - `triad`
    - `data_year`
    - `working_year`
  - **Input**
    - `min_sale_year`
    - `max_sale_year`
  - **Model**
    - `seed` (doesn't matter, just as a legacy practice)
  - **Ratio_study**
    - `far_year`
    - `near_year`
- [ ] Run [model_feature report](https://github.com/ccao-data/model-condo-avm/blob/master/reports/model_features/model_features.qmd) via GitHub Actions.
  - Pay attention to any changes to or from NA values and key features such as schools, location (neighborhood, x-y coordinates), and characteristics (square footage, number of rooms)
- [ ] Run the model and update DVC hashes to represent the newly ingested data. This must be done with a local run rather than GitHub Actions. This involves both pushing the DVC changes through `dvc push` as well as noting the changed values in the `params.yaml` file. See this pull request for updated [params](https://github.com/ccao-data/model-condo-avm/pull/125/changes).


---

# Post-Modeling Checklist

## High priority

High priority tasks must be completed before the model deadlines.

### Res model

- [ ] Make sure we have completed a model run with cross-validation (CV) enabled and used the hyperparameters it discovered for any final model run
- [ ] Make sure the final model has SHAPs and comps. If it doesn't, run one more model with SHAPs and comps enabled
- [ ] Tag the final model as `final` using the [`tag-model-runs` workflow](https://github.com/ccao-data/model-res-avm/actions/workflows/tag-model-runs.yaml)
- [ ] Export desk review workbooks for the [res model](https://github.com/ccao-data/model-res-avm/) using the `export` pipeline stage and upload them to OneDrive
  - [ ] Have everyone on the team take a few minutes to look through the workbooks to confirm they work and are formatted as expected, including the pivot tables
- [ ] Export iasWorld upload files for the res model using the `export` pipeline stage and upload them to OneDrive
- [ ] Follow the instructions in the [model API README](https://github.com/ccao-data/api-res-avm/) to add the new final res model and set it as the default
- [ ] Export API workbooks for the res model using the `api` pipeline stage and upload them to OneDrive
  - This often requires adding or removing features from the workbook template to match the request format for the annual model, so get started on it a few days ahead of time
  - [ ] Have everyone on the team take a few minutes to look through the workbooks to confirm they work and are formatted as expected - data validation for columns should match with the expected values of the column's associated feature. Confirm the workbooks call the expected model.
- [ ] Upload the performance report for the final res model to OneDrive
- [ ] Make sure the attached deliverables have been thoroughly reviewed and send the res model email with the subject line `$YEAR Initial Model Values (Residential)`

### Condo model

- [ ] Make sure we have completed a model run with cross-validation (CV) enabled and used the hyperparameters it discovered for any final model run
- [ ] Make sure the final model has SHAPs. If it doesn't, run one more model with them enabled
- [ ] Tag the final model as `final` using the [`tag-model-runs` workflow](https://github.com/ccao-data/model-res-avm/actions/workflows/tag-model-runs.yaml)
- [ ] Export desk review workbooks for [the condo model](https://github.com/ccao-data/model-condo-avm/) and upload them to OneDrive
  - [ ] Have everyone on the team take a few minutes to look through the workbooks to confirm they work and are formatted as expected, including the pivot tables
- [ ] Export iasWorld upload files for the condo model and upload them to OneDrive
- [ ] Upload performance report for the final condo model to OneDrive
- [ ] Make sure the attached deliverables have been thoroughly reviewed and send condo model email with the subject line `$YEAR Initial Model Values (Condos)`


## Low priority

Low priority tasks must be complete eventually, but are not time-sensitive:

- [ ] Update the `model.final_model` seed in [`data-architecture`](https://github.com/ccao-data/data-architecture/) to include metadata for the res and condo models
- [ ] Update the `vars.data_test_model_current_assessment_year` variable in the `dbt_project.yml` config file in [`data-architecture`](https://github.com/ccao-data/data-architecture/) to increment the assessment year
    - Before incrementing this value, make sure that the weekly `test-dbt-models` data integrity test workflow in `data-architecture` has run at least once since this year's final models ran, since otherwise it's possible that the final model artifacts may be untested
- [ ] Update `params.yaml` with the hyperparameters discovered by the CV run for both models
- [ ] Make sure the `vars_dict` data in [`ccao`](https://github.com/ccao-data/ccao/) is up-to-date for new features
  - If you add any features to this dictionary that are used in either model, make sure to re-knit the README for models that use the feature
- [ ] Make sure [any new features are up on the open data portal](https://github.com/ccao-data/wiki/blob/master/How-To/Add-columns-to-an-existing-open-data-asset.md)
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
    - [ ] Update the [`model.training_data`](https://github.com/ccao-data/data-architecture/blob/master/dbt/models/model/model.training_data.py) incremental model by running the [`build-and-test-dbt` github workflow](https://github.com/ccao-data/data-architecture/actions/workflows/build_and_test_dbt.yaml) and specifying `model.training_data`
- [ ] Take a pass through the condo model README to make sure it's up to date
  - [ ] Double-check that the "Features Used" table includes all features and has no missing descriptions
  - [ ] Make sure the "Prior Models" section has a pointer to last year's model
  - [ ] Fetch the final data used to train the model using DVC (same sub-steps as res)
- [ ] Merge the annual feature branch for the res model into main, then [tag the commit](https://git-scm.com/book/en/v2/Git-Basics-Tagging) so that it shows up on [the tags page for the res model](https://github.com/ccao-data/model-res-avm/tags)
  - See the docs below for instructions on [tagging a final model commit](#tagging-a-final-model-commit)
- [ ] Once the annual res model feature branch is merged into main, re-knit the README for the condo model so that the column identifying features that are unique to the condo model in the "Features Used" table is correct
- [ ] Merge the annual condo model feature branch into main, then tag the commit to match the res model

# Appendix

This section contains some documentation that may be helpful for accomplishing the tasks described above. Click on a section title to expand the docs for that section.

<details>
<summary><h2>Tagging a final model commit</h2></summary>

There are a few different approaches to tagging a final model commit. The right approach for your situation will depend on whether you are tagging the year's final model for the first time or re-tagging an existing year; it will also depend on whether you are tagging the main branch or a feature branch.

### Tagging the main branch for the first time in an assessment year

If you are tagging a model commit for the first time in an assessment year, you can tag the latest commit on the main branch and push it to the remote:

```bash
# Make sure you've checked out the latest commit on the main branch
git checkout main
git pull origin main

# Tag the commit (replace <YEAR> with the current assessment year in this command)
git tag -a "<YEAR>-assessment-year" -m "Final model for <YEAR>"

# Push the tag to the remote (also replace <YEAR> here)
git push origin "<YEAR>-assessment-year"
```

### Re-tagging an assessment year using the main branch

If a tag already exists for a given assessment year, but you need to update that tag to point to a new commit on the main branch due to changes in the model for that year, you can use [the `--force` option](https://git-scm.com/docs/git-tag#Documentation/git-tag.txt--f) to update the existing tag:

```bash
# Make sure you've checked out the latest commit on the main branch
git checkout main
git pull origin main

# Re-tag the year to point to a new commit (replace <YEAR> with the current assessment year)
git tag --force -a "<YEAR>-assessment-year" -m "Updated final model for <YEAR>"

# Force-push the new tag to the remote (also replace <YEAR> here)
git push --force origin "<YEAR>-assessment-year"
```

### Re-tagging an assessment year using a feature branch

Sometimes we need to tag a new model commit for an assessment year that already has a tag, but the new commit can't be on the main branch, because the main branch has already incorporated changes that we don't want to backport to the existing assessment year. This has happened to us in the past when we noticed partway through the year that we missed some sales for that year's model, but we had already merged some breaking changes into the main branch that weren't appropriate for the prior assessment year.

In situations like this, you can create a long-lived feature branch off of the existing model tag, then selectively make changes to that branch to reflect the necessary changes for the assessment year:

```bash
# Check out the existing tag (replace <YEAR> with the current assessment year)
git checkout "<YEAR>-assessment-year"

# Branch off of the tag to create a long-lived feature branch (also replace <YEAR> here)
git checkout -b "<YEAR>-assessment-year-update"
```

Commit any necessary changes to the new feature branch, then tag it and push the tag:

```bash
# Make sure your long-lived feature branch is pushed to the remote (replace <YEAR> here)
git push origin "<YEAR>-assessment-year-update"

# Tag the new branch (also replace <YEAR> here)
git tag --force -a "<YEAR>-assessment-year" -m "Updated final model for <YEAR>"

# Force-push the new tag to the remote (also replace <YEAR> here)
git push --force origin "<YEAR>-assessment-year"
```

You should also [create a branch protection rule](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/managing-protected-branches/managing-a-branch-protection-rule#creating-a-branch-protection-rule) for the long-lived feature branch to make sure we don't accidentally delete it. Select "Restrict deletions" and "Block force pushes" for this rule, and point it at your new branch. For an example, see the branch protection rule for the `2026-assessment-year-update` branch in the [condo model rulesets](https://github.com/ccao-data/model-condo-avm/settings/rules). If you don't have permissions to create a branch protection rule, ask a repository owner to create the rule.
</details>
