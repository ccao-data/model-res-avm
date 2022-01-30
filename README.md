
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Under Construction

This is a placeholder document for the work-in-progress 2022 residential
assessment model.

To see the 2021 residential assessment model, visit the
[2021-assessment-year](https://gitlab.com/ccao-data-science---modeling/models/ccao_res_avm/-/tree/2021-assessment-year)
git tag.

## Athena Guide

| athena_table         | observation_unit               | primary_key                                                                  | notes                                                                                    |
|:---------------------|:-------------------------------|:-----------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------|
| metadata             | model run                      | year, run_id                                                                 | Information about each run                                                               |
| parameter_search     | model cv fold                  | year, run_id, configuration, fold_id                                         | Each row is the result from one fold assessment from one iteration                       |
| parameter_final      | model run                      | year, run_id                                                                 | Best parameters chosen for each run, as chosen by tune::select_best()                    |
| parameter_range      | parameter                      | year, run_id, parameter_name                                                 | CV search range per parameter per model run                                              |
| assessment           | improvement                    | year, run_id, meta_pin, meta_card_num                                        | Assessment results at the improvement level. Multi-card PINs will have more than one row |
| performance          | geography by class             | year, run_id, stage, geography_type, geography_id, by_class, class           | Peformance metrics broken out by class for different levels of geography                 |
| performance_quantile | geography by class by quantile | year, run_id, stage, geography_type, geography_id, by_class, class, quantile | Performance metrics by quantile within class and geography                               |
| shap                 | improvement                    | year, run_id, meta_pin, meta_card_num                                        | SHAP values for each feature of each improvement in the assessment data                  |
| timing               | model run                      | year, run_id                                                                 | Each row represents the timing for one run, where columns represent the stages           |
