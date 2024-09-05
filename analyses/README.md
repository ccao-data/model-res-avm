The new-feature-template is a feature selection tool which provides insight into if new feature provides an added value to the model.

To complete this report, execute the following steps:

Workflow:

1\. Identify if there is a model run which utilizes all features except for the new feature that you plan on adding.

2\. If there is comparable run, skip to step 5.

3\. Update the params with the added variable for the new feature.

4\. Run dvc ingest unfreeze in terminal.

5\. Run the ingest stage with dvc repro -f ingest in terminal.

6\. Run the model through github actions with SHAP values, upload to S3 and cross validation enabled.

7\. Create a new folder with the new feature and an ascending numeric value.

8\. Update the params of new-feature-template with the correct run_id's for the comparison run and the new run.

9\. Run the report and review the results.

10\. Write a summary of the results in the README.md file of the new feature folder.
