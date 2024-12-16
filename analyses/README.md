The analyses folder provides one-off assessments whenever new features are created. The purpose of it is to identify if adding them to the model adds useful information, providing

The produced reports aim to provide the following information.

-   Summary statistics for the added variable (mean, median, mode, etc.)

-   Does the added feature correlate with existing variables?

-   Does the added feature improve model performance?

-   Do SHAP values demonstrate that the added value is an important added feature?

-   Are their spatial disparities to the added feature and the changes in assessed values?

The new feature template has two folders, categorical and continuous representing two types of variables which can be added to the model. In general, these produce similar outputs, although there are some differences. For example, rather than identifying the mean of a variable, we can identify the percentage of the most common and second most common feature.

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
