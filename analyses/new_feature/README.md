# Quarto Analysis Documents

## Overview

This directory contains Quarto analysis documents for the residential model. These documents need to be manually triggered following a model run. At present, they are designed to test the implementation of new features. These `qmd` files are located in the `new_feature` folder. In that folder, there are three sub-folders, `categorical`, `continuous`, and `shared`. `Shared` contains relevant information for both categorical and continuous variables, most notably the ingest script.

## Structure

The documents in this directory are **modularized** and separated by topic area. Documents prefixed with an underscore are considered **modules** and can be interpolated into other documents using the Quarto [include shortcode](https://quarto.org/docs/authoring/includes.html).

For example, `categorical/categorial_shaps.qmd` produces charts and tables relating to the SHAP values of the newly added features. It uses the shortcode `{{< include ../_setup.qmd >}}` on the first line to run `_setup.qmd`, which loads all necessary libraries and data dependencies.

Main documents do not have an underscore prefix. They combine metadata with multiple modules to generate a single, large report. For example, `categorical/categorical_new_feature.qmd` combines modules and renders to a single HTML document with a table of contents.

> \[!NOTE\] Each `.qmd` file (even a module) can be rendered independently. Simply click the **Render** button in RStudio to knit only that document. Note however, that the HTML styling/metadata from `categorical/categorical_new_feature.qmd` will *not* apply to the rendered output (no table of contents, hidden code, etc.).

### Adding New Content

To add new content to a module of a main document, simply edit the relevant module. Your changes will be interpolated directly into the main document when it renders.

Be sure to load any new data you used in the module in `_shared_setup.qmd`, and add any new libraries you used to the main `DESCRIPTION` file (see [Managing R Dependencies](https://github.com/ccao-data/model-res-avm?tab=readme-ov-file#managing-r-dependencies) in the main README).

### Adding a New Module

To add a new module, create a new Quarto document and prefix its filename with an underscore. Be sure to include the setup shortcode (`{{< include ../_setup.qmd >}}`) on the first line of the new file.

Once your module is finalized, check that it renders independently using the **Render** button at the top of the document. If it succeeds, include it in a main document using an [include shortcode](https://quarto.org/docs/authoring/includes.html).

## New Feature Analysis:

The New Feature Analysis aim to answer the following:

-   What are the summary statistics for the added variable (mean, median, mode, etc.)?

-   Does the added feature correlate with existing variables?

-   Does the added feature improve model performance?

-   Do SHAP values demonstrate that the added value is an important added feature?

-   Are their spatial disparities for the feature and are there spatial changes in assessed values?

### New Feature Workflow

To complete this report, execute the following steps:

1.  Identify if there is a model run which utilizes all features except for the new feature that you plan on adding.

2.  If there is no comparable runs:

    -   Update `params.yaml` and the parms of with the added variable for the new feature.

    -   Run `dvc ingest unfreeze` in terminal.

    -   Run the ingest stage with `dvc repro -f ingest` in terminal.

3.  If you push the commit to github, you can run the model through github actions with SHAP values and upload to S3 enabled. COMPs and cross-validation are not required. Otherwise, modify `params.yml` to make sure SHAP values and Upload are enabled and run `dvc repro train -f` in a `tmux` terminal.

4.  Create a new folder with the new feature and an ascending numeric value (`feature_name_000x)`.

5.  Update the `params` of `new-feature-template` with the correct run_ids for the comparison run and the new run as well as the .

6.  Run the report, review the results, and move the html file into the corresponding folder .

7.  Write a summary of the results in the README.md file of the new feature folder.
