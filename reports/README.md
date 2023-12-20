# Quarto Diagnostic Documents

## Overview

This directory contains Quarto diagnostic documents for the residential model. These documents are built and saved automatically for each model run. They contain model performance statistics, diagnostic visualizations, debugging/quality control tables, and more.

These documents are intended to help refine, examine, and diagnose *individual models*. Cross-model comparison is performed via separate Tableau dashboards.

## Structure

The documents in this directory are **modularized** and separated by topic area. Documents prefixed with an underscore are considered **modules** and can be interpolated into other documents using the Quarto [include shortcode](https://quarto.org/docs/authoring/includes.html).

For example, `_sales.qmd` builds only plots and tables related to the input sales data. It uses the shortcode `{{< include _setup.qmd >}}` on the first line run `_setup.qmd`, which loads all necessary libraries and data dependencies.

Main documents do not have an underscore prefix. They combine metadata with multiple modules to generate a single, large report. For example, `performance.qmd` combines modules and renders to a single HTML document with a table of contents.

> [!NOTE]
Each `.qmd` file (even a module) can be rendered independently. Simply click the **Render** button in RStudio to knit only that document. Note however, that the HTML styling/metadata from `performance.qmd` will *not* apply to the rendered output (no table of contents, hidden code, etc.).

### Adding New Content

To add new content to a module of a main document, simply edit the relevant module. Your changes will be interpolated directly into the main document when it renders.

Be sure to load any new data you used in the module in `_setup.qmd`, and add any new libraries you used to the main `DESCRIPTION` file (see [Managing R Dependencies](https://github.com/ccao-data/model-res-avm?tab=readme-ov-file#managing-r-dependencies) in the main README).

### Adding a New Module

To add a new module, create a new Quarto document and prefix its filename with an underscore. Be sure to include the setup shortcode (`{{< include _setup.qmd >}}`) on the first line of the new file.

Once your module is finalized, check that it renders independently using the **Render** button at the top of the document. If it succeeds, include it in a main document using an [include shortcode](https://quarto.org/docs/authoring/includes.html).
