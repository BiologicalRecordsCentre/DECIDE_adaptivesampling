# Converting SDM outputs and constraint layers into usable outputs for recorders on the DECIDE project.

## Overview

This repository is for developing R code takes SDM outputs from https://github.com/TMondain/DECIDE_WP1 and constraint layers from https://github.com/BiologicalRecordsCentre/DECIDE_constraintlayers to generate information that can be shown to the user through the DECIDE score layer, and specific suggestions (a.k.a. nudges). This comprises work package 2 of the DECIDE project:

![image](https://user-images.githubusercontent.com/17750766/133752632-5f2b8ea9-e9cb-4465-a63c-5202e5cb117d.png)

The R code in this repository is not the same R code as is deployed on the app. The app code is hosted in a GitLab repository and deployed on a kubernetes cluster. R code from this repository is 

## File structure

 * `/Scripts` - contains all the R code organised into `demos`, `modules`, `workflows`. `modules` contains R functions which are then used in the demos and workflows. There is quite a lot of overlap in the types of scripts in `demos` and `workflows`.

 * `/Schematics` - contains diagrams drawn with https://app.diagrams.net/ for the project. The main diagram types is in `/Schematics/Overview` and the files are numbered with the version number of the app. When new versions of the app are released with changes in the workflow then a new diagram should be created, rather than overwriting the old diagram.

 * `/renv` is to do with project-local R dependency management https://rstudio.github.io/renv/articles/renv.html

## Workflows

 * `create_decide_variation_weighted_time_since_records.R`
 * `create_map_function.R`
 * `data_products_outline.Rmd`
 * `decide_compare_locs.Rmd`
 * `decide_score_options.Rmd`
 * `diagram.R`
 * `find_accessible_areas.Rmd`
 * `getting_metadata.R`
 * `getting_metadata_rich.R`
 * `making_effort_layer.R`
 * `making_recommendations.Rmd`
 * `testing_all_species.R`

## Demos

 * `decide_score_compare.Rmd`
 * `decide_score_examples_for_recorders.Rmd`
 * `decide_score_rationale.Rmd`
 * `nudge_examples.R`
 * `providing_decide_nudges.Rmd`
 * `providing_nudges_rich.R`
 * `weight_decide_by_effort.Rmd`

## Modules

 * `convert_raster.R`
 * `count_records.R`
 * `crop_records.R`
 * `extract_metric.R`
 * `filter_accessible_locations.R`
 * `filter_distance.R`
 * `filter_phenology.R`
 * `filter_policy_relevance.R`
 * `filter_user_preference.R`
 * `load_gridnumbers.R`
 * `metadata_accessible.R`
 * `metadata_lcm.R`
 * `metadata_model_info.R`
 * `metadata_species.R`
 * `nudge_accessible.R`
 * `nudge_list.R`
 * `nudge_metadata.R`
 * `nudge_select.R`
 * `nudge_thin.R`
 * `recommend_agg_rank.R`
 * `recommend_metric.R`
 * `recommend_rank.R`
 * `rescale_metadata.R`
 * `smooth_recording.R`
