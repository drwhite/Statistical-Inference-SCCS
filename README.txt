Project description
-------------------

For a description of the Standard Cross Cultural Sample see: http://en.wikipedia.org/wiki/Standard_Cross-Cultural_Sample. 

This R package is meant to facilitate statistical inference on the Standard Cross Cultural Sample.

Requirements
------------
  * R: http://www.r-project.org/
  * SCCS codebook (for consultation): http://intersci.ss.uci.edu/wiki/index.php/Standard_Cross-Cultural_Sample 

Getting Started
---------------

This short guide will walk you through the steps of installing the package, building a model, and fitting it.
* On linux type, ./install.sh; on windows type, ./install.bat
* Load R, type: library(sccs)
* In R, set the working directory by typing: setwd("<working dir>")
* In R, load an example model by typing: source("examples/src/create_model_value_children.R")
* In R, run two stage ordinary least squares by typing: source("examples/src/two_stage_ols_mi.R")
* Examine the summary statistics generated from the fitted model by inspecting the file: summary_model_value_children.csv




