
## To-do

### High Priority

* Fix bug with button listening in Shiny app
* Package testing script with pipeline
* Add custom training/testing splitting parameter to model generation
* Make scores\_ml.R more closely match ml\_test.R
* Allow users to change name of each generated model in Shiny app
* Allow users to upload models to the current model folder in Shiny app
* Allow users to delete models from the current model folder in Shiny app
* Add GPL software license to package
* Upload bwtool to gitlab
* Write more verbose documentation at top of scripts

### Medium Priority

* Add analysis functions for processed and unprocessed data in Shiny app
* Add ability to run original Weka classifier using Rweka package in Shiny app
* Improve text output while running the pipeline in Shiny app
* Add mouseover text to explain each option in Shiny app
* Allow users to change the model folder in Shiny app
* Add more detailed filtering options for data processing

### Long-term Goals

* Convert to R package

## Finished

* Finished basic versions of data processing and analyzing scripts
* Deprecated the clunky predict_mae.R in favor of process.R, analyze.R and generate.R
* Finished prototype of Shiny UI
* Finished hooking up Shiny UI to pipeline, albeit with bugs
* Finished generating model for comparison to Weka classifier

