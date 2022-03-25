DALEXtra (development)
----------------------------------------------------------------
* bump the requirement for `DALEX` from v1.3 to v2.0.1 (dated 18 months ago: 2020-10-26)
* remove `randomForest` from suggest due to it enforcing R v4.1 (changed to `ranger`)
* fix `predict_surrogate()` when `new_observation` has too many variables (e.g. target outcome) 
* auto-convert the mlr3 learner-like objects with `mlr3::as_learner()` in `explain_mlr3()`

DALEXtra 2.1.1
----------------------------------------------------------------
* Fix CRAN results issues 

DALEXtra 2.1
----------------------------------------------------------------
* Fix errors coming from the new reticulate version
* Adjust explain functions to DALEX 2.1

DALEXtra 2.0
----------------------------------------------------------------
* `explain_tidymodels()` added as a support for tidymodels workflows.
* Removed aspect importance. It's available in triplot package https://cran.r-project.org/web/packages/triplot/index.html.
* `predict_surrogate()` function is added to provide easier interface of accessing lime/iml/localModel implementations of the LIME method.

DALEXtra 1.3.2
----------------------------------------------------------------
* Fixed cran check results

DALEXtra 1.3.1
----------------------------------------------------------------
* In added `yhat.GraphLearner()` and `model_info.GraphLearner()` to handle GraphLearners `mlr3` objects.
* New examples.

DALEXtra 1.3.0
----------------------------------------------------------------
* In `explain_h2o()` data parameter will bo converted to data.frame if H2OFrame object was passed.
* Aspect importance related functions set deprecated. Will be removed with next release.
* `explain_xgboost()` function added

DALEXtra 0.2.3
----------------------------------------------------------------
* DALEXtra now supports multiclass classification (accordingly to DALEX >= 1.3)
* `funnel_mesure()` and `training_test_comparison()` recognizes type of the task and applies proper loss_function
* `yhat.WrappedModel()` returns factor response if `predict.type` is not `prob`.

DALEXtra 0.2.2
----------------------------------------------------------------
* `explain_h2o()` now supports `model` as `H2OAutoML`

DALEXtra 0.2.1
----------------------------------------------------------------
* Removed h2o::init() from explain_h2o()
* Removed mljar support as mljar package is not available for R 3.6.2
* Ajusted to DALEX 1.0
* fixed `yhat.LearnerClassif()` returning wrong column of probabilities (PR #34, thanks Hubert!)

DALEXtra 0.2.0
----------------------------------------------------------------
* Rebuilded `plot.overall_comparison()` (I lack words that could describe Your greatness, Ania!).
* New README and DESCRIPTION. They are more accurate now.
* Small fixes to `funnel_measure()` that imporves it's stability.

DALEXtra 0.1.11
----------------------------------------------------------------
* New plot function for `funnel_measure()` objects. (Thanks Anna Kozak, You are awesome!).
* New tests for `funnel_measure()` and `plot.funnel_measure()` (Once again You are awesome, Ania!).

DALEXtra 0.1.10
----------------------------------------------------------------
* Added `aspect_importnace` from `ingredients`  ([#19](https://github.com/ModelOriented/ingredients/issues/19))
* Support for `mlr3` added
* DALEXtra now depends DALEX (0.4.9)

DALEXtra 0.1.9
----------------------------------------------------------------
* Ceiling replaced with round in `funnel_measure()`

DALEXtra 0.1.8
----------------------------------------------------------------
* `champion_challenger()`.
* `overall_comparison()` added with generic plot and print functions. 
* `training_test_comparison()` added with generic plot and print functions. 
* `funnel_measure()` added with generic plot and print functions. 
* test for h2o rebuilded.

DALEXtra 0.1.7
----------------------------------------------------------------
* `explain_keras()` added. 
* `explain_mljar()` added.
* documentation refreshed with links to functions.
* `explain_scikitlearn()` rebuilded. Some of the code was exported to inner functions (helper_functions.R).
* conda installation in `README.md`.
* `scikitlearn_unix.yml` file renamed to `testing_environment.yml`.

DALEXtra 0.1.6
----------------------------------------------------------------
* `explain_scikitlearn()` rebuilded. Now class scikitlearn_model is a additional class for original Python object instead of another object.
* explainers created with `explain_scikitlearn()` have addidtional field `param_set`.
* `yhat()` is now generic.
* New examples in `README.md`.

DALEXtra 0.1.5
----------------------------------------------------------------
* Now when you pass .yml that consist environment name that already exists one the machine, DALEXtra will not rise an error and contiune work with existing env.
* If condaenv is NULL when creating_env on unixlike OS, DALEXtra will try to find conda on his own.
* `on_attach()` function now checks if conda is installed. Alert is rised if not.

DALEXtra 0.1.4
----------------------------------------------------------------
* yhat.R created. Predict functions are stored there in order to be more accesible.
* `explain_h2o()` and `explain_mlr()` rebuilded. 

DALEXtra 0.1.3
----------------------------------------------------------------
* travis and codecov is now aviable available for DALEXtra.
* tests added.

DALEXtra 0.1.2
----------------------------------------------------------------
* `scikitlearn_unix.yml` file added to external data. This helps testing using linuxlike OS.
* few minor updates in the documentation.
* message in `create_env()` changed.

DALEXtra 0.1.1
----------------------------------------------------------------
* `explain_mlr()` function implemented.
* `explain_h2o()` function implemented.

DALEXtra 0.1
----------------------------------------------------------------
* DALEXtra package is now public.
* `explain_scikitlearn()` function implemented.
* `create_env()` function implemented.

