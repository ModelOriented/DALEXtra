DALEXtra 0.1.6
----------------------------------------------------------------
* `explain_scikitlearn()` rebuilded. Now class scikitlearn_model is a additional class for original Python object instead of another object.
* explainers created with `explain_scikitlearn()` have addidtional field `param_set`.
* yhat is now generic
* New examples in `README.md`

DALEXtra 0.1.5
----------------------------------------------------------------
* Now when you pass .yml that consist environment name that already exists one the machine, DALEXtra will not rise an error and contiune work with existing env
* If condaenv is NULL when creating_env on unixlike OS, DALEXtra will try to find conda on his own
* .onLoad function now checks if conda is installed. Alert is rised if not.

DALEXtra 0.1.4
----------------------------------------------------------------
* yhat.R created. Predict functions are sotred there in order to be more accesible
* explain_h2o() and explain_mlr() rebuilded. 

DALEXtra 0.1.3
----------------------------------------------------------------
* travis and codecov is now aviable available for DALEXtra
* tests added

DALEXtra 0.1.2
----------------------------------------------------------------
* scikitlearn_unix.yml file added to external data. This helps testing using linuxlike OS
* few minor updates in documentation
* message in `create_env()` changed

DALEXtra 0.1.1
----------------------------------------------------------------
* `explain_mlr()` function implemented
* `explain_h2o()` function implemented

DALEXtra 0.1
----------------------------------------------------------------
* DALEXtra package is now public
* `explain_scikitlearn()` function implemented
* `create_env()` function implemented

