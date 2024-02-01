# evprof 1.1.1

* Improved the consistency of the provided example data sets


# evprof 1.1.0

* Energy GMM inside of `evmodel` also contain the `ratio` of every `charging_rate`


# evprof 1.0.0

* Added functions to save and read the model in JSON instead of RDS files
* Bug fix in the `evmodel` printing function
* Default `log` value of function `detect_outliers` set to `TRUE`
* Replace all deprecated `aes_string` functions by using `.data[[var]]`
* Remove unused functions
* Remove `days` parameter from function `divide_by_disconnection`
* Print a message with time-cycles' table in function `divide_by_timecycle`
* Complete tests
* Including California EV sessions when loading the package
* Include examples in all exported functions
* CRAN release


# evprof 0.1.0

* First release
