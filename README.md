# dataversionr
Create and maintain time-versioned datasets using `arrow`.

## Status
The functions work against both local and s3 endpoints, and informal tests pass. 

## TODO

Formal tests
Roxygen
All that R package linting stuff

## R Package Development Workflow

We use the `devtools` package to facilitate package development. Rstudio provides nice integration with this package. 

We've flown quite close to this tutorial in our package development workflow - https://r-pkgs.org

To initialise the environment, clone this repo into Rstudio and load it as your working project. In your R console - 

1. `library(devtools)`
2. To load the package via source, `load_all()`
2. To create a function `use_r("function_name")`
3. To document the function, put your cursor inside the function source code and navigate to Code -> Insert Roxygen Skeleton. Be sure to populate the template correctly. For more details on this, refer to the roxygen2 documentation. 
4. After you've documented the function, run `document()` to create the right documentation files and update your `NAMESPACE` file.
5. Once you are done, `load_all()` again.
6. To check your package is well formed, run `rcmdcheck::rcmdcheck(repos = NULL)`. The `repos = NULL` is applicable if you are running in a firewalled environment like `tiro`.
7. To create a test, `use_test("fuction_name")`. Write your test and save it. 
8. To run a test code coverage report `library(covr); cov <- package_coverage()`
9. Check which lines are not covered by your tests `zero_coverage(cov)`
10. To build a PDF manual - `devtools::build_manual()`

