# Note on this test
# If we change the summary statistics we will use in this function
# This test will fail because it compares against a saved, manually inspected summary dataframe
# So, if you want to modify the test, do the following -
# 1. library(devtools)
# 2. load_all()
# 3. iriss_summary <- summarise_df(iriss)
# 4. !!!! Manually inspect the summary and conform it is what you expect it to be
# 5. usethis::use_data(iriss, iriss_summary, internal = TRUE)
# 6. Run the test again

set.seed(100)
iriss <- iris %>%
  dplyr::mutate(char = "a",
         date = seq(as.POSIXct("2017-02-03"), length.out = nrow(iris), by = 1000))

n <- 50
iriss <- apply (iriss, 2, function(x) {x[sample( c(1:n), floor(n/10))] <- NA; x} ) %>%
  as.data.frame %>%
  dplyr::mutate(across(c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width), as.numeric),
         across(Species, as.factor),
         across(date, as.POSIXct))
iriss[6,6] <- " "
iriss[7,6] <- ""
iriss[8,6] <- "aa"


test_that("summarise_df works", {
  expect_equal(summarise_df(iriss), iriss_summary)
})
