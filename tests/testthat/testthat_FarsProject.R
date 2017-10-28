context("Test FARS functions")
filename <- system.file("extdata", "accident_2013.csv.bz2", package = "FarsProject")
expect_that(fars_read(filename), is_a("tbl"))

