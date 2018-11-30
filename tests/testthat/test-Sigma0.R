context("Simple test for Sigma0")

testthat::test_that("Make sure Sigma0 giving same answer", {
  filename = system.file("extdata","sample_GT3X+.csv.gz",package="ActivityIndex")
  res = ReadGT3XPlus(filename)
  hertz = res$Hertz
  x = res$Raw[ 1:1000, c("Time", "X", "Y", "Z")]
  res = Sigma0(x, hertz = hertz)
  testthat::expect_equal(res, c(SD = 0.184321637135534))

  # tests for data.frame
  res_df = Sigma0(as.data.frame(x), hertz = hertz)
  testthat::expect_equal(res, res_df)
})
