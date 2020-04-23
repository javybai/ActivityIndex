testthat::context("Output doesn't change for the Sample Data")

testthat::test_that("computeAI same answer regardless of input", {

  sampleGT3XPlus = ReadGT3XPlus(system.file("extdata","sample_GT3X+.csv.gz",
                                            package="ActivityIndex"))
  sampleTable = ReadTable(system.file("extdata", "sample_table.csv.gz",
                                      package="ActivityIndex"))

  testthat::expect_named(sampleGT3XPlus, c("SN", "StartTime", "StartDate",
                                           "Epoch", "DownloadTime",
                                           "DownloadDate",
                                           "Hertz", "Raw"))

  testthat::expect_named(sampleGT3XPlus$Raw, c("Date", "Time", "X", "Y", "Z"))

  s0 = sampleTable[1004700:1005600,]
  AI_sampleTable_x = computeActivityIndex(
    sampleTable,
    x_sigma0=s0, epoch=1, hertz=30)
  AI_sampleGT3XPlus_x = computeActivityIndex(
    sampleGT3XPlus, x_sigma0=s0,
    epoch=1,
    hertz=30)


  testthat::expect_equal(AI_sampleTable_x$AI,
                         AI_sampleGT3XPlus_x$AI)
  testthat::expect_error(
    computeActivityIndex(sampleGT3XPlus, x_sigma0=s0,
                         epoch = 1,
                         hertz = 100)
  )
  x = AI_sampleGT3XPlus_x$AI
  stats = c(mean(x), sd(x), median(x),
            mean(x[x > 0]), median(x[x > 0]))
  testthat::expect_equal(stats,
                         c(22.7449457740193, 67.1315848298163,
                           0.961727793460805, 36.1350372579237,
                           4.33613021194137))
})
