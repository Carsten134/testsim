
testthat::test_that("generates sample with normal density with k = 1", {
    set.seed(1)
    sample = factor(1)
    result = data.frame(value = rnorm(10), sample = rep(sample, 10))
    set.seed(1)
    expect_equal(
      generate_ksamples(
          1,
          c(rnorm),
          c(10)
          ),
      result
      )
  }
)


testthat::test_that("generates multiple samples", {
    set.seed(1)
    samples = factor(1:2)
    values_1 = rnorm(10)
    values_2 = rnorm(10)
    result = data.frame(value = c(values_1, values_2),
                        sample = c(rep(samples[1], 10), rep(samples[2],10)))
    set.seed(1)
    expect_equal(
      generate_ksamples(
          2,
          c(rnorm,rnorm),
          c(10,10)
      ),
      result)
    }
)

