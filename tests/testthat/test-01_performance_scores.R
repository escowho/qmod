
# Specification Errors ----------------------------------------------------

test_that("Missing required elements",{
  test1 <- dplyr::select(qmod::raw1, price:smart_tech)

  expect_error(
    test2 <- performance_scores(high=4, low=2)
  )

  expect_error(
    test2 <- performance_scores(4, 2)
  )

  expect_error(
    test2 <- performance_scores(test1)
  )

  expect_error(
    test2 <- performance_scores(test1, high=4)
  )

  expect_error(
    test2 <- performance_scores(test1, low=2)
  )
})



# Clean Run with No Errors ------------------------------------------------

test_that("Missing required elements",{
  test1 <- dplyr::select(qmod::raw1, price:smart_tech)

  expect_no_error(
    test2 <- performance_scores(data=test1, high=4, low=2)
  )

  expect_no_error(
    test3 <- test1 %>%
      performance_scores(., 4, 2)
  )

  expect_equal(names(test2), c("Variable", "Negative", "Neutral", "Positive"))
  expect_equal(test2$Negative, c(.199, .0951, .0605, .0908, .0783, .0885, .169, .206),
              tolerance=.01)
  expect_equal(test2$Neutral, c(.287, .145, .121, .175, .133, .170, .386, .397),
               tolerance=.01)
  expect_equal(test3$Positive, c(.514, .760, .818, .734, .788, .741, .445, .407),
               tolerance=.01)
})


