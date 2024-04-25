
# Specification Errors ----------------------------------------------------

test_that("Missing required elements",{


  fit1 <- lm(qmod::mod1, formula=nps~.)

  expect_error(
    get_driver_coefs()
  )

  expect_error(
    get_driver_coefs(qmod::mod1)
  )

  expect_error(
    get_driver_coefs(fit1, type="oops")
  )

})



# Clean Run with No Errors ------------------------------------------------

test_that("lm runs as expected",{

  fit1 <- lm(qmod::mod1, formula=nps~.)

  expect_no_error(
    test1 <- get_driver_coefs(fit1)
  )

  expect_equal(names(test1), c("term", "estimate", "p.value", "dir_prob", "rsq", "p.mod"))

  expect_equal(test1$estimate, c(-22.838, 20.238, -43.88, 12.707, -13.054, 5.076,
                                 23.235, 32.877, -14.78, 16.65, 12.565, 11.300,
                                 -4.219, 4.375, -2.490, 9.578, -11.243), tolerance=.1)

  expect_equal(max(test1$rsq, na.rm=TRUE), 0.42, tolerance=.1)
  expect_equal(sum(test1$dir_prob), 2)
})

test_that("output print=TRUE",{
  fit1 <- lm(qmod::mod1, formula=nps~.)

  expect_no_error(
    test1 <- capture.output(get_driver_coefs(fit1, print=TRUE))
  )
  expect_equal(test1[6], " 3 price_LO                   -43.     0           0 NA       NA")
})

test_that("glm runs as expected",{

  withr:::local_envvar(
    QMOD_TEST = TRUE,
    .local_envir = parent.frame())

  mod2 <- qmod::mod1 %>%
        dplyr::mutate(nps = dplyr::case_when(nps==100 ~ 1,
                                      nps <100 ~ 0))
  fit2 <- glm(data=mod2 , formula=nps~., family="binomial")

  expect_no_error(
    test2 <- get_driver_coefs(fit2)
  )

  expect_equal(names(test2), c("term", "estimate", "or", "prob", "p.value", "dir_prob",
                               "rsq", "p.mod", "plus_10"))

  expect_equal(test2$estimate, c(-1.937, .798, -.903, .413, -.273, .147, .785,
                                 1.025, -.162, .488, .481, .371, -.138, .196, -.059,
                                 .487, -.205), tolerance=.1)

  expect_equal(max(test2$rsq, na.rm=TRUE), 999)
  expect_equal(sum(test2$dir_prob), 2)
  expect_equal(sum(test2$plus_10), .085, tolerance=.01)

})
