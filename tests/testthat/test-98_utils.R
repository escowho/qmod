
# version -----------------------------------------------------------------


test_that("version returns current version", {
  expect_equal(version(), packageVersion("qmod"))
})


# refresh -----------------------------------------------------------------

test_that("update works correctly using QMOD_TEST option", {
  withr:::local_envvar(
    QMOD_TEST = TRUE,
    .local_envir = parent.frame())

  expect_equal(qmod:::refresh(), "update_pack")
  expect_equal(qmod:::refresh(dev=TRUE), "update_dev")

})
