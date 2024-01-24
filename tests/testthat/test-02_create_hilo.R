
# Specification Errors ----------------------------------------------------

test_that("Missing required elements",{

  test1 <- qmod::dat1 %>%
    dplyr::select(price:smart_tech)

  expect_error(
    test2 <- qmod::dat1 %>%
      dplyr::select(price:smart_tech) %>%
      create_hilo()
  )

  expect_error(
    test2 <- create_hilo()
  )

  expect_error(
    test2 <- create_hilo(high=4, low=2)
  )

  expect_error(
    test2 <- create_hilo(test1, high=4)
  )

  expect_error(
    test2 <- create_hilo(test1, low=2)
  )
})



# Clean Run with No Errors ------------------------------------------------

test_that("Runs as expected",{

  test1 <- qmod::dat1 %>%
    dplyr::select(price:smart_tech)

  expect_no_error(
    test2 <- qmod::dat1 %>%
      dplyr::select(price:smart_tech) %>%
      create_hilo(., 4, 2)
  )

  expect_no_error(
    test3 <- test1 %>%
      performance_scores(., 4, 2)
  )

  expect_equal(names(test2), c("price_HI", "price_LO",
                               "billing_HI", "billing_LO",
                               "payment_options_HI", "payment_options_LO",
                               "customer_service_HI", "customer_service_LO",
                               "online_account_HI", "online_account_LO",
                               "registration_process_HI", "registration_process_LO",
                               "services_products_HI", "services_products_LO",
                               "smart_tech_HI", "smart_tech_LO"))

  expect_equal(test2 %>% dplyr::summarize_all(sum) %>% qpack::flip() %>% dplyr::pull(row_1),
               c(5187, 2007, 7955, 970, 8557, 616, 7722, 916, 8271, 791, 7832,
                 881, 3747, 1427, 2644, 1339))
})


