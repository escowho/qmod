temp1 <- qmod::dat1 %>%
  dplyr::select(nps)

temp2 <- qmod::dat1 %>%
  dplyr::select(price:smart_tech) %>%
  qmod::create_hilo(., 4, 2)

mod1 <- dplyr::bind_cols(temp1, temp2)

usethis::use_data(mod1, overwrite=TRUE)
rm(temp1, temp2, mod1)
