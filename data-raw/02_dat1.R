dat1 <- qmod::raw1 %>%
  dplyr::mutate(nps = qpack::nps_score(ltr)) %>%
  dplyr::select(-ltr) %>%
  dplyr::relocate(nps, .after=response_id) %>%
  qpack::create_na() %>%
  dplyr::group_by(year, fuel_type, payment_type) %>%
  qpack::fix_na("mean") %>%
  select(-c(payment_type, fuel_type, year))

usethis::use_data(dat1, overwrite=TRUE)
rm(dat1)
