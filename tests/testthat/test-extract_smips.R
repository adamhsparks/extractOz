test_that("extract_smips works", {
  locs <- list(
    "Merredin" = c(x = 118.28, y = -31.48),
    "Corrigin" = c(x = 117.87, y = -32.33),
    "Tamworth" = c(x = 150.84, y = -31.07)
  )

  w_all <- extract_smips(x = locs, day = "2023-09-23")
  expect_equal(unique(w_all$location),
               c("Corrigin", "Merredin", "Tamworth"))
  expect_s3_class(w_all, "data.frame")
  expect_length(w_all, 6)
})
