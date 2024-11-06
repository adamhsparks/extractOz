test_that("extract_smips works", {
  locs <- list(
    "Merredin" = c(x = 118.28, y = -31.48),
    "Corrigin" = c(x = 117.87, y = -32.33),
    "Tamworth" = c(x = 150.84, y = -31.07)
  )

  sm_all <- extract_smips(x = locs, day = "2023-09-23")
  expect_equal(unique(sm_all$location),
               c("Corrigin", "Merredin", "Tamworth"))
  expect_s3_class(sm_all, "data.frame")
  expect_length(sm_all, 6)
  expect_named(
    sm_all,
    c(
      "location",
      "longitude",
      "latitude",
      "smips_totalbucket_mm_20230923",
      "smips_longitude",
      "smips_latitude"
    )
  )
})
