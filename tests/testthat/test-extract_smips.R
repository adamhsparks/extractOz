test_that("extract_smips works", {
  skip_if_offline()
  skip_on_ci()
  locs <- list(
    "Merredin" = c(x = 118.28, y = -31.48),
    "Corrigin" = c(x = 117.87, y = -32.33),
    "Tamworth" = c(x = 150.84, y = -31.07)
  )

  sm_all <- extract_smips(x = locs, day = "2023-09-23")
  expect_equal(unique(sm_all$location),
               c("Corrigin", "Merredin", "Tamworth"))
  expect_s3_class(sm_all, "data.frame")
  expect_length(sm_all, 7)
  expect_named(
    sm_all,
    c(
      "location",
      "longitude",
      "latitude",
      "smips_totalbucket_mm",
      "smips_longitude",
      "smips_latitude",
      "date"
    )
  )
  expect_identical(lapply(sm_all, class),
    list(
      location = "character",
      longitude = "numeric",
      latitude = "numeric",
      smips_totalbucket_mm = "numeric",
      smips_longitude = "numeric",
      smips_latitude = "numeric",
      date = "Date"
    )
  )
})
