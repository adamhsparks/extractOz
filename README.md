
# extractOz

Extract the GRDC agroecological zone, major soil order and weather data
from your GPS sampling points. Datasets for the GRDC agroecological
zones and a modified Digital Atlas of Australian Soils are included in
this package for ease of use. You may also use your own geospatial
vector format file to extract similiar information using the generic
function, `extract_area()`.

## Quick start

``` r
library(extractOz)
library(dplyr)
```

## Create locations in WA and NSW

``` r
locs <- data.frame(
  site = c("Merredin", "Corrigin", "Tamworth"),
  "x" = c(118.28, 117.87, 150.84),
  "y" = c(-31.48, -32.33, -31.07)
)
```

## Extract the GRDC AgroEcological Zones

See `?extract_ae_zone()` for more help on how to use this function.

``` r
z <- extract_ae_zone(x = locs, coords = c("x", "y"))
```

## Extract the soil order

See `?extract_soil_order()` for more help on how to use this function.

``` r
s <- extract_soil_order(x = locs, coords = c("x", "y"))
```

## Get Temperature, Rain and RH for these Locations in 2020

Using the previously created data frame, fetch weather data from SILO
for 2020. This is just an example, replace `MY_EMAIL_ADDRESS` with your
email address below. See `?cropgrowdays::get_multi_silodata()` from for
more help on how to use this function.

``` r
three_sites  <-
  get_multi_silodata(
    latitude = locs$y,
    longitude = locs$x,
    Sitename = locs$site,
    START = "20200101",
    FINISH = "20201231",
    email = "adam.sparks@dpird.wa.gov.au"
  )
```

## Join the Weather Data with AE Zone, Soil Order and Site Information

Now using `dplyr::left_join()`, create a single `data.frame()` of the
Site, GPS coordinates, agroecological zone and weather data.

``` r
library(dplyr)

left_join(z, three_sites, by = c("site" = "Sitename")) %>% 
  left_join(s)
```
