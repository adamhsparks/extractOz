#' Extract Soil Moisture Data from TERN's SMIPS Using Australian GPS Coordinates
#'
#' Extracts the soil moisture at the GPS points provided assuming that they are
#'  land-based coordinates in Australia from the Soil Moisture Integration and
#'  Prediction System \acronym{SMIPS} data set.
#'
#' @inheritParams extract_ae_zone
#' @param data A character vector of the data source to be queried, currently
#'   only \dQuote{smips}.
#' @param collection A character vector of the data collection to be queried,
#'  currenly only \dQuote{smips} is supported with the following collections:
#'  * SMindex
#'  * bucket1
#'  * bucket2
#'  * deepD
#'  * runoff
#'  * totalbucket
#'
#'  Defaults to \dQuote{totalbucket}. Multiple `collections` are supported,
#'  _e.g._, `collection = c("SMindex", "totalbucket")`.
#'
#' @param day A vector of date(s) to query, _e.g._, `day = "2017-12-31"` or
#'  `day = seq.Date(as.Date("2017-12-01"), as.Date("2017-12-31"), "days")`, both
#'  `Character` and `Date` classes are accepted.
#' @param api_key A `character` string containing your \acronym{API} key,
#'   a random string provided to you by \acronym{TERN}, for the request.
#'   Defaults to automatically detecting your key from your local .Renviron,
#'   .Rprofile or similar.  Alternatively, you may directly provide your key as
#'   a string here or use functionality like that from \CRANpkg{keyring}.  If
#'   nothing is provided, you will be prompted on how to set up your \R session
#'   so that it is auto-detected and a browser window will open at the
#'   \acronym{TERN} website for you to request a key.
#' @param max_tries An integer `Integer` with the number of times to retry a
#'   failed download before emitting an error message.  Defaults to 3.
#' @param initial_delay An `Integer` with the number of seconds to delay before
#'   retrying the download.  This increases as the tries increment.  Defaults to
#'   1.
#' @return A [data.table::data.table] with the provided \acronym{GPS}
#'  coordinates and the respective soil moisture data from \acronym{SMIPS}
#'
#' @references <https://portal.tern.org.au/metadata/TERN/15728dba-b49c-4da5-9073-13d8abe67d7chttps://portal.tern.org.au/metadata/TERN/d1995ee8-53f0-4a7d-91c2-ad5e4a23e5e0>
#'
#' @examplesIf interactive()
#' locs <- list(
#'   "Merredin" = c(x = 118.28, y = -31.48),
#'   "Corrigin" = c(x = 117.87, y = -32.33),
#'   "Tamworth" = c(x = 150.84, y = -31.07)
#' )
#'
#' extract_smips(x = locs, day = "2023-09-23")
#' @export

extract_smips <- function(x,
                          data = "smips",
                          collection = "totalbucket",
                          day,
                          api_key = nert::get_key(),
                          max_tries = 3,
                          initial_delay = 1) {

  .check_lonlat(x)

  tern <- nert::read_cog(data,
                         collection,
                         day,
                         api_key,
                         max_tries,
                         initial_delay)
  x <- .create_dt(x)
  data.table::setnames(x, old = c("x", "y"), new = c("user_x", "user_y"))
  points_sf <- sf::st_as_sf(x,
                            coords = c("user_x", "user_y"),
                            crs = terra::crs(tern))

  out <- data.table::as.data.table(
    terra::extract(x = tern, y = points_sf, xy = TRUE))
  data.table::setnames(out, old = c("x", "y"), new = c("smips_x", "smips_y"))

  out <- cbind(x, out)[, -c("ID")]

  return(out[])
}
