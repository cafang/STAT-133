#' Get BART stations
#'
#' Request station names, locations, and abbreviations from the BART API.
#'
#' @param key BART API key. Defaults to BART's shared public key.
#' @returns A tibble with one row per station and six columns: abbr, name,
#'   city, county, lat, and lon. Coordinates are numeric decimal degrees.
#' @examples
#' bart_stations()
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @importFrom tibble tibble
#' @export
bart_stations <- function(key = "MW9S-E7SL-26DU-VV8V") {
  res <- request("https://api.bart.gov/api/stn.aspx") |>
    req_url_query(cmd = "stns", key = key, json = "y") |>
    req_perform()

  body <- resp_body_json(res, simplifyVector = TRUE)
  stations <- body$root$stations$station

  tibble(
    abbr = stations$abbr,
    name = stations$name,
    city = stations$city,
    county = stations$county,
    lat = as.numeric(stations$gtfs_latitude),
    lon = as.numeric(stations$gtfs_longitude)
  )
}
