#' Get a scheduled BART trip
#'
#' Find the last trip that arrives by a deadline and its adult Clipper fare.
#' Times use the America/Los_Angeles time zone.
#' When two valid station codes are identical, return a zero-minute,
#' zero-fare row at the deadline. This means no train journey and no entry
#' through a fare gate, rather than an actual same-station excursion fare.
#'
#' @param orig One nonmissing origin station abbreviation, as a string.
#' @param dest One nonmissing destination station abbreviation, as a string.
#'   Defaults to Downtown Berkeley, DBRK.
#' @param arrive_by Arrival deadline, such as "8:45am".
#' @param date Trip date in month/day/year format. Defaults to 11/18/2026.
#' @param key BART API key. Defaults to BART's shared public key.
#' @returns A one-row tibble with orig, dest, depart, arrive, minutes, fare,
#'   and transfers. Depart and arrive are Pacific date-times, minutes is
#'   numeric, fare is the adult one-way Clipper fare in US dollars, and
#'   transfers is the number of train changes.
#' @examples
#' bart_trip("MLBR")
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @importFrom lubridate mdy_hm
#' @importFrom tibble tibble
#' @export
bart_trip <- function(orig, dest = "DBRK", arrive_by = "8:45am",
                      date = "11/18/2026", key = "MW9S-E7SL-26DU-VV8V") {
  if (!is.character(orig) || length(orig) != 1 || any(is.na(orig)) || orig == "") {
    stop("orig must be a single nonmissing station string")
  }
  if (!is.character(dest) || length(dest) != 1 || any(is.na(dest)) || dest == "") {
    stop("dest must be a single nonmissing station string")
  }

  # BART refuses a schedule request whose origin equals its destination.
  if (orig == dest) {
    stations <- bart_stations(key)
    if (!(orig %in% stations$abbr)) {
      stop("Unknown station abbreviation")
    }
    deadline <- mdy_hm(paste(date, arrive_by), tz = "America/Los_Angeles")
    return(tibble(
      orig = orig, dest = dest, depart = deadline, arrive = deadline,
      minutes = 0, fare = 0, transfers = 0
    ))
  }

  res <- request("https://api.bart.gov/api/sched.aspx") |>
    req_url_query(
      cmd = "arrive", orig = orig, dest = dest, date = date,
      time = arrive_by, b = 0, a = 0, key = key, json = "y"
    ) |>
    req_perform()

  body <- resp_body_json(res, simplifyVector = TRUE)
  trip <- body$root$schedule$request$trip
  if (is.null(trip)) {
    stop("BART returned no matching trip")
  }

  # These are XML attribute names retained in the JSON response.
  fares <- trip$fares$fare
  adult_fare <- fares[fares$`@class` == "clipper", ]
  if (nrow(adult_fare) != 1) {
    stop("BART returned no unique adult Clipper fare")
  }

  tibble(
    orig = orig,
    dest = dest,
    depart = mdy_hm(
      paste(trip$`@origTimeDate`, trip$`@origTimeMin`),
      tz = "America/Los_Angeles"
    ),
    arrive = mdy_hm(
      paste(trip$`@destTimeDate`, trip$`@destTimeMin`),
      tz = "America/Los_Angeles"
    ),
    minutes = as.numeric(trip$`@tripTime`),
    fare = as.numeric(adult_fare$`@amount`),
    transfers = nrow(trip$leg) - 1
  )
}
