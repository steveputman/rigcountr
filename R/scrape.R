baseurl <- "https://rigcount.bakerhughes.com"
northamerpath <- "/na-rig-count"
user_agent <- "https://github/com/steveputman/rigcountr"

get_link <- function(title) {
  landingpage <- rvest::read_html(paste0(baseurl, northamerpath))
  landingnodes <- landingpage %>% rvest::html_nodes("a")
  landingtitles <- landingnodes %>% rvest::html_attr("title")
  matchnode <- match(title,
                       landingtitles)
  landinglinks <- landingnodes %>% rvest::html_attr("href")
  matchlink <- landinglinks[matchnode]

}

get_excel <- function(link, exceltype = "xlsx") {
  httr::GET(link,
            httr::user_agent(user_agent),
            httr::write_disk(tf <- tempfile(
              fileext = paste0(".", exceltype)))
            )
  return(tf)
}

read_xlsblink <- function(tf, sheet, range) {
  if (!requireNamespace("readxlsb", quietly = TRUE)) {
    stop("readxlsb needed to parse .xlsb files. To use this function, please
         install readxlsb", call. = FALSE)
  }
  sheet <- readxlsb::read_xlsb(tf, sheet, range)
}


#' get_all_narigs
#'
#' @description Pulls historical rig counts (oil/gas splits and trajectory) from
#' Baker Hughes
#'
#' @return A data frame of historical rig counts
#'
#' @export

get_all_narigs <- function() {
  link <- get_link("north_america_rotary_rig_count_jan_2000_-_current.xlsb")
  tempxl <- get_excel(link, "xlsb")
  allnaog <- get_all_narigs_og(tempxl)
  allnatraj <- get_all_narigs_traj(tempxl)
  allrigs <- dplyr::full_join(allnaog, allnatraj, by = c("date", "total"))
}

get_all_narigs_og <- function(allnarigs) {
  weekrows <- as.integer((Sys.Date() - as.Date("1987-07-17"))/7)
  startrow <- 7
  ogrange <- paste0("A", startrow, ":G", weekrows + startrow + 5)
  allnaog <- read_xlsblink(allnarigs, "US Oil & Gas Split", ogrange)
  allnaog$Date <- as.Date(allnaog$Date, origin = "1899-12-30")
  names(allnaog) <- c("date", "oil", "gas", "misc", "total", "oilpct", "gaspct")
  return(allnaog)
}

get_all_narigs_traj <- function(allnarigs) {
  weekrows <- as.integer((Sys.Date() - as.Date("1987-07-17"))/7)
  startrow <- 6
  trajrange <- paste0("A", startrow, ":H", weekrows + startrow + 5)
  allnatraj <- read_xlsblink(allnarigs, "US Count by Trajectory", trajrange)
  allnatraj$Date <- as.Date(allnatraj$Date, origin = "1899-12-30")
  names(allnatraj) <- c("date", "dir", "horiz", "vert", "total",
                      "dirpct", "horizpct", "vertpct")
  return(allnatraj)
}



