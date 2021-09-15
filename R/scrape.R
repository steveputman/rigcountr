baseurl <- "https://rigcount.bakerhughes.com"
northamerpath <- "/na-rig-count"

get_link <- function(title) {
  landingpage <- rvest::read_html(paste0(baseurl, northamerpath))
  landingnodes <- landpage %>% rvest::html_nodes("a")
  landingtitles <- landingnodes %>% rvest::html_attr("title")
  matchnode <- match(title,
                       landingtitles)
  landinglinks <- landingnodes %>% rvest::html_attr("href")
  matchlink <- landinglinks[matchnode]

}

read_xlsblink <- function(link, sheet, range) {
  httr::GET(link, httr::write_disk(tf <- tempfile(fileext = exceltype)))
  sheet <- readxlsb::read_xlsb(tf, sheet, range)
}

get_all_narigs <- function() {
  link <- get_link("north_america_rotary_rig_count_jan_2000_-_current.xlsb")
  sheet <- read_excellink(link, )

}
