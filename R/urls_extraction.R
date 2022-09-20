#' @title Given a year find the race resuslts.html
#' @param year int. The year you want to scrape
#' @return Character. A url.
#' @export
#'
#' @examples
#' # get_race_results_url(2019)
get_race_results_url <- function(year)
{
  # Given a year, retrieve the race-results.html file.
  url <- sprintf("https://www.formula1.com/en/results.html/%s/races.html", year)

  html_doc <- rvest::read_html(url)
  base_url <- "https://www.formula1.com"

  # This selector is suitable to scrape every
  # conceivable year on the https://www.formula1.com/en/results.html website.
  css_selector <-
    "body > div.site-wrapper > main > article > div > div.ResultArchiveContainer>
     div.resultsarchive-filter-container > div:nth-child(3) > ul > li:nth-child(2) > a"

  part_url <- html_doc %>%
    rvest::html_node(css = css_selector) %>%
    rvest::html_attr("href")

  # On this url, you can find all relevant
  # urls with statistics from the stated year.
  race_results <- paste(base_url, part_url, sep = "")
  return(race_results)
}


#' @title Scrape all urls that link to qualifying statistics.
#'
#' @param main_url url from formula1.com ending with '/qualifying.html'
#' @param base_url The base url. Default 'https://www.formula1.com'.
#'
#' @return character with urls.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' url <- 'https://www.formula1.com/en/results.html/2022/races/1114/great-britain/qualifying.html'
#' quali_urls <- find_qualifying_urls(url)
#' print(quali_urls)
find_qualifying_urls <- function(main_url, base_url = "https://www.formula1.com") {

  html_file <- rvest::read_html(main_url)

  # Scrape tags with the href attribute,
  # because those link to a website.
  urls <- rvest::html_nodes(html_file, "a") %>%
    rvest::html_attr("href")

  urls <- urls[stringr::str_detect(urls, "qualifying")]
  urls <- urls[!is.na(urls)]

  # Combine base and end url to complte full urls.
  qualifying_urls <- stringr::str_c(base_url, urls)
  return(unique(qualifying_urls))
}


#' @title Get the url with qualification results from a year.
#' @param year int. Year that you want to scrape.
#' @return
#' @export
get_qualification_urls <- function(year) {

  # First, get the main race results of a race year.
  race_results_url <- get_race_results_url(year)

  # Given the main race results page, from there we move
  # to the qualification results. To do this, we find
  # a url that contains the word 'qualifying' in it.
  qualifying_main <- find_qualifying_urls(race_results_url)

  if (length(qualifying_main) == 1){

    # Thereafter, when we arrive at the qualification results,
    # we again search for url's that have the word 'qualifying' into it.
    qualifying_urls <- find_qualifying_urls(qualifying_main)
  }
  # These url's link to the qualifying results of each race.
  return(qualifying_urls)
}


#' @title Extract the year from a url like 'https://www.formula1.com/en/results.html/2022/races/'
#' @param driver_name Character.
#' @return
#' @export
# Get the capital name (e.g. 'Max Verstappen VER' becomes 'VER' )
get_capital_name <- function(driver_name){
  return(base::sub(".* ", "", driver_name))
}


#' @title Extract the year from a url like 'https://www.formula1.com/en/results.html/2022/races/'
#' @param url Character.
#'
#' @return Character. Year.
#' @export
get_year_from_url <- function(url) {
  return(stringr::str_extract(url, "\\d{4}"))
}


#' @title Extract the circuit country from a url.
#' @param url Character.
#'
#' @return Character.
#' @export
#'
#' @examples
#' url <- 'https://www.formula1.com/en/results.html/2022/races/1114/great-britain/qualifying.html'
#' circuit_name <- find_circuit_in_url(url)
#' print(circuit_name)
find_circuit_in_url <- function(url) {

  # find all  '/' chars in your url
  # it is assumed that the circuit/country
  # is located between the 8th and 9th '/'.
  lookup <- stringr::str_locate_all(url, "/")[[1]]
  start_end <- lookup[c(8,9), 1]

  circuit_name <- stringr::str_sub(
    url,
    start = start_end[1] + 1,
    end = start_end[2] - 1
  )
  return(circuit_name)
}


#' @title Vectorised version of .
#' @param urls Character. url's
#'
#' @return
#' @export
find_circuits_in_urls_vec <- function(urls) {
  return(unname(sapply(urls, find_circuit_in_url)))
}
