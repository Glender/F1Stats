#' Scrape all urls that link to qualifying statistics.
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

#' Extract the table with qualifying stats from the website.
#'
#' @param url Character. URL from formula1.com that ends with
#' qualifying.html'. It is used to scrape
#'
#' @return
#' @export
#' @importFrom magrittr %>%
scrape_qualification <- function(url) {

  data <- rvest::read_html(url) %>%
    rvest::html_table()

  if(length(data) != 0){

    # Unlist the data and remove NA cols.
    data <- data[[1]]
    data <- data[, colSums(is.na(data)) < nrow(data)]
    data$Circuit <- find_circuit_in_url(url)
    data$Year <- get_year_from_url(url)

  }

  cat("Scraped the website:", url, "\n")
  return(data)
}


#' @title Vectorised version to scrape multiple urls at once.
#'
#' @param qualifying_urls Character. URL's scraped with
#' the fun `find_qualifying_urls()`
#'
#' @return
#' @export
#'
#' @examples
#' url <- 'https://www.formula1.com/en/results.html/2022/races/1114/great-britain/qualifying.html'
#' quali_urls <- find_qualifying_urls(url)
#' data <- scrape_qualifying_stats(quali_urls)
scrape_qualifying_stats <- function(qualifying_urls) {

  data <- lapply(qualifying_urls, scrape_qualification)
  names(data) <- find_circuits_in_urls_vec(qualifying_urls)

  return(data)
}

