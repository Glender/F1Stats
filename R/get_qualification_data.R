#' @title Extract the table with qualifying stats from the website.
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


#' @title Scrape the qualification result statistics from a given year.
#' @param year int.
#'
#' @return
#' @export
#'
#' @examples
#' # get_qualification_stats(2021)
get_qualification_stats <- function(year){

  # Extract the data.
  quali_urls <- get_qualification_urls(year)
  data <- scrape_qualifying_stats(quali_urls)

  # Transform the data: clean and merge data.
  df <- rem_empty_elements(data)
  data_total <- lapply(df, transf_data)
  data_total <- do.call(rbind, data_total)

  return(data_total)
}
