# Remove trailing white space in the drivers name.
trim_driver_name <- function(string) {
  return(stringr::str_squish(string))
}


# Get only the first and last name.
# Requires trimmed names; no fancy aesthetics.
get_first_last_name <- function(name){
  return(stringr::word(trim_driver_name(name), 1, 2))
}


# Convert str like '1:09.23' to double 69.23.
str_to_seconds <- function(string){

  # Split the time in two parts, minutes and seconds.
  l <- stringr::str_split(string, ":")
  l_num <- lapply(l, as.numeric)

  # Create empty container to store the results.
  time <- vector("double", length = length(l_num))
  idx <- 1
  for(iter in l_num){

    min <- iter[1] # Get minutes.
    sec <- iter[2] # And seconds.

    # Convert to a total time score in seconds.
    time[idx] <- (min*60) + sec
    idx <- idx + 1
  }
  return(time)
}


# Transform second vector to delta seconds.
sec_to_delta <- function(seconds) {

  min_t <- min(stats::na.omit(seconds))
  t_delta <- sapply(seconds, function(time)  time - min_t)

  return(t_delta)
}


# Calculate a driver's improvement relative to its
# previously lapped qualification time.
# Inspired by:  https://www.statology.org/r-mapply/
lap_improvement <- function(Q1, Q2){

  impr <- mapply(function(Q1, Q2) Q1 - Q2, Q1, Q2)
  return(impr)
}


# Remove all empty elements from a list.
rem_empty_elements <- function(l){

  out <- l[lapply(l, length) > 0]
  return(out)
}


#' @title Clean and transform the Qualification data
#' scraped from formula1.com.
#' @param data Data.frame
#'
#' @return
#' @export
transf_data <- function(data){

  df <- tibble::tibble(

    # Global information.
    Year = data$Year,
    Circuit = data$Circuit,
    Team = data$Car,
    Driver = get_first_last_name(data$Driver),
    No = data$No,
    Abbr = get_capital_name(data$Driver),
    Pos = data$Pos,

    # Time scores.
    Q1 = str_to_seconds(data$Q1),
    Q2 = str_to_seconds(data$Q2),
    Q3 = str_to_seconds(data$Q3),

    # Driver improvement.
    Q1_2 = lap_improvement(Q1, Q2),
    Q2_3 = lap_improvement(Q2, Q3),

    # Delta relative to quickest driver.
    Q1_d = sec_to_delta(Q1),
    Q2_d = sec_to_delta(Q2),
    Q3_d = sec_to_delta(Q3)
  )

  return(df)
}
