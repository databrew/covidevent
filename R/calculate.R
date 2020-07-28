#' Calculate
#' 
#' Calculate the likelihood of attendance of infectious person   
#' @param n The number of participants
#' @param incidence Weekly incidence per 100,000
#' @param prob_skip_sick Likelihood of skipping of a person who is infected
#' @param days_infectious Number of days of infectiousness of a person
#' @import dplyr
#' @return A dataframe
#' @export

calculate <- function(n = 500,
                      incidence = 30,
                      prob_skip_sick = 0,
                      days_infectious = 10){
  
  message('Simulating an event with ',
          n, ' participants .')
  
  # Calculate daily incidence
  daily_incidence <- incidence / 7
  
  # Calculate likelihood of being infectious on any given day
  any_day_infectious <- daily_incidence * days_infectious
  
  # Build sample population
  # Build the sample population
  generate_population <- function(){
    tibble(id = 1:100000,
                         infected = c(rep(TRUE, round(any_day_infectious)),
                                      rep(FALSE, 100000 - round(any_day_infectious))))
  }
  pop <- generate_population()
  
  # Adjust for non-attendance
  if(prob_skip_sick > 0){
    message('---Removing non-attenders')
    ind_infected <- which(pop$infected)
    n_infected <- length(ind_infected)
    n_removed <- round(n_infected * (prob_skip_sick / 100))
    pop <- pop[(n_removed+1):nrow(pop),]
  }
  
  # Go through 1000 times and get the concert population
  nt <- 1000
  out_list <- list()
  for(i in 1:nt){
    this_time <- i
    this_sample <- dplyr::sample_n(tbl = pop,
                                   size = n)
    n_infectious <- length(which(this_sample$infected))
    out_list[[i]] <- tibble(sim = i,
                            infectious = n_infectious)
  }  
  out <- bind_rows(out_list)  
  final <- out %>%
    group_by(infectious) %>%
    tally %>%
    mutate(p = n / 10)
  return(final)
}
