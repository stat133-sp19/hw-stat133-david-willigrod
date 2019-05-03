#A private function to test if an input prob is a valid probability value
check_prob <- function(prob){
  if (prob >= 0 && prob <= 1){
    return (TRUE)
  }
  else{
    stop("prob has to be a numeric value between 0 and 1")
  }
}

#A private function to test if the an input trialsis a valid value for number of trials
check_trials <- function(trials){
  if (trials > -1 && (trials == round(trials))){
    return (TRUE)
  }
  else{
    stop("invalid number of trials")
  }
}

#A private function to test an input successis a valid value for number of successes
check_success <- function(success, trials){
  if (any(success < 0)){
    stop("Success values must be positive integers")
  }
  if(any(success > trials)){
    stop("number of success must be less than or equal to trials")
  }
  else{
    return (TRUE)
  }
}

#A private function that calculates the mean
aux_mean <- function(trials, prob){
  return(trials*prob)
}

#A private function that calculates the variance
aux_variance <- function(trials, prob){
  return(trials*prob*(1-prob))
}

#A private function that calculates the mode
aux_mode <- function(trials, prob){
  m <- trials*prob + prob
  if (m == round(m)){
    return(c(m, m-1))
  }
  else{
    return(floor(m))
  }
}

#A private function that calculates the skewness
aux_skewness <- function(trials, prob){
  return((1-2*prob)/(sqrt(aux_variance(trials, prob))))
}

#A private function that calculates the kurtosis
aux_kurtosis <- function(trials, prob){
  return((1 - 6*prob*(1-prob))/aux_variance(trials,prob))
}

#' @title bin_choose
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials
#' @param k number of successes
#' @return the number of combinations in which k successes can occur in n trials
#' @export
#' @examples
#' # Calculates the number of combinations in which 0 through 2 successes can happen in 5 trials
#' bin_choose(n = 5, k = 0:2)
#'
bin_choose <- function(n, k){
  if (any(k > n)){
    stop("k cannot be greater than n")
  }
  else{
    return((factorial(n)/(factorial(k)*factorial(n-k))))
  }
}

#' @title bin_probability
#' @description calculates the probability of k successes in n trials with probability prob of success in each trial
#' @param n number of trials
#' @param k number of successes
#' @param prob probility of success in each trial
#' @return the probability of k successes in n trials with probability prob of success in each trial
#' @export
#' @examples
#' # Calculates probability of 2 successes in 5 trials with probability of success 50%
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
bin_probability <- function(success, trials, prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)

  return(bin_choose(trials,success)*(prob^success)*(1-prob)^(trials - success))
}

#' @title bin_distribution
#' @description calculates the binomial distribution with n trials and probability prob
#' @param n number of trials
#' @param prob probablity of success in each trial
#' @return a dataframe with two classes: "bindis" and "data.frame"
#' @export
#' @examples
#' # Calculates the distribution of probabilities of each possible number of successes in a dataframe and object with 5 trials and a probability of .5
#' bin_distribution(trials = 5, prob = 0.5)
#'
bin_distribution <- function(trials, prob){
  distribution <- data.frame(success = 0:trials, probability = bin_probability(0:trials, trials, prob))
  class(distribution) <- c("bindis")
  return (distribution)
}

#' @export
plot.bindis <- function(bindis){
  barplot(bindis$probability,
          xlab = "Successes", ylab = "Probability",
          names.arg = c(0:(length(bindis$success)-1)))
}

#' @title bin_cumulative
#' @description calculates the cumulative probability of 0 to each possible success value with probability prob of success for each trial n
#' @param n number of trials
#' @param prob probablity of success in each trial
#' @return a dataframe with two classes: "bincum" and "data.frame"
#' @export
#' @examples
#' # Calculates the cumulative probability of each possible number of successes in a dataframe and object with 5 trials and a probability of .5
#' bin_cumulative(trials = 5, prob = 0.5)
#'
bin_cumulative <- function(trials, prob){
  distribution <- data.frame(success = 0:trials, probability = bin_probability(0:trials, trials, prob), cumulative = cumsum(bin_probability(0:trials, trials, prob)))
  class(distribution) <- c("bincum")
  return (distribution)
}

#' @export
plot.bincum <- function(bincum){
  plot(bincum$success,bincum$cumulative, xlab = "Successes", ylab = "Cumulative Probability")
  lines(bincum$success,bincum$cumulative)
}

#' @title bin_variable
#' @description creates a binomial random variable object with stored values of trials n and probability prob
#' @param n number of trials
#' @param prob probablity of success in each trial
#' @return an object of class binvar represented by a list
#' @export
#' @examples
#' # Creates a binomial random variable object with 10 trials and a probability of success for each trial of .3
#' bin1 <- bin_variable(trials = 10, p = 0.3)
bin_variable <- function(trials, prob){

  check_trials(trials)
  check_prob(prob)

  bvar <- list(trials = trials,prob = prob)
  class(bvar) <- "binvar"
  return(bvar)
}

#' @export
print.binvar <- function(binvar){
  cat("  'Binomial variable'","\n\n" ,
      " Parameters","\n",
      "- number of trials:",binvar$trials,
      "\n",
      "- prob of success:",binvar$prob)
}

#' @export
summary.binvar <- function(binvar){
  summ <- list(trials = binvar$trials, prob = binvar$prob,
               mean = aux_mean(binvar$trials, binvar$prob),
               variance = aux_variance(binvar$trials, binvar$prob),
               mode = aux_mode(binvar$trials, binvar$prob),
               skewness = aux_skewness(binvar$trials, binvar$prob),
               kurtosis = aux_kurtosis(binvar$trials, binvar$prob))
  class(summ) <- "summary.binvar"
  return (summ)
}

#' @export
print.summary.binvar <- function(summary.binvar){
  cat("  'Binomial variable'","\n\n" ," Parameters","\n","- number of trials:",summary.binvar$trials,"\n","- prob of success:",summary.binvar$prob, "\n\n")
  cat("Measures","\n", "- mean:", summary.binvar$mean, "\n - variance:", summary.binvar$variance, "\n - mode:",
      summary.binvar$mode, "\n - skewness:", summary.binvar$skewness, "\n - kurtosis:", summary.binvar$kurtosis)
}

#' @title bin_mean
#' @description calculates the mean number of successes given a number of trials and probability of success for each trial
#' @param n number of trials
#' @param prob probablity of success in each trial
#' @return the mean of the given binomial distribution with n trials and probability prob of success for each trial
#' @export
#'
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title bin_variance
#' @description calculates the variance of the number of successes given a number of trials and probability of success for each trial
#' @param n number of trials
#' @param prob probablity of success in each trial
#' @return the variance of the given binomial distribution with n trials and probability prob of success for each trial
#' @export
#'
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title bin_mode
#' @description calculates the mode of the number of successes given a number of trials and probability of success for each trial
#' @param n number of trials
#' @param prob probablity of success in each trial
#' @return the mode of the given binomial distribution with n trials and probability prob of success for each trial
#' @export
#'
bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title bin_skewness
#' @description calculates the skewness of the number of successes given a number of trials and probability of success for each trial
#' @param n number of trials
#' @param prob probablity of success in each trial
#' @return the skewness of the given binomial distribution with n trials and probability prob of success for each trial
#' @export
#'
bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title bin_kurtosis
#' @description calculates the kurtosis of the number of successes given a number of trials and probability of success for each trial
#' @param n number of trials
#' @param prob probablity of success in each trial
#' @return the kurtosis of the given binomial distribution with n trials and probability prob of success for each trial
#' @export
#'
bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}

