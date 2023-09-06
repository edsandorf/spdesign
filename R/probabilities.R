#' Calculate the probabilities of the design
#'
#' Will take the design object and calculate the probabilities of each
#' alternative and choice tasks.
#'
#' Using Bayesian priors the average across the prior distribution will be used.
#'
#' Using the specific type of model, either the MNL or RPL probs will be
#' returned.
#'
#' @param x An 'spdesign' object.
#'
#' @return A matrix of probabilities for each alternative and choice task.
#'
#' @export
probabilities <- function(x) {
  pr_j <- switch(
    x$model,
    mnl = probabilities_mnl(x)
  )

  return(
    pr_j
  )
}

#' Calculate the MNL probabilities
#'
#' @inheritParams probabilities
#'
#' @return A matrix of probabilities for each alternative and choice task. With
#' Bayesian priors the return is the average probabilites over the prior
#' distribution
probabilities_mnl <- function(x) {
  db_env <- new.env()
  list2env(as.list(as.data.frame(x$design)), envir = db_env)

  pr_j <- lapply(x$prior_values, function(p) {
    list2env(as.list(p), envir = db_env)

    obs_utility <- lapply(update_utility(x$utility), function(v) eval(parse(text = v), envir = db_env))
    exp_utility <- lapply(obs_utility, exp)
    sum_utility <- Reduce("+", exp_utility)
    pr_j <- lapply(exp_utility, function(v) {
      v <- v / sum_utility
      v[is.na(v)] <- 0
      as.vector(v)
    })

    return(do.call(cbind, pr_j))
  })

  return(
    Reduce("+", pr_j) / length(pr_j)
  )
}
