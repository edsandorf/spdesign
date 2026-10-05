#' Build the candidate set
#'
#' Builds the candidate set one alternative at a time instead of from the full
#' factorial of all alternatives. This uses far less memory and avoids choice
#' tasks that are the same apart from the order of the alternatives.
#'
#' The profiles of each alternative are the full factorial of its attribute
#' levels. Exclusions that only refer to a single alternative are applied to its
#' profiles before the alternatives are combined. Alternatives are exchangeable
#' if their utility functions and profiles are identical apart from the name of
#' the alternative, e.g. two unlabelled alternatives. By default, each choice
#' task contains a set of different profiles for the exchangeable alternatives
#' only once, and the order of the exchangeable alternatives is randomised in
#' each row to ensure that no alternative systematically gets the same
#' profiles. Labelled alternatives are combined with all profiles of the other
#' alternatives, as in the full factorial. Finally, exclusions that refer to
#' more than one alternative are applied.
#'
#' Because the order of exchangeable alternatives is random, exclusions across
#' exchangeable alternatives must be specified in both directions, e.g. that
#' alt1 dominates alt2 and that alt2 dominates alt1.
#'
#' With `allow_reversed_pairs = TRUE` and no exclusions, the candidate set is
#' the full factorial without choice tasks where exchangeable alternatives have
#' the same profile. To create a candidate set with attribute levels that are
#' different from those in the utility functions, use \code{\link{expand.grid}}.
#'
#' @inheritParams generate_design
#' @param allow_reversed_pairs If TRUE, include the profiles of exchangeable
#' alternatives in every order, e.g. both A versus B and B versus A. This also
#' applies to sets of three or more exchangeable alternatives. The default is
#' FALSE.
#'
#' @return A data frame with the candidate set
#'
#' @examples
#' utility <- list(
#'   alt1 = "b_x1[0.1] * x1[1:3] + b_x2[-0.2] * x2[c(0, 1)]",
#'   alt2 = "b_x1      * x1      + b_x2       * x2"
#' )
#'
#' build_candidate_set(utility, exclusions = list("alt1_x1 == 1 & alt1_x2 == 0"))
#'
#' @export
build_candidate_set <- function(
  utility,
  exclusions = list(),
  allow_reversed_pairs = FALSE
) {
  x <- define_profiles(utility, exclusions)
  n <- vapply(x$groups, function(g) nrow(x$profiles[[g[1]]]), numeric(1))
  k <- lengths(x$groups)

  # Warn before building a very large candidate set with reversed pairs
  rows <- prod(vapply(
    seq_along(n),
    function(i) prod(n[i] - seq_len(k[i]) + 1),
    numeric(1)
  ))

  if (allow_reversed_pairs && rows > 1e6) {
    warning(
      "Allowing reversed pairs gives a candidate set with ",
      format(rows, big.mark = ","),
      " rows before the exclusions across alternatives are applied. Setting ",
      "allow_reversed_pairs = FALSE would reduce this to ",
      format(prod(choose(n, k)), big.mark = ","),
      " rows. A large candidate set may exhaust the memory of your computer.",
      call. = FALSE
    )
  }

  # Combine profile rows within groups, and all combinations between groups
  idx <- lapply(seq_along(n), function(i) {
    combine_profiles(n[i], k[i], allow_reversed_pairs)
  })

  grid <- expand.grid(
    lapply(idx, function(m) seq_len(nrow(m))),
    KEEP.OUT.ATTRS = FALSE
  )

  index <- do.call(
    cbind,
    lapply(seq_along(idx), function(i) {
      idx[[i]][grid[[i]], , drop = FALSE]
    })
  )

  colnames(index) <- unlist(x$groups)

  # Randomise the order of exchangeable alternatives in each row, so that no
  # alternative systematically gets the lower profile rows
  if (!allow_reversed_pairs) {
    for (g in x$groups[k > 1]) {
      random_order <- order(
        rep(seq_len(nrow(index)), length(g)),
        stats::runif(nrow(index) * length(g))
      )

      index[, g] <- matrix(
        as.vector(index[, g])[random_order],
        ncol = length(g),
        byrow = TRUE
      )
    }
  }

  candidate_set <- as.data.frame(do.call(
    c,
    lapply(names(x$profiles), function(j) {
      lapply(x$profiles[[j]], function(column) column[index[, j]])
    })
  ))

  return(
    exclude(candidate_set, x$across)
  )
}

#' Define the profiles of each alternative
#'
#' Defines the profiles of each alternative as the full factorial of its
#' attribute levels, with the exclusions that only refer to that alternative
#' applied, and finds the groups of exchangeable alternatives. See
#' \code{\link{build_candidate_set}}.
#'
#' @inheritParams build_candidate_set
#'
#' @return A list with the profiles of each alternative, the groups of
#' exchangeable alternatives and the exclusions across alternatives
define_profiles <- function(utility, exclusions = list()) {
  lvls <- expand_attribute_levels(utility)
  alts <- names(utility)

  # expand_attribute_levels() lists all attributes for each alternative in turn
  alt_of <- rep(alts, each = length(lvls) / length(alts))

  # Exclusions that refer to a single alternative are applied to its profiles
  refers_to <- lapply(exclusions, function(e) {
    unique(alt_of[str_detect(e, as_whole_word(names(lvls)))])
  })

  profiles <- lapply(stats::setNames(alts, alts), function(j) {
    exclude(
      expand.grid(lvls[alt_of == j], KEEP.OUT.ATTRS = FALSE),
      exclusions[vapply(refers_to, identical, logical(1), j)]
    )
  })

  # Alternatives are exchangeable if their utility functions and profiles are
  # identical apart from the name of the alternative
  utility_clean <- clean_utility(utility)
  key <- vapply(
    alts,
    function(j) {
      prefix <- paste0("\\b", j, "_")
      paste(
        str_remove_all(utility_clean[[j]], prefix),
        str_remove_all(paste(names(profiles[[j]]), collapse = " "), prefix),
        paste(unlist(profiles[[j]]), collapse = " ")
      )
    },
    character(1)
  )

  return(
    list(
      profiles = profiles,
      groups = unname(split(alts, factor(key, levels = unique(key)))),
      across = exclusions[lengths(refers_to) != 1]
    )
  )
}

#' Combine the profiles of a group of exchangeable alternatives
#'
#' @param n The number of profiles of each alternative in the group
#' @param k The number of alternatives in the group
#' @inheritParams build_candidate_set
#'
#' @return A matrix with one row per combination and one column per
#' alternative, giving the rows of the profiles to combine
combine_profiles <- function(n, k, allow_reversed_pairs) {
  if (k == 1) {
    return(matrix(seq_len(n)))
  }

  if (!allow_reversed_pairs) {
    return(t(utils::combn(n, k)))
  }

  # All orderings of distinct profiles
  m <- as.matrix(expand.grid(rep(list(seq_len(n)), k), KEEP.OUT.ATTRS = FALSE))

  distinct <- Reduce(
    `&`,
    lapply(utils::combn(k, 2, simplify = FALSE), function(p) {
      m[, p[1]] != m[, p[2]]
    })
  )

  return(
    m[distinct, , drop = FALSE]
  )
}
