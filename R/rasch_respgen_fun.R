#' Rasch partial credit model response generating function
#'
#' Creates responses based on the Rasch partial credit model.
#'
#' @param pers_ability is a value equal to the personal ability measure
#' @param item_diff is a value equal to the item difficulty.
#' @param item_thrsh is a vector of the item thresholds. If dichotomous, item_thrsh will be a single value.
#' @keywords response generation simulation mcmc pcm partial credit model
#' @export
#' @examples
#' set.seed(1010)
#' ability = rnorm(1,0,1)  #simulated person ability
#' difficulty  = rnorm(1,0,1)  #simulated item difficulty
#'
#' a = rnorm(2, 0, 1)
#' b = -a
#' thresholds = c(a, b) #simulated threshold measures
#'
#' rasch_respgen_fun(ability, difficulty, thresholds)

rasch_respgen_fun <- function(pers_ability, # person ability measure
                              item_diff,    # item difficult
                              item_thrsh) { # item thresholds

  #REDEFINE ITEM DIFFICULTY
  bb = item_diff

  #ENUMERATE THE STEPS
  numb_steps = length(item_thrsh)

  tt_values = matrix(rep(NA, length(item_thrsh) * 1), length(item_thrsh), 1)
  pp_values = matrix(rep(NA, length(item_thrsh) * 1), length(item_thrsh), 1)
  sp_values = matrix(rep(NA, length(item_thrsh) * 1), length(item_thrsh), 1)

  #GENERATE VALUES FOR tt
  for (i in 1:(numb_steps)) {
    tt_values[i] = exp(i * pers_ability - i * bb - (sum(item_thrsh[1:i])))
  }

  #GENERATE VALUES FOR pp and sp
  for (i in 1:(numb_steps)) {
    if (i == 1) {
      pp_values[i] = 1 / sum(1, tt_values)
      sp_values[i] = sum(pp_values[1:i])
    } else {
      pp_values[i] = tt_values[i-1] / (sum(1, tt_values))
      sp_values[i] = sum(pp_values[1:i])
    }
  }

  #DRAW A RANDOM VALUE FROM A UNIFORM DISTRIBUTION
  rini = runif(1)

  #COMPARE RANDOM VALUE WITH sp VALUE, GENERATE RESPONSE BASED ON COMPARISON
  for (i in 1:(numb_steps)) {

    if (rini < sp_values[1]) {temp_resp = 0} else { #checks if value is less than lower bound

      if (sp_values[length(sp_values)] < rini) {temp_resp = i} else { #checks if value is greater than uppor bound

        if ((sp_values[i] < rini) && (rini < sp_values[i + 1])) {temp_resp = i; break} #checks if value is one of the inner bounds

      }

    }

  }

  return(temp_resp)

}
