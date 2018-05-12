#' Rasch Anchored Maximum Likelihood Estimation (AMLE) Function with know item difficulties
#'
#' Estimates the abilities based on known item difficulties. More info here: https://www.rasch.org/rmt/rmt122q.htm
#'
#' @param response is a vector of scored responses.
#' @param response_max is a vector of the maximum of the scored responses.
#' @param scale_id is a vector indicating the scale for each item. The scale ID must be the row ID in 'scale_cuts'.
#' @param scale_cuts is a list of the cuts/thresholds for each scale type. Should be ordered to ID with 'scale_id'.
#' @param it_diffic is a vector of item difficulties.
#' @keywords Rasch AMLE estimation mle Muti-stage adaptive
#' @export
#' @examples
#' set.seed(1010)
#' #Simulate some response string
#' resp = c(1,1,2,2,3,3,4,4)
#'
#' #Simulate maximum responses
#' max_responses = c(2,2,3,3,3,3,4,4)
#'
#' #Identify the scale type for each item
#' scale_ID = c(1,1,2,2,2,2,3,3)
#'
#' #List of a made-up scale thresholds
#' final_scale = list("ScaleA" = c(-0.2,0.2),
#'                    "ScaleB" = c(-0.3, -0.2, 0.5),
#'                    "ScaleC" = c(-0.4, -0.1, 0.3, 0.2))
#'
#' #Simulate item difficulty parameters
#' item_difficulties = c(runif(8, min = -3, max = 3) )
#'
#' rasch_amlestimate(resp, max_responses, scale_ID, final_scale, item_difficulties)

amle_fun <- function(response,    # responses to items
                     response_max,# maximum response to items
                     scale_id,    # scale that the items takes
                     scale_cuts,  # item thresholds, however many in the model
                     it_diffic) { # item difficulties

  #OBTAIN MAX LENGTH FOR EACH ELEMENT IN LIST
  max_len = sapply(scale_cuts, length)

  #EXPAND EACH ELEMENT IN THE LIST AND ADD NA'S FOR EMPTY SPACE TO MAKE LIST ELEMENTS HAVE EQUAL LENGTH
  even_list = lapply(scale_cuts, `length<-`, max(max_len))

  #RBIND LIST OF ELEMNTS NOW WITH EVEN LENGTH
  bound_list = do.call(rbind, even_list)

  #RENAME ROWS OF THE DATAFRAME FOR NO ERRORS
  rownames(bound_list) = c(1:nrow(bound_list))

  #CONVERT TO DATA.FRAME (also add a col. of zeros for calculations)
  bound_list = as.data.frame(cbind(scale = c(1:nrow(bound_list)), rep(0,nrow(bound_list)), bound_list) )

  #MATCH scale_cuts IN ELEMENTS OF scale_id
  scale_id = data.frame(scale = scale_id )

  #ASSIGN scale_cuts AS THE MERGED scale_id AND bound_list
  scale_cuts = merge(x = scale_id, y = bound_list, by = "scale"  )

  r_min = 0
  #EMPTY PLACEHOLDERS FOR EXPECTATIONS, VARIANCES, AND NUMBER OF ITERATIONS TO BE COLLECTED
  exp_list = list()
  var_list = list()
  logit_scores = data.frame("item1" = 0)

  #FUNCTION USED TO OBTAIN THE PROBABILITY OF A RESPONSE
  get_prob = function(max_resp, temp_num, pers_meas, it_meas, it_thresh){
    #max_resp: maximum value for a response for the item
    #temp_num: temporary numerator
    #pers_meas: person measure
    #it_meas: item measure
    #it_thresh: item threshold measures

    for (j in 1:(max_resp + 1)) {
      temp_num[j] = exp(((j - 1) * (pers_meas - it_meas)) - sum(it_thresh[1:j]))
    }

    return(temp_num)
  }

  #MAIN LOOP TO ESTIMATE ABILITY PARAMETER "M"
  for (i in 1:length(response)) {

    #THE FIRST ITERATION WILL HAVE A DIFFERENT INITIAL M
    if (i == 1) {

      R = sum(response[1:i])
      r_max = sum(response_max[1:i])
      if (R == r_min) {R = R + 0.3}
      if (R == r_max) {R = R - 0.3}

      initial_ability = mean(it_diffic[1]) + log((R - r_min) / (r_max - R)) #initial item difficulty must be specified

      temp_numer = matrix(rep(NA, response_max[i]), 1)
      first_it_cuts = scale_cuts[i, (2:ncol(scale_cuts))]
      temp_numer2 = get_prob(response_max[i], temp_numer, initial_ability, it_diffic[i], first_it_cuts) #initial item diff and scale for this item **THE SCALE NEEDS TO BE THE FIRST ITEM SCALE
      temp_P = temp_numer2 / sum(temp_numer2)
      expected_R = sum(temp_P * seq(0, response_max[i]))
      var_final = sum(seq(0, response_max[i])^2 * temp_P) - (sum(seq(0, response_max[i]) * temp_P))^2
      new_ability = initial_ability + (R - expected_R) / max(2 * var_final, 1)

      iter = 0

      #ITERATIONS i>1 WILL TAKE ON THE ESTIMATE OF M FROM THE PREVIOUS ITERATION
    } else {

      R = sum(response[1:i])
      r_max = sum(response_max[1:i])
      if (R == r_min) {R = R + 0.3}
      if (R == r_max) {R = R - 0.3}

      initial_ability = new_ability

      #OBTAIN THE EXPECTATIONS AND VARIANCES FOR EACH ITEM UP TO i
      for (k in 1:i) {

        k_scale = scale_cuts[k, (2:ncol(scale_cuts))]
        scale_length = length( which( !is.na(scale_cuts[k, (2:ncol(scale_cuts))] ) ) )

        temp_numer = matrix(rep(NA, response[k]), 1)

        #CALCULATION OF EXPECTED VALUE TIMES j(THRESHOLD NUMBER FROM 0 TO response_max[k])
        exp_list[[k]] = (0: (scale_length - 1)) * ( ( get_prob(response_max[k], temp_numer, initial_ability, it_diffic[k], scale_cuts[i, (2:ncol(scale_cuts))]) ) / sum(get_prob(response_max[k], temp_numer, initial_ability, it_diffic[k], k_scale)) )

        #CALCULATION OF VARIANCE
        var_list[[k]] = (0: (scale_length - 1))^2 * ( ( get_prob(response_max[k], temp_numer, initial_ability, it_diffic[k], k_scale) )/sum(get_prob(response_max[k], temp_numer, initial_ability, it_diffic[k], k_scale)) )

      }

      #SUM UP THE EXPECTATIONS BY ITEM
      exp_summed = lapply(exp_list, sum)

      #SUM UP ALL EXPECATIONS
      expectation_R = Reduce("+", exp_summed )

      #OBTAIN THE VARIANCE
      var1 = lapply(var_list, sum )
      var2 = lapply(exp_summed, function(x) x^2)
      var_final = Reduce("+",mapply(function(g, h) (g[[1]] - h[[1]]), var1, var2) )

      #OBTAIN NEW M
      new_ability = initial_ability + (R - expectation_R) / max(var_final * 2, 1)

      #START A NEW COUNT FOR THE NUMBER OF ITERATIONS, BEGINNING FROM 1
      iter = 1

    }

    #DO THE FOLLOWING LOOP UNTIL THE CRITERION IS SATISFIED
    while (abs(new_ability - initial_ability) > 0.01) {

      #COUNT EACH ITERATION
      iter = iter + 1

      #SET THE INITIAL M
      initial_ability = new_ability

      #PLACEHOLDER FOR THE NUMBER OF NUMERATORS FOR CALCULATION OF EXPECTED VALUE
      temp_numer = matrix(rep(NA, response[i]), 1)

      #SAME LOOP AS BEFORE - COLLECTION OF DIFFERENT PARTS OF THE FINAL EXPECTATION
      for (k in 1:i) {

        k_scale = scale_cuts[k, (2:ncol(scale_cuts))]
        scale_length = length( which( !is.na(scale_cuts[k, (2:ncol(scale_cuts))] ) ) )

        temp_numer = matrix(rep(NA,response[k]), 1)

        exp_list[[k]] = (0: (scale_length - 1)) * ((get_prob(response_max[k], temp_numer, initial_ability, it_diffic[k], k_scale)) / sum(get_prob(response_max[k], temp_numer, initial_ability, it_diffic[k], k_scale)))

        var_list[[k]] = (0: (scale_length-1))^2 * ( ( get_prob(response_max[k], temp_numer, initial_ability, it_diffic[k], k_scale)) / sum(get_prob(response_max[k], temp_numer, initial_ability, it_diffic[k], k_scale)))

      }

      #ONCE AGAIN, COLLECT THE SUM OF THE EXPECTATIONS
      exp_summed = lapply(exp_list, sum)

      expectation_R = Reduce("+", exp_summed )

      #ONCE AGAIN COLLECT THE VARIANCE ESTIMATES
      var1 = lapply(var_list, sum)
      var2 = lapply(exp_summed, function(x) x^2)
      var_final = Reduce("+", mapply(function(g, h) (g[[1]] - h[[1]]), var1, var2) )

      #OBTAIN THE NEW ABILITY THAT WILL BE COMPARED TO INITIAL ABILITY
      new_ability = initial_ability + (R - expectation_R) / max(var_final * 2, 1)

    }

    #RECORD THE FINAL SCORE
    logit_scores[1, paste0("item", i)] = new_ability
    logit_scores[2, i] = 1 / sqrt(var_final)

  }

  #MAX NUMBER OF ITERATIONS
  return(logit_scores)

}
