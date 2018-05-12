#' Rasch model response generating function
#'
#' Creates responses based on the Rasch model.
#'
#' @param theta is a vector of scored responses.
#' @param diff is a vector of the maximum of the scored responses.
#' @param mscale is a vector indicating the scale for each item. The scale ID must be the row ID in 'scale_cuts'.
#' @param scale_group is a list of the cuts/thresholds for each scale type. Should be ordered to ID with 'scale_id'.
#' @keywords response generation simulation mcmc
#' @export
#' @examples
#' set.seed(1010)

respgen_fun = function(theta, diff, mscale, scale_group){

  # theta = theta #person ability
  # diff = iparams$ML[i,"Diff"] #item difficulty
  # mscale = Scales[[iparams$ML[i,"Scale"]]] #scales
  # scale_group = iparams$ML[i,"Scale"]
  #
  # lapply(mscale, function(x) append(0, x) )

  #INITIAL ITEM DIFFICULTIES (ALL LL ITEMS)
  bb = diff

  #TEMP LIST
  temp_resp = list()

  if(scale_group == 1){
    #INITIAL STEP PARAMTERS (STEP PARAMETERS FOR LL ITEMS)
    cc1 = mscale[1]
    cc2 = mscale[2]

    #RATING SCALE MODEL PARTS (LL)
    t1 = exp(theta-bb-cc1)
    t2 = exp(2*theta-2*bb-cc1-cc2)

    #STEP PARAMETERS 2 (LL)
    pp0 = (1/(1+t1+t2))
    pp1 = (t1/(1+t1+t2))

    #STEP PARAMETERS 3 (LL)
    sp0 = pp0
    sp1 = pp0+pp1

    #DRAW RANDOM VALUE
    rini = runif(1)

    #COMPARE RANDOM VALUE WITH STEPS, GENERATE DATA FOR LL ITEM
    if(rini<sp0)              {temp_resp = 0}
    if((sp0<rini)&&(rini<sp1)){temp_resp = 1}
    if(sp1<rini              ){temp_resp = 2}

    return(temp_resp)

  }

  if(scale_group == 2){
    #INITIAL STEP PARAMTERS (STEP PARAMETERS FOR ML ITEMS)
    cc1 = mscale[1]
    cc2 = mscale[2]
    cc3 = mscale[3]
    cc4 = mscale[4]

    #STEP PARAMETERS 1 (ML)
    t1 = exp(theta-bb-cc1)
    t2 = exp(2*theta-2*bb-cc1-cc2)
    t3 = exp(3*theta-3*bb-cc1-cc2-cc3)
    t4 = exp(4*theta-4*bb-cc1-cc2-cc3-cc4)

    #STEP PARAMETERS 2 (ML)
    pp0 = (1/ (1+t1+t2+t3+t4))
    pp1 = (t1/(1+t1+t2+t3+t4))
    pp2 = (t2/(1+t1+t2+t3+t4))
    pp3 = (t3/(1+t1+t2+t3+t4))

    #STEP PARAMETERS 3 (ML)
    sp0 = pp0
    sp1 = pp0+pp1
    sp2 = pp0+pp1+pp2
    sp3 = pp0+pp1+pp2+pp3

    #DRAW RANDOM VALUE
    rini = runif(1)

    #COMPARE RANDOM VALUE WITH STEPS, GENERATE DATA FOR ML ITEM
    if(rini<sp0)              {temp_resp = 0}
    if((sp0<rini)&&(rini<sp1)){temp_resp = 1}
    if((sp1<rini)&&(rini<sp2)){temp_resp = 2}
    if((sp2<rini)&&(rini<sp3)){temp_resp = 3}
    if( sp3<rini             ){temp_resp = 4}

    return(temp_resp)

  }

  if(scale_group == "3" | scale_group == "4" ){
    #INITIAL STEP PARAMTERS (STEP PARAMETERS FOR MH AND HH ITEMS)
    cc1 = mscale[1]
    cc2 = mscale[2]
    cc3 = mscale[3]
    cc4 = mscale[4]
    cc5 = mscale[5]

    #STEP PARAMETERS 1 (ML/HH)
    t1 = exp(theta-bb-cc1)
    t2 = exp(2*theta-2*bb-cc1-cc2)
    t3 = exp(3*theta-3*bb-cc1-cc2-cc3)
    t4 = exp(4*theta-4*bb-cc1-cc2-cc3-cc4)
    t5 = exp(5*theta-5*bb-cc1-cc2-cc3-cc4-cc5)

    #STEP PARAMETERS 2 (ML/HH)
    pp0 = (1/ (1+t1+t2+t3+t4+t5))
    pp1 = (t1/(1+t1+t2+t3+t4+t5))
    pp2 = (t2/(1+t1+t2+t3+t4+t5))
    pp3 = (t3/(1+t1+t2+t3+t4+t5))
    pp4 = (t4/(1+t1+t2+t3+t4+t5))

    #STEP PARAMETERS 3 (ML/HH)
    sp0 = pp0
    sp1 = pp0+pp1
    sp2 = pp0+pp1+pp2
    sp3 = pp0+pp1+pp2+pp3
    sp4 = pp0+pp1+pp2+pp3+pp4

    #DRAW RANDOM VALUE
    rini = runif(1)

    #COMPARE RANDOM VALUE WITH STEPS, GENERATE DATA FOR (ML/HH) ITEM
    if(rini<sp0)              {temp_resp = 0}
    if((sp0<rini)&&(rini<sp1)){temp_resp = 1}
    if((sp1<rini)&&(rini<sp2)){temp_resp = 2}
    if((sp2<rini)&&(rini<sp3)){temp_resp = 3}
    if((sp3<rini)&&(rini<sp4)){temp_resp = 4}
    if( sp4<rini             ){temp_resp = 5}

    return(temp_resp)

  }

}




