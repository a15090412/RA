#' Title
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
add <- function(a, b) {
  a + b
}

RA <- function(df , y , x , alpha = 0.95 , x0 = 0 ){

  mod <- lm(y ~ x,data = df)
  obj <- summary(mod)
  tb <- anova(mod)
  ci = confint(mod , level = alpha)
  e_x_con = predict(mod,data.frame(x = x0),interval = "confidence" ,level = alpha )
  e_x_pre = predict(mod,data.frame(x = x0),interval = "prediction" ,level = alpha )
  ##################################################################################
  bata1_h <- obj$coefficients[2,1]
  bata0_h <- obj$coefficients[1,1]
  var_bata1 <- obj$coefficients[2,2]
  var_bata0 <- obj$coefficients[1,2]

  SSreg <- tb$`Sum Sq`[1]
  SSres <- tb$`Sum Sq`[2]
  MSreg <- tb$`Mean Sq`[1]
  MSres <- tb$`Mean Sq`[2]
  F_value <- tb$`F value`[1]
  R_square <- SSreg/(SSres+SSreg)


  bata0_ci_U <- ci[1,2]
  bata0_ci_L <- ci[1,1]
  bata1_ci_U <- ci[2,2]
  bata1_ci_L <- ci[2,1]

  e_x_con_U <- e_x_con[3]
  e_x_con_L <- e_x_con[2]

  e_x_pre_U <- e_x_pre[3]
  e_x_pre_L <- e_x_pre[2]

  ##########################################################################
  new_tb <- data.frame(x = c("bata0_h","bata1_h","SSreg","SSres",
                            "MSreg" , "MSres","F_valus","R^2","bata0_ci_L",
                            "bata0_ci_U","bata1_ci_L","bata1_ci_U",
                            "E[y|x]_L of confidence","E[y|x]_U of confidence" ,
                            "E[y|x]_L of prediction","E[y|x]_U of prediction")
                      , y = c(bata0_h,bata1_h,SSreg,SSres,
                              MSreg,MSres,F_value,R_square,bata0_ci_L,
                              bata0_ci_U,bata1_ci_L,bata1_ci_U,
                              e_x_con_L,e_x_con_U,e_x_pre_L,e_x_pre_U))

  print(new_tb)
  if(obj$coefficients[1,4] > (1-alpha)){
    print("don't reject,bata0 = 0")
  }else{
    print("reject")
  }

  if(obj$coefficients[2,4] > (1-alpha)){
    print("don't reject,bata1 = 0")
  }else{
    print("reject")
  }

}
