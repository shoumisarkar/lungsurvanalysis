#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(survival)
library(DT)
library(dplyr)

shinyServer(function(input, output) {

  output$mytable = DT::renderDataTable({
    lung
  })
    
output$pointprev =  renderText({
       #point prevalence of death: no. of subjects who died at time t, divided by total population.
    pointprev <- (lung %>% filter(status==2 & time==input$pp_time) %>% nrow() )/nrow(lung)
    paste("The point prevalence of death among the lung cancer patients at time ",
          input$pp_time, " is ", round(pointprev,6),
          ".\nIt is the number of people dying at time ", input$pp_time,
          " (which is ", (lung %>% filter(status==2 & time==input$pp_time) %>% nrow()), "),
          divided by the total size of the population, ", nrow(lung), ".",sep="")
})

output$periodprev = renderText({
  #period prevalence of death: proportion of population dying in that time period.
  periodprev <- (lung %>% filter(status==2 & time>input$pp_rng[1] & time<=input$pp_rng[2]) %>% nrow() )/nrow(lung)
  
  if(input$pp_rng[1]==input$pp_rng[2])
  {
    paste("Try again with distinct time points!")
  }
  
  else
  {paste("The period prevalence of death among the lung cancer patients in the time interval (", input$pp_rng[1], ",
        ",input$pp_rng[2], "]"," is ", round(periodprev,6),
        ".\nIt is the number of people dying in the time interval (", input$pp_rng[1], ",",
        input$pp_rng[2], "], ",
        "(which is ", (lung %>% filter(status==2 & time==input$pp_time) %>% nrow()), "),
          divided by the total size of the population, ", nrow(lung), ".",sep="")
  }
  
})

output$incirateratio = renderText({
  #incidence rate ratio in an interval (female to male):
  #ratio of number of deaths in females/total female person-time divided by
  #ratio of number of deaths in males/total male person-time,
  #for that time interval.
  
  subsetdata = na.omit(lung %>% filter(time>input$pp_rng[1] & time<=input$pp_rng[2]))
  
  mf = subsetdata %>% filter(status==2 & sex==2) %>% nrow()
  Tf =subsetdata %>% filter(sex==2) %>% nrow()
  
  mm = subsetdata %>% filter(status==2 & sex==1) %>% nrow()
  Tm =subsetdata %>% filter(sex==1) %>% nrow()
  
  inciratef = round(mf/Tf, 6)
  inciratem = round(mm/Tm, 6)
  
  incirateratio = round(inciratef/inciratem, 6)
  ci_incirateratio = round(sort(incirateratio * exp( c(-1,1)*1.96*sqrt(1/mf + 1/mm))), 6) 

  if(input$pp_rng[1]==input$pp_rng[2])
  {
    paste("Try again with distinct time points!")
  }
  
  else if(Tm==0) #if in the given time interval, there are observations on only ONE gender (female): not meaningful to calculate the incidence rate ratio.
  {paste("For the chosen time period (", input$pp_rng[1], ", ", input$pp_rng[2], "], ",
         "the incidence rate of death among female lung cancer patients is ",
         inciratef, " and there are no male observations. ", 
         "As we do not have observations on both genders for this time interval, it is not meaningful to compute the incidence rate ratio.",
         " Try setting a wider time interval!\n", sep="")
  }
  else if(Tf==0) #if in the given time interval, there are observations on only ONE gender (male): not meaningful to calculate the incidence rate ratio.
  {paste("For the chosen time period (", input$pp_rng[1], ", ", input$pp_rng[2], "], ",
         "the incidence rate of death among male lung cancer patients is ",
         inciratem, " and there are no female observations. ", 
         "As we do not have observations on both genders for this time interval, it is not meaningful to compute the incidence rate ratio.",
         " Try setting a wider time interval!\n", sep="")
  }
  else{
        paste("For the chosen time period (", input$pp_rng[1], ", ", input$pp_rng[2], "], ", 
        "the incidence rate of death among female lung cancer patients is ",
         inciratef, " and that among male patients is ", inciratem, 
        ". The incidence rate ratio comparing females to males is ", incirateratio,
        ", with confidence interval (", ci_incirateratio[1], ", ",
        ci_incirateratio[2], ").\n",
        sep="")
  }
  
})
  
output$variableselectorAFT <- renderTable({
  if (length(input$variables) == 0) return(head(lung, n=8))
  lung %>% select(!!!unique(c("time", "status", input$variables))) %>% head(n=8)
}, rownames = TRUE) 
 
#to fit AFT model and get model summary:
output$AFTsummary <- renderPrint({
  
  subsetted = lung
  subsetted$sex = as.factor(subsetted$sex)
   
  if(length(input$variables)!=0)
  {  
    subsetted = subsetted %>% select(!!!unique(c("time", "status", input$variables)))
    #subset the dataframe keeping only selected columns (in addition to time and status) 
    
    if(input$AFTdist=="exp")
    {
     expfit = survreg(Surv(time, status)~., data = subsetted, dist="exp") 
     summary(expfit)
    }
    else if(input$AFTdist=="weib")
    {
     weibfit = survreg(Surv(time, status)~., data = subsetted, dist = "weib")
     summary(weibfit)
    }
    else if(input$AFTdist=="loglogistic")
    {
      llogfit = survreg(Surv(time, status)~., data = subsetted, dist = "loglogistic")
      summary(llogfit)
    }
    
  }else{
    
    if(input$AFTdist=="exp")
    {
      expfit = survreg(Surv(time, status)~., data = subsetted, dist="exp") 
      summary(expfit)
    }
    else if(input$AFTdist=="weib")
    {
      weibfit = survreg(Surv(time, status)~., data = subsetted, dist = "weib")
      summary(weibfit)
    }
    else if(input$AFTdist=="loglogistic")
    {
      llogfit = survreg(Surv(time, status)~., data = subsetted, dist = "loglogistic")
      summary(llogfit)
    }
  }
  
  
  })

output$AFTselected = renderPrint({
  
 subsetted = lung
 subsetted$sex = as.factor(subsetted$sex)
  
 if(length(input$variables)!=0)
 {
  subsetted = subsetted %>% select(!!!unique(c("time", "status", input$variables))) #subset the dataframe keeping only selected columns (in addition to time and status) 
  if(input$AFTdist=="exp")
  {
    distname = "Exponential"
    expfit = survreg(Surv(time, status)~., data = subsetted, dist="exp") 
    modAFT=summary(expfit)
    pvalsAFT = modAFT$table[-1,4]
    #pvalues for all the chosen covariates excluding intercept
  }
  else if(input$AFTdist=="weib")
  {
    distname = "Weibull"
    weibfit = survreg(Surv(time, status)~., data = subsetted, dist = "weib")
    modAFT=summary(weibfit)
    pvalsAFT = modAFT$table[-c(1, length(modAFT$table[,4])),4]
    #pvalues for all the chosen covariates excluding intercept
  }
  else if(input$AFTdist=="loglogistic")
  {
    distname = "Loglogistic"
    llogfit = survreg(Surv(time, status)~., data = subsetted, dist = "loglogistic")
    modAFT=summary(llogfit)
    pvalsAFT = modAFT$table[-c(1, length(modAFT$table[,4])),4]
    #pvalues for all the chosen covariates excluding intercept
  }
  
  signifcovsAFT = names(pvalsAFT[which(pvalsAFT<0.05)])
  #names of the significant predictors

  if(length(signifcovsAFT)>=1)
  {cat("The ", distname, " AFT regression model with covariates: (", 
                toString(unique(c("time", "status", input$variables))),
                ") yields the following predictors significant at the 0.05 level: ",
                toString(signifcovsAFT), ".", sep="")
  }else{
    cat("The ", distname, " AFT regression model with covariates: ", 
                  toString(unique(c("time", "status", input$variables))),
                  " does not yield any predictors significant at the 0.05 level.", sep="")
  }
 
 }else{###OUTER ELSE
   
   if(input$AFTdist=="exp")
   {
     distname = "Exponential"
     expfit = survreg(Surv(time, status)~., data = subsetted, dist="exp") 
     modAFT=summary(expfit)
     pvalsAFT = modAFT$table[-1,4]
     #pvalues for all the chosen covariates excluding intercept
   }
   else if(input$AFTdist=="weib")
   {
     distname = "Weibull"
     weibfit = survreg(Surv(time, status)~., data = subsetted, dist = "weib")
     modAFT=summary(weibfit)
     pvalsAFT = modAFT$table[-c(1, length(modAFT$table[,4])),4]
     #pvalues for all the chosen covariates excluding intercept
   }
   else if(input$AFTdist=="loglogistic")
   {
     distname = "Loglogistic"
     llogfit = survreg(Surv(time, status)~., data = subsetted, dist = "loglogistic")
     modAFT=summary(llogfit)
     pvalsAFT = modAFT$table[-c(1, length(modAFT$table[,4])),4]
     #pvalues for all the chosen covariates excluding intercept
   }
   
   signifcovsAFT = names(pvalsAFT[which(pvalsAFT<0.05)])
   #names of the significant predictors
   
   if(length(signifcovsAFT)>=1)
   {cat("The ", distname, " AFT regression model with covariates: (", 
                  toString(unique(names(lung))),
                  ") yields the following predictors significant at the 0.05 level: ",
                  toString(signifcovsAFT), ".", sep = "")
   }else{
     cat("The ", distname, " AFT regression model with covariates: ", 
                   toString(unique(names(lung))),
                   " does not yield any predictors significant at the 0.05 level.",
         sep = "")
   }
 }
  
})

output$AFT.CS.plot = renderPlot({
  
  subsetted = na.omit(lung)
  subsetted$sex = as.factor(subsetted$sex)
  
  exp.H = function(t, lambda) {(lambda*t)}
  weib.H = function(t, lambda, gamma){(lambda*t)^gamma}
  llog.H = function(t, lambda, gamma){log(1+(lambda*t)^gamma)}
  
  if(length(input$variables)!=0)
  {
    subsetted = subsetted %>% select(!!!unique(c("time", "status", input$variables))) #subset the dataframe keeping only selected columns (in addition to time and status) 
    if(input$AFTdist=="exp")
    {
      expfit = survreg(Surv(time, status)~., data = subsetted, dist="exp")
      exp.lambda_hat = exp(-predict(expfit, type = "lp"))
      
      CS.exp <- exp.H(subsetted$time, exp.lambda_hat)
      
      NA.exp <- survfit(
        Surv(CS.exp, subsetted$status) ~ 1, type = "fh", error = "tsiatis")
      
      plot(
        NA.exp, fun = "cumhaz", mark.time = FALSE, conf.int = FALSE,
        lty = "dotted", main = "Cox-Snell goodness-of-fit",
        xlab = "Cox-Snell residual", ylab = "Estimated cumulative hazard"
      )
      abline(0, 1)
      
    }
    else if(input$AFTdist=="weib")
    {
      
      weibfit = survreg(Surv(time, status)~., data = subsetted, dist="weibull")
      
      weib.lambda_hat = exp(-predict(weibfit, type = "lp"))
      
      weib.gamma_hat = 1/weibfit$scale
      
      CS.weib <- weib.H(subsetted$time, weib.lambda_hat, weib.gamma_hat)
      
      NA.weib <- survfit(
        Surv(CS.weib, subsetted$status) ~ 1, type = "fh", error = "tsiatis")
      
      plot(
        NA.weib, fun = "cumhaz", mark.time = FALSE, conf.int = FALSE,
        lty = "dotted", main = "Cox-Snell goodness-of-fit",
        xlab = "Cox-Snell residual", ylab = "Estimated cumulative hazard"
      )
      abline(0, 1)
      
      
    }
    else if(input$AFTdist=="loglogistic")
    {
      llogfit = survreg(Surv(time, status)~., data = subsetted, dist="loglogistic")
      llog.lambda_hat = exp(-predict(llogfit, type = "lp"))
      llog.gamma_hat = 1/llogfit$scale
      
      CS.llog <- llog.H(subsetted$time, llog.lambda_hat, llog.gamma_hat)
      
      NA.llog <- survfit(
        Surv(CS.llog, subsetted$status) ~ 1, type = "fh", error = "tsiatis")
      
      plot(
        NA.llog, fun = "cumhaz", mark.time = FALSE, conf.int = FALSE,
        lty = "dotted", main = "Cox-Snell goodness-of-fit",
        xlab = "Cox-Snell residual", ylab = "Estimated cumulative hazard"
      )
      abline(0, 1)
      
    }
    
    
  }else if(length(input$variables)==0)
  {
    if(input$AFTdist=="exp")
    {
      expfit = survreg(Surv(time, status)~., data = subsetted, dist="exp")
      exp.lambda_hat = exp(-predict(expfit, type = "lp"))
      
      CS.exp <- exp.H(subsetted$time, exp.lambda_hat)
      
      NA.exp <- survfit(
        Surv(CS.exp, subsetted$status) ~ 1, type = "fh", error = "tsiatis")
      
      plot(
        NA.exp, fun = "cumhaz", mark.time = FALSE, conf.int = FALSE,
        lty = "dotted", main = "Cox-Snell goodness-of-fit",
        xlab = "Cox-Snell residual", ylab = "Estimated cumulative hazard"
      )
      abline(0, 1)
      
    }
    else if(input$AFTdist=="weib")
    {
      
      weibfit = survreg(Surv(time, status)~., data = subsetted, dist="weibull")
      
      weib.lambda_hat = exp(-predict(weibfit, type = "lp"))
      
      weib.gamma_hat = 1/weibfit$scale
      
      CS.weib <- weib.H(subsetted$time, weib.lambda_hat, weib.gamma_hat)
      
      NA.weib <- survfit(
        Surv(CS.weib, subsetted$status) ~ 1, type = "fh", error = "tsiatis")
      
      plot(
        NA.weib, fun = "cumhaz", mark.time = FALSE, conf.int = FALSE,
        lty = "dotted", main = "Cox-Snell goodness-of-fit",
        xlab = "Cox-Snell residual", ylab = "Estimated cumulative hazard"
      )
      abline(0, 1)
      
      
    }
    else if(input$AFTdist=="loglogistic")
    {
      llogfit = survreg(Surv(time, status)~., data = subsetted, dist="loglogistic")
      llog.lambda_hat = exp(-predict(llogfit, type = "lp"))
      llog.gamma_hat = 1/llogfit$scale
      
      CS.llog <- llog.H(subsetted$time, llog.lambda_hat, llog.gamma_hat)
      
      NA.llog <- survfit(
        Surv(CS.llog, subsetted$status) ~ 1, type = "fh", error = "tsiatis")
      
      plot(
        NA.llog, fun = "cumhaz", mark.time = FALSE, conf.int = FALSE,
        lty = "dotted", main = "Cox-Snell goodness-of-fit",
        xlab = "Cox-Snell residual", ylab = "Estimated cumulative hazard"
      )
      abline(0, 1)
      
    }
    
  }
  
  legend("bottomright",
         lty = c("dashed", "solid"),
         col = c("black", "black"),
         legend = c(
           "Parametric Model", "y=x")
  )  
  
})

output$NAplotWithOverlay = renderPlot({
  
  subsetted = lung
  subsetted$sex = as.factor(subsetted$sex)
  
  if(length(input$variables)!=0)
  {
    subsetted = subsetted %>% select(!!!unique(c("time", "status", input$variables))) #subset the dataframe keeping only selected columns (in addition to time and status) 
  }  
  
  ## Nelson-Aalen curve:
  NelsonAalen <- survfit(Surv(time, status) ~ 1, data = subsetted, 
                         conf.type = "log-log", type = "fleming-harrington",
                         error = "tsiatis")
  
  plot(NelsonAalen, mark.time = FALSE, fun = "cumhaz",
       main = "Cumulative hazard curves",
       xlab = "Time", ylab = "Cumulative hazard")
  
  #Cumulative Hazard Function fitted from Exponential,
  #using the obtained lambda_hat:
  exp.H = function(t, lambda) {(lambda*t)}
  weib.H = function(t, lambda, gamma){(lambda*t)^gamma}
  llog.H = function(t, lambda, gamma){log(1+(lambda*t)^gamma)}
  
  expfit = survreg(Surv(time, status)~1, data=subsetted, dist="exponential")
  exp.lambda_hat = exp(-coef(expfit))
  
  weibfit = survreg(Surv(time, status)~1, data=subsetted, dist="weibull")
  weib.lambda_hat = exp(-coef(weibfit))
  weib.gamma_hat = 1/weibfit$scale
  
  llogfit = survreg(Surv(time, status)~1, data=subsetted, dist="loglogistic")
  llog.lambda_hat = exp(-coef(llogfit))
  llog.gamma_hat = 1/llogfit$scale
  
  if("exp" %in% input$AFTdistNA)
  {
    lines(sort(lung$time),
          exp.H(sort(lung$time),
                lambda=exp.lambda_hat), col = "red")
  
  }
  if("weib" %in% input$AFTdistNA){
    lines(sort(lung$time),
          weib.H(sort(lung$time),
                 lambda=weib.lambda_hat,
                 gamma=weib.gamma_hat), col = "blue")
  
  }
  if("loglogistic" %in% input$AFTdistNA){
    lines(sort(lung$time),
          llog.H(sort(lung$time),
                 lambda=llog.lambda_hat,
                 gamma=llog.gamma_hat), col = "green")
    }
  
  #for the legend:
  if("exp" %in% input$AFTdistNA &
     "weib" %in% input$AFTdistNA &
     "loglogistic" %in% input$AFTdistNA){ #if all three selected: include all 3 in legend.
    
    legend("topleft",
           lty = c("solid", "dashed", "solid", "solid", "solid"),
           col = c("black", "black", "red", "blue", "green"),
           legend = c(
             "Nelson-Aalen curve", "95% confidence limits for Nelson-Aalen curve",
             "Cumulative hazard fitted from Exponential model",
             "Cumulative hazard fitted from Weibull model",
             "Cumulative hazard fitted from Log-Logistic model")
    )
    
  }else if("exp" %in% input$AFTdistNA &
             "weib" %in% input$AFTdistNA)
  {
    legend("topleft",
           lty = c("solid", "dashed", "solid", "solid"),
           col = c("black", "black", "red", "blue"),
           legend = c(
             "Nelson-Aalen curve", "95% confidence limits for Nelson-Aalen curve",
             "Cumulative hazard fitted from Exponential model",
             "Cumulative hazard fitted from Weibull model")
    )
  }else if("exp" %in% input$AFTdistNA &
           "loglogistic" %in% input$AFTdistNA)
  {
    legend("topleft",
           lty = c("solid", "dashed", "solid", "solid"),
           col = c("black", "black", "red", "green"),
           legend = c(
             "Nelson-Aalen curve", "95% confidence limits for Nelson-Aalen curve",
             "Cumulative hazard fitted from Exponential model",
             "Cumulative hazard fitted from Log-logistic model")
    )
  }else if("weib" %in% input$AFTdistNA &
           "loglogistic" %in% input$AFTdistNA)
  {
    legend("topleft",
           lty = c("solid", "dashed", "solid", "solid"),
           col = c("black", "black", "blue", "green"),
           legend = c(
             "Nelson-Aalen curve", "95% confidence limits for Nelson-Aalen curve",
             "Cumulative hazard fitted from Weibull model",
             "Cumulative hazard fitted from Log-logistic model")
    )
  }else if("exp" %in% input$AFTdistNA)
  {
    legend("topleft",
           lty = c("solid", "dashed", "solid"),
           col = c("black", "black", "red"),
           legend = c(
             "Nelson-Aalen curve", "95% confidence limits for Nelson-Aalen curve",
             "Cumulative hazard fitted from Exponential model")
    )
  }else if("weib" %in% input$AFTdistNA)
  {
    legend("topleft",
           lty = c("solid", "dashed", "solid"),
           col = c("black", "black", "blue"),
           legend = c(
             "Nelson-Aalen curve", "95% confidence limits for Nelson-Aalen curve",
             "Cumulative hazard fitted from Weibull model")
    )
  }else if("loglogistic" %in% input$AFTdistNA)
  {
    legend("topleft",
           lty = c("solid", "dashed", "solid"),
           col = c("black", "black", "green"),
           legend = c(
             "Nelson-Aalen curve", "95% confidence limits for Nelson-Aalen curve",
             "Cumulative hazard fitted from Log-logistic model")
    )
  }
  else
  {
    legend("topleft",
           lty = c("solid", "dashed"),
           col = c("black", "black"),
           legend = c(
             "Nelson-Aalen curve", "95% confidence limits for Nelson-Aalen curve")
    )
  }
})

output$variableselectorcoxph <- renderTable({
  if (length(input$variables.coxph) == 0) return(head(lung, n=8))
  lung %>% select(!!!unique(c("time", "status", input$variables.coxph))) %>% head(n=8)
}, rownames = TRUE) 

#to fit Cox PH model and get model summary:
output$coxphsummary <- renderPrint({
  
  subsetted2 = lung
  subsetted2$sex = as.factor(subsetted2$sex)
  
  if(length(input$variables.coxph)!=0)
  {  
    subsetted2 = subsetted2 %>% select(!!!unique(c("time", "status", input$variables.coxph)))
    #subset the dataframe keeping only selected columns (in addition to time and status) 
  }
  
  
  if(input$ties.method=="exact")
  {  
    coxmod = coxph(Surv(time, status) ~ ., data = subsetted2, ties = "exact")
  }
  else if(input$ties.method=="efron")
  {  
    coxmod = coxph(Surv(time, status) ~ ., data = subsetted2, ties = "efron")
  }
  else if(input$ties.method=="breslow")
  {  
    coxmod = coxph(Surv(time, status) ~ ., data = subsetted2, ties = "breslow")
  }
  
  
  summary(coxmod) 
})


output$coxphselected = renderPrint({
  subsetted2 = lung
  subsetted2$sex = as.factor(subsetted2$sex)
  
  if(length(input$variables.coxph)!=0)
  {  
    subsetted2 = subsetted2 %>% select(!!!unique(c("time", "status", input$variables.coxph)))
    #subset the dataframe keeping only selected columns (in addition to time and status) 
  }
  
  
  if(input$ties.method=="exact")
  {  
    coxmod = coxph(Surv(time, status) ~ ., data = subsetted2, ties = "exact")
  }
  else if(input$ties.method=="efron")
  {  
    coxmod = coxph(Surv(time, status) ~ ., data = subsetted2, ties = "efron")
  }
  else if(input$ties.method=="breslow")
  {  
    coxmod = coxph(Surv(time, status) ~ ., data = subsetted2, ties = "breslow")
  }
  
  modcox=summary(coxmod)
  pvalscox = modcox$coefficients[,5]
  signifcovs.cox = names(pvalscox[which(pvalscox<0.05)])
  #names of the significant predictors
  
  if(length(signifcovs.cox)>=1)
  {cat("The Cox Proportional Hazards model with covariates: (", 
       toString(unique(c("time", "status", input$variables.coxph))),
       ") yields the following predictors significant at the 0.05 level: ",
       toString(signifcovs.cox), ".", sep="")
  }else{
    cat("The Cox Proportional Hazards model with covariates: ", 
        toString(unique(c("time", "status", input$variables.coxph))),
        " does not yield any predictors significant at the 0.05 level.", sep="")
  }
})


output$coxph.CS.plot = renderPlot({
  
  subsetted = na.omit(lung)
  subsetted$sex = as.factor(subsetted$sex)
  
  if(length(input$variables.coxph)!=0)
  {
    subsetted = subsetted %>% select(!!!unique(c("time", "status", input$variables.coxph))) #subset the dataframe keeping only selected columns (in addition to time and status) 
    
    if(input$ties.method=="exact")
    {cox <- coxph(Surv(time, status) ~ ., data = subsetted, ties="exact")
    }else if(input$ties.method=="breslow")
    {cox <- coxph(Surv(time, status) ~ ., data = subsetted, ties="breslow")
    }else if(input$ties.method=="efron")
    {cox <- coxph(Surv(time, status) ~ ., data = subsetted, ties="efron")
    }
    
    delta <- subsetted$status - 1
    CS.cox <- delta - residuals(cox, type = "martingale")
    NA.cox <- survfit(Surv(CS.cox, delta) ~ 1, type = "fh", error = "tsiatis")
    
    plot(
      NA.cox, fun = "cumhaz", mark.time = FALSE, conf.int = FALSE,
      lty = "dashed"
    )
    abline(0,1)
    
    legend(
      "bottomright", lty = c("dashed", "solid"),
      legend = c(
        "Cox model", "y=x"
      )
    )
  }
  else if(length(input$variables.coxph)==0)
  {
    if(input$ties.method=="exact")
    {cox <- coxph(Surv(time, status) ~ ., data = subsetted, ties="exact")
    }else if(input$ties.method=="breslow")
    {cox <- coxph(Surv(time, status) ~ ., data = subsetted, ties="breslow")
    }else if(input$ties.method=="efron")
    {cox <- coxph(Surv(time, status) ~ ., data = subsetted, ties="efron")
    }
    
    delta <- subsetted$status - 1
    CS.cox <- delta - residuals(cox, type = "martingale")
    NA.cox <- survfit(Surv(CS.cox, delta) ~ 1, type = "fh", error = "tsiatis")
    
    plot(
      NA.cox, fun = "cumhaz", mark.time = FALSE, conf.int = FALSE,
      lty = "dashed"
    )
    abline(0,1)
    
    legend(
      "bottomright", lty = c("dashed", "solid"),
      legend = c(
        "Cox model", "y=x"
      )
    )
  } 
})

output$logranktest = renderPrint({
  subsetted = lung %>% filter(time>input$logranktime[1] & time<input$logranktime[2])
  subsetted$sex = as.factor(subsetted$sex)
  
  if(input$logranktime[1] == input$logranktime[2])
  {
    cat("Try again with two"," distinct timepoints!", sep="")
  }  
  else if(length(levels(subsetted$sex))==1) #if there is just one level
  {
    cat("There is only one gender group in this interval."," Try again with a wider interval!", sep="")
  }
   else{   
  survdiff(Surv(time, status) ~ sex, data = subsetted, rho = 0)
   }
}) 

output$logrankpvalue = renderPrint({
    subsetted = lung %>% filter(time>input$logranktime[1] & time<input$logranktime[2])
    subsetted$sex = as.factor(subsetted$sex)

    if((input$logranktime[1] != input$logranktime[2]) &
       length(levels(subsetted$sex))==2)
    {  
    t =  survdiff(Surv(time, status) ~ sex, data = subsetted, rho=0)
    pval = 1 - pchisq(t$chisq, 1)

    if(pval>0.05)
    {
      cat("The p-value for the log-rank test is ", round(pval,6), 
          ". As it is => 0.05, we accept the null hypothesis that the survival is same across male and female groups.",
          " A plot of the survival curves by sex is as below.", sep="")
    }else{
      cat("The p-value for the log-rank test is ", round(pval,6), 
          ". As it is < 0.05, we reject the null hypothesis that the survival is same across male and female groups.",
          " A plot of the survival curves by sex is as below.", sep="")
    }
    
    }
})

output$logrankplot = renderPlot({
  subsetted = lung %>% filter(time>input$logranktime[1] & time<input$logranktime[2])
  subsetted$sex = as.factor(subsetted$sex)
  
  if((input$logranktime[1] != input$logranktime[2]) &
     length(levels(subsetted$sex))==2)
  {  
  plot(survfit(Surv(time, status) ~ sex, data = subsetted), 
       xlab = "Time", 
       ylab = "Overall survival probability", col=c("black", "red"),
       main = "Survival curves by sex")
    
  legend("topright",
           lty = c("solid", "solid"),
           col = c("black", "red"),
           legend = c("Male", "Female")
    )
    
  }
})




})

