# Libraries needed
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(metafor)
library(stringr)
library(readxl)
library(mapdata)
library(maps)
library(maptools)
library(rgdal)
library(colorspace)
library(gt)
library(scales)
library(Amelia)
library(epiR)
# Note the analysis is based on smaller functional modules (R functions). 
# Each of the functions can be run repeatedly using different sets of data based on the specific needs.
# Following each of the functions, there are explanations for arguments if they are not self-explanatory already.
# Some of the functions depend on others so they need to be sourced all at once.
# 1. Generate Monte Carlo draws----
genMC <- function(df, id, input.mean, input.se, transFUN, n, output.name = "value") {
  set.seed(6920) # Change to any values if needed
  res <-by(df[c(id, input.mean, input.se)],
           df[id],
           function(x, n) 
             return(data.frame(
               id = x[[id]],
               index = 1:n,
               value = transFUN(rnorm(n = n,mean = x[[input.mean]], sd = x[[input.se]]))
             )),
           n = n)
  res <- do.call(rbind, res)
  row.names(res) <- NULL
  names(res) <- c(id, "index", output.name)
  return(res)
}
# df - dataset
# id - study id
# transFUN - function for data transformation
# n - no. of draws
# 2. Generate incidence rate and its standard error----
genINC <- function(case, deno) {
  est <- log(case/deno)
  se <- sqrt(1/case- 1/deno)
  return(c(est, se))
}
# 3. Generate rate ratios for imputation----
# 3.1. RSV-ALRI incidence rate----
genComImputeTab <- function(df) {
  fit1 <- rma.glmm(measure = "IRR",
                   data = df[
                     with(df, 
                          SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(ALRI_N)],
                                             SID[AGEGR == "0-<12m" & !is.na(ALRI_N)])
                          & AGEGR %in% c("0-<60m", "0-<12m")), c("SID", "AGEGR", "ALRI_Deno", "ALRI_N")
                     ] %>% pivot_wider(names_from =AGEGR, values_from = c(ALRI_Deno, ALRI_N)),
                   x1i = `ALRI_N_0-<60m`, x2i=`ALRI_N_0-<12m`, t1i = `ALRI_Deno_0-<60m`, t2i = `ALRI_Deno_0-<12m`)
  fit2 <- rma.glmm(measure = "IRR",
                   data = df[
                     with(df, 
                          SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(ALRI_N)],
                                             SID[AGEGR == "0-<24m" & !is.na(ALRI_N)])
                          & AGEGR %in% c("0-<60m", "0-<24m")), c("SID", "AGEGR", "ALRI_Deno", "ALRI_N")
                     ] %>% pivot_wider(names_from =AGEGR, values_from = c(ALRI_Deno, ALRI_N)),
                   x1i = `ALRI_N_0-<60m`, x2i=`ALRI_N_0-<24m`, t1i = `ALRI_Deno_0-<60m`, t2i = `ALRI_Deno_0-<24m`)
  fit3 <- rma.glmm(measure = "IRR",
                   data = df[
                     with(df, 
                          SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(ALRI_N)],
                                             SID[AGEGR == "0-<36m" & !is.na(ALRI_N)])
                          & AGEGR %in% c("0-<60m", "0-<36m")), c("SID", "AGEGR", "ALRI_Deno", "ALRI_N")
                     ] %>% pivot_wider(names_from =AGEGR, values_from = c(ALRI_Deno, ALRI_N)),
                   x1i = `ALRI_N_0-<60m`, x2i=`ALRI_N_0-<36m`, t1i = `ALRI_Deno_0-<60m`, t2i = `ALRI_Deno_0-<36m`)
  res.df <-     data.frame(
    AGEGR = c("0-<12m", "0-<24m", "0-<36m"),
    est = unlist(list(fit1$b, fit2$b, fit3$b)),
    se = unlist(list(fit1$se, fit2$se, fit3$se)),
    deno.p = c(5, 5/2, 5/3)
  )
  res.df$IRR.est <- exp(res.df$est)
  res.df$IRR.lci <- exp(res.df$est - 1.96*res.df$se)
  res.df$IRR.uci <- exp(res.df$est + 1.96*res.df$se)
  return(res.df)
}
# df - dataset; other variables mentioned in the function are included in the shared data.
# 3.2 RSV-ALRI hospitalisation rate----
genHosImputeTab <- function(df) {
  suppressWarnings({
    fit1 <- rma.glmm(measure = "IRR",
                     data = df[
                       with(df, 
                            SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(HosALRI_N)],
                                               SID[AGEGR == "0-<12m" & !is.na(HosALRI_N)])
                            & AGEGR %in% c("0-<60m", "0-<12m")), c("SID", "AGEGR", "HosALRI_Deno", "HosALRI_N")
                       ] %>% pivot_wider(names_from =AGEGR, values_from = c(HosALRI_Deno, HosALRI_N)),
                     x1i = `HosALRI_N_0-<60m`, x2i=`HosALRI_N_0-<12m`, 
                     t1i = `HosALRI_Deno_0-<60m`, t2i = `HosALRI_Deno_0-<12m`)
    fit2 <- rma.glmm(measure = "IRR",
                     data = df[
                       with(df, 
                            SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(HosALRI_N)],
                                               SID[AGEGR == "0-<24m" & !is.na(HosALRI_N)])
                            & AGEGR %in% c("0-<60m", "0-<24m")), c("SID", "AGEGR", "HosALRI_Deno", "HosALRI_N")
                       ] %>% pivot_wider(names_from =AGEGR, values_from = c(HosALRI_Deno, HosALRI_N)),
                     x1i = `HosALRI_N_0-<60m`, x2i=`HosALRI_N_0-<24m`, 
                     t1i = `HosALRI_Deno_0-<60m`, t2i = `HosALRI_Deno_0-<24m`)
    fit3 <- rma.glmm(measure = "IRR",
                     data = df[
                       with(df, 
                            SID %in% intersect(SID[AGEGR == "0-<60m" & !is.na(HosALRI_N)],
                                               SID[AGEGR == "0-<36m" & !is.na(HosALRI_N)])
                            & AGEGR %in% c("0-<60m", "0-<36m")), c("SID", "AGEGR", "HosALRI_Deno", "HosALRI_N")
                       ] %>% pivot_wider(names_from =AGEGR, values_from = c(HosALRI_Deno, HosALRI_N)),
                     x1i = `HosALRI_N_0-<60m`, x2i=`HosALRI_N_0-<36m`, 
                     t1i = `HosALRI_Deno_0-<60m`, t2i = `HosALRI_Deno_0-<36m`)
  })
  res.df <-     data.frame(
    AGEGR = c("0-<12m", "0-<24m", "0-<36m"),
    est = unlist(list(fit1$b, fit2$b, fit3$b)),
    se = unlist(list(fit1$se, fit2$se, fit3$se)),
    deno.p = c(5, 5/2, 5/3)
  )
  res.df$IRR.est <- exp(res.df$est)
  res.df$IRR.lci <- exp(res.df$est - 1.96*res.df$se)
  res.df$IRR.uci <- exp(res.df$est + 1.96*res.df$se)
  return(res.df)
}
# df - dataset; other variables mentioned in the function are included in the shared data.
# 4. Imputation based on rate ratios obtained above----
# 4.1 RSV-ALRI incidence rate----
genComImputeEach <- function(df.each) {
  suppressMessages({
    ref <- left_join(df.each[nrow(df.each),],com_rate.impute)
    impute.res <- left_join(ref[rep(1,N.imputation),
                                c("SID", "UNICEF", "WHO", "Setting0", "StudyMY", "Income", "Dev", "Indigenous", 
                                  "ViralTest", "studyLabel", "QA_all","USID")],df.each[0,])
  })
  impute.res$AGEGR <- "0-<60m"
  impute.res$Impute <- 1:N.imputation
  impute.res$ALRI_Deno <- ref$ALRI_Deno * ref$deno.p
  set.seed(6920)
  impute.res$ALRI_N <-   round(exp(rnorm(N.imputation, mean = ref$est, 
                                         sd = ref$se)) * ref$ALRI_N/ref$ALRI_Deno * impute.res$ALRI_Deno,0)
  return(impute.res)
}
# df.each - data by each study (the imputation is done one [study] by one).
# other variables mentioned in the function are included in the shared data.
# 4.2 RSV-ALRI hospitalisation rate----
genHosImputeEach <- function(df.each) {
  suppressMessages({
    ref <- left_join(df.each[nrow(df.each),],hos_rate.impute)
    impute.res <- left_join(ref[rep(1,N.imputation),
                                c("SID", "UNICEF", "WHO", "Setting0", "StudyMY", "Income", "Dev", "Indigenous", 
                                  "ViralTest", "studyLabel", "QA_all","USID")],df.each[0,])
  })
  impute.res$AGEGR <- "0-<60m"
  impute.res$Impute <- 1:N.imputation
  impute.res$HosALRI_Deno <- ref$HosALRI_Deno * ref$deno.p
  set.seed(6920)
  impute.res$HosALRI_N <-   round(exp(rnorm(N.imputation, mean = ref$est, 
                                            sd = ref$se)) * ref$HosALRI_N/ref$HosALRI_Deno * impute.res$HosALRI_Deno,0)
  return(impute.res)
}
# df.each - data by each study (the imputation is done one [study] by one).
# other variables mentioned in the function are included in the shared data.
# 5. Meta-analysis (regional estimates)----
# 5.1 Rate----
genMetaRateEach <- function(df, prefix, varToKeep = NULL, rate.adjust = 1000) {
  if(is.null(df)){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      IR.est = NA,
      IR.lci = NA,
      IR.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  df <- df[!(is.na(df[paste(prefix, "_N", sep = "")])|is.na(df[paste(prefix, "_Deno", sep = "")])),]
  df[paste(prefix, "_Deno", sep = "")] <- round(df[paste(prefix, "_Deno", sep = "")],0)
  df[paste(prefix, "_N", sep = "")] <- round(df[paste(prefix, "_N", sep = "")],0)
  if(nrow(df)==0){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      IR.est = NA,
      IR.lci = NA,
      IR.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  if(nrow(df) ==1) {
    res.df <- data.frame(
      est = genINC(df[[paste(prefix, "_N", sep = "")]], 
                   df[[paste(prefix, "_Deno", sep = "")]])[1],
      se = genINC(df[[paste(prefix, "_N", sep = "")]], 
                  df[[paste(prefix, "_Deno", sep = "")]])[2],
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
  }else{
    fit <- rma.glmm(measure = "IRLN", data = df, 
                    xi = get(paste(prefix, "_N", sep = "")),
                    ti = get(paste(prefix, "_Deno", sep = "")))
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
  }
  res.df$IR.est <- exp(res.df$est) * rate.adjust
  res.df$IR.lci <- exp(res.df$est - 1.96*res.df$se)* rate.adjust
  res.df$IR.uci <- exp(res.df$est + 1.96*res.df$se)* rate.adjust
  res.df <- cbind(df[1,varToKeep], res.df)
  return(res.df)
}
# Definition needs to be specified by using the argument of "prefix".
# Other information from the original dataset can be retained through the argument of "varToKeep".
# 5.2 Proportion (by definition)----
genMetaPropEach <- function(df, prefix, deno.text,varToKeep = NULL, prop.adjust = 100) {
  if(is.null(df)){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      prop.est = NA,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  df <- df[!(is.na(df[paste(prefix, "_N", sep = "")])|is.na(df[paste(prefix, "_", deno.text,sep = "")])),]
  df[paste(prefix, "_", deno.text,sep = "")] <- round(df[paste(prefix, "_", deno.text,sep = "")],0)
  df[paste(prefix, "_N", sep = "")] <- round(df[paste(prefix, "_N", sep = "")],0)
  if(nrow(df)==0){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      prop.est = NA,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  if(nrow(df) ==1) {
    fit <- rma.glmm(measure = "PLO", data = df, 
                    xi = get(paste(prefix, "_N", sep = "")),
                    ni = get(paste(prefix, "_", deno.text,sep = "")),
                    model="UM.FS")
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
  }else{
    fit <- rma.glmm(measure = "PLO", data = df, 
                    xi = get(paste(prefix, "_N", sep = "")),
                    ni = get(paste(prefix, "_", deno.text,sep = "")))
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
  }
  res.df$prop.est <- transf.ilogit(res.df$est) * prop.adjust
  res.df$prop.lci <- transf.ilogit(res.df$est - 1.96*res.df$se)* prop.adjust
  res.df$prop.uci <- transf.ilogit(res.df$est + 1.96*res.df$se)* prop.adjust
  res.df <- cbind(df[1,varToKeep], res.df)
  return(res.df)
}
# Definition needs to be specified by using the argument of "prefix".
# Other information from the original dataset can be retained through the argument of "varToKeep".
# 5.3 Proporton (user self-specified; more flexible)----
genMetaPropEach.flex <- function(df, case.text, deno.text,varToKeep = NULL, prop.adjust = 100) {
  if(is.null(df)){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      prop.est = NA,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  df <- df[!(is.na(df[case.text])|is.na(df[deno.text])),]
  df[deno.text] <- round(df[deno.text],0)
  df[case.text] <- round(df[case.text],0)
  if(nrow(df)==0){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      prop.est = NA,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  if(nrow(df) ==1) {
    fit <- rma.glmm(measure = "PLO", data = df, 
                    xi = get(case.text),
                    ni = get(deno.text),
                    model="UM.FS")
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
  }else{
    fit <- rma.glmm(measure = "PLO", data = df, 
                    xi = get(case.text),
                    ni = get(deno.text))
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
  }
  res.df$prop.est <- transf.ilogit(res.df$est) * prop.adjust
  res.df$prop.lci <- transf.ilogit(res.df$est - 1.96*res.df$se)* prop.adjust
  res.df$prop.uci <- transf.ilogit(res.df$est + 1.96*res.df$se)* prop.adjust
  res.df <- cbind(df[1,varToKeep], res.df)
  return(res.df)
}
# this is similar to 5.2 but users can specify the case and denominator cols.
# 5.4 for imputed datasets----
# as multiple imputed samples are generated, the meta-analysis will be conducted multiple times.
# This function is for single meta-analysis for one sample of the imputed datasets
genMetaRateEach.Impute <- function(df, prefix,varToKeep = NULL, rate.adjust = 1000) {
  if(is.null(df)){
    return(NULL)
  }else{
    res.df <- data.frame(      est = NA,
                               se = NA,
                               n.all = nrow(df),
                               n.impute = NA,
                               IR.est = NA,
                               IR.lci = NA,
                               IR.uci = NA)
    res.df <- res.df[0,]
    for(i in 1:N.imputation){
      res.df <- rbind(res.df,
                      genMetaRateEach(df[df$Impute %in% c(0,i),], prefix = prefix,
                                      varToKeep = "Impute"))
    }
  }
  res.rubins <- unlist(mi.meld(q = res.df["est"], se = res.df["se"]))
  res.rubins <- data.frame(
    est = res.rubins[1],
    se = res.rubins[2],
    n.all = res.df$n.all[1],
    n.impute = res.df$n.impute[1]
  )
  res.rubins <- cbind(df[1,varToKeep], res.rubins)
  res.rubins$IR.est <- exp(res.rubins$est) * rate.adjust
  res.rubins$IR.lci <- exp(res.rubins$est - 1.96*res.rubins$se)* rate.adjust
  res.rubins$IR.uci <- exp(res.rubins$est + 1.96*res.rubins$se)* rate.adjust
  return(res.rubins)
}
# 5.5 Proportion (for sensitivity analysis)----
# The two functions below are similar to 5.2 and 5.3 but use REM rather than GLMM, for sensitivity analysis
genMetaPropEach.flex <- function(df, case.text, deno.text,varToKeep = NULL, prop.adjust = 100) {
  if(is.null(df)){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      prop.est = NA,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  df <- df[!(is.na(df[case.text])|is.na(df[deno.text])),]
  df[deno.text] <- round(df[deno.text],0)
  df[case.text] <- round(df[case.text],0)
  if(nrow(df)==0){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      prop.est = NA,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  if(nrow(df) ==1) {
    fit <- rma.glmm(measure = "PLO", data = df, 
                    xi = get(case.text),
                    ni = get(deno.text),
                    model="UM.FS")
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
  }else{
    fit <- rma.glmm(measure = "PLO", data = df, 
                    xi = get(case.text),
                    ni = get(deno.text))
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
  }
  res.df$prop.est <- transf.ilogit(res.df$est) * prop.adjust
  res.df$prop.lci <- transf.ilogit(res.df$est - 1.96*res.df$se)* prop.adjust
  res.df$prop.uci <- transf.ilogit(res.df$est + 1.96*res.df$se)* prop.adjust
  res.df <- cbind(df[1,varToKeep], res.df)
  return(res.df)
}
genMetaPropEach_previous.flex <- function(df, case.text, deno.text,varToKeep = NULL, prop.adjust = 100) {
  if(is.null(df)){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      prop.est = NA,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  df <- df[!(is.na(df[case.text])|is.na(df[deno.text])),]
  df[deno.text] <- round(df[deno.text],0)
  df[case.text] <- round(df[case.text],0)
  if(nrow(df)==0){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      n.impute = NA,
      prop.est = NA,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  if(nrow(df) ==1) {
    fit <- rma(measure = "PLO", data = df, 
               xi = get(case.text),
               ni = get(deno.text),
               add = 0.005,
               model="UM.FS")
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
  }else{
    fit <- rma(measure = "PLO", data = df, 
               xi = get(case.text),
               ni = get(deno.text),
               add = 0.005)
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df),
      n.impute = sum(df$Impute!=0)
    )
  }
  res.df$prop.est <- transf.ilogit(res.df$est) * prop.adjust
  res.df$prop.lci <- transf.ilogit(res.df$est - 1.96*res.df$se)* prop.adjust
  res.df$prop.uci <- transf.ilogit(res.df$est + 1.96*res.df$se)* prop.adjust
  res.df <- cbind(df[1,varToKeep], res.df)
  return(res.df)
}
# 6.Generate global estimates----
# 6.1 from rate (e.g. RSV-ALRI incidence rate)----
genRateGlobal <- function(df, n.level){
  if(sum(!is.na(df$est))<n.level) {
    return(df)
  }else{
    rate <- genMC(df =df, id = "Group", input.mean = "est", input.se = "se", n = N.MC,transFUN = exp)
    rate <- left_join(rate, unique(df[c("Group", "Pop")]))
    rate <- rate %>% group_by(index) %>%
      dplyr::summarise(N = round(sum(value * Pop),3))
    new.df <- df[1,]
    new.df$AGEGR <- df$AGEGR[1]
    new.df$Group <- "Global"
    new.df$est <- NA
    new.df$se <- NA
    new.df$n.all <- sum(df$n.all)
    new.df$n.impute <- sum(df$n.impute)
    new.df$N.est <- round(median(rate$N),3)
    new.df$N.lci <- round(quantile(rate$N, 0.025),3)
    new.df$N.uci <- round(quantile(rate$N, 0.975),3)
    new.df$Pop <- sum(df$Pop)
    new.df$IR.est <- with(new.df, N.est / Pop*1000)
    new.df$IR.lci <- with(new.df, N.lci / Pop*1000)
    new.df$IR.uci <- with(new.df, N.uci / Pop*1000)
    return(rbind(df, new.df))
  }
}
# n.level - specify the number of levels expected (e.g. 2 for country development status and 4 for country income classifiation)
# 6.2 from rate and proportion (i.e. two steps such as in-hospital deaths)----
genRateGlobal.fromProp <- function(df, n.level) {
  df <- df[!(is.na(df$est.x)|is.na(df$est.y)),]
  if(nrow(df)==0){
    return(NULL)
  }
  df <- suppressMessages(left_join(df, pop_region.raw))
  df.MC <-   suppressMessages({
    left_join(
      df,
      left_join(
        genMC(df =df, id = "Group", input.mean = "est.x", input.se = "se.x", 
              n =N.MC, transFUN = exp, output.name = "rate"),
        genMC(df =df, id = "Group", input.mean = "est.y", input.se = "se.y", 
              n =N.MC, transFUN = transf.ilogit, output.name = "prop")
      )
    )
  })
  df.MC$N <- df.MC$Pop *df.MC$rate
  df.MC$N_prop <- df.MC$Pop *df.MC$rate * df.MC$prop
  df1<- df.MC %>% group_by(AGEGR, Group, Impute.x, Impute.y, n.all, n.impute) %>%
    dplyr::summarise(N.est = round(median(N_prop),3),
                     N.lci = round(quantile(N_prop, 0.025),3),
                     N.uci = round(quantile(N_prop, 0.975),3),
                     Pop = Pop[1],
                     prop.est = median(N_prop/N)*100,
                     prop.lci = quantile(N_prop/N, 0.025)*100,
                     prop.uci = quantile(N_prop/N, 0.975)*100
    )
  df1$IR.est <- with(df1, N.est/Pop*1000)
  df1$IR.lci <- with(df1, N.lci/Pop*1000)
  df1$IR.uci <- with(df1, N.uci/Pop*1000)
  if(nrow(df1) == n.level){
    df2<- df.MC %>% group_by(index) %>%
      dplyr::summarise(AGEGR = AGEGR[1],
                       Group = "Global",
                       Impute.x = Impute.x[1],
                       Impute.y = Impute.y[1],
                       n.all = sum(n.all),
                       n.impute = sum(n.impute),
                       N = sum(N),
                       N_prop = sum(N_prop),
                       Pop = sum(Pop)
      )
    df2 <- df2 %>% group_by(AGEGR, Group, Impute.x, Impute.y, n.all, n.impute) %>%
      dplyr::summarise(N.est = round(median(N_prop),3),
                       N.lci = round(quantile(N_prop, 0.025),3),
                       N.uci = round(quantile(N_prop, 0.975),3),
                       Pop = Pop[1],
                       prop.est = median(N_prop/N)*100,
                       prop.lci = quantile(N_prop/N, 0.025)*100,
                       prop.uci = quantile(N_prop/N, 0.975)*100
      )
    df2$IR.est <- with(df2, N.est/Pop*1000)
    df2$IR.lci <- with(df2, N.lci/Pop*1000)
    df2$IR.uci <- with(df2, N.uci/Pop*1000)
    return(rbind(df1, df2))
  }else{
    return(df1)
  }
}
# n.level - specify the number of levels expected (e.g. 2 for country development status and 4 for country income classifiation)
