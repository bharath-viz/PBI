library(dplyr)
library(survival)
library(survminer)

set.seed(10)


tte <- data.frame(
  USUBJID = paste0("phuse23", sample(x = 100:200, 65)),
  TRT = sample(c("TRT01", "TRT02", "TRT03"), 65, replace = T),
  AGE = floor(runif(65,45,75)),
  SEX = sample(c("Male", "Female"), 65, T),
  RACE = sample(c(
    "WHITE", "Asian", "Black/African American"
  ), 65, T),
  BOR = sample(c("CR", "PR", "SD", "PD", "NA"), 65, T),
  AVAL = floor(runif(65,15,200)),
  PARAMCD = "PFS",
  PARAM="Progression-free Survival",
  CENSOR = sample(c(1,2), 65, T),
  SAFFL=sample(c("Y", "N"), 65, T)
) 
write.csv(tte,"C:\\Users\\euzln\\OneDrive - Bayer\\Documents\\PBI Projects\\phuse 2023\\tte.csv")

#strata for Power Bi#
strata <- setdiff(colnames(dataset),c("AVAL","CENSOR"))
#strata for R studio#
strata <- "SEX" 
dataset <- tte #name of data is always dataset in PBI#
surv_object <- surv_fit(
  as.formula(paste0("Surv(AVAL,CENSOR)~", strata)),
  data = dataset ,
  conf.type = "log-log",
  conf.int = 0.95
)
ggsurvplot(
  surv_object,                     # survfit object with calculated statistics.
  data = dataset,             # data used to fit survival curves.
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  xlim = c(0,200),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 25,     # break X axis in time intervals by 500.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.height = 0.4, # the height of the risk table
  risk.table.y.text = FALSE,# show bars instead of names in text annotation 
  conf.int.style = "step",  # customize style of confidence intervals
  surv.median.line = "hv",  # add the median survival pointer.
)
####COX#####################################
dataset <- tte #name of data is always dataset in PBI#
covariates <-  setdiff(colnames(dataset), c("AVAL", "CENSOR", "SEQ"))#for PBI#
covariates <- c("SEX", "RACE", "AGE")

ggforest(coxph(as.formula(paste(
  "Surv(AVAL, CENSOR) ~", paste(covariates, collapse = "+")
)),
data = dataset),
data = dataset,
fontsize = 1)

