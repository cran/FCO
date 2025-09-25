## ----echo=F-------------------------------------------------------------------
require(lavaan)
require(FCO)
require(dplyr)

## ----setup--------------------------------------------------------------------
library(FCO)

## ----fit, eval=FALSE----------------------------------------------------------
#  library(lavaan)
#  library(dplyr)
#  HS.model <- ' visual  =~ x1 + x2 + x3
#                textual =~ x4 + x5 + x6
#                speed   =~ x7 + x8 + x9 '
#  
#  fit <- cfa(
#    HS.model,
#    data = HolzingerSwineford1939
#  )
#  
#  fitmeasures(fit)

## ----simulation 1, eval=FALSE-------------------------------------------------
#  #Note: Demonstration only! Please use higher numbers of replications for your applications (>= 500).
#  fits <- gen_fit2(fit = fit, rep = 100)

## ----inspect and plot, eval=FALSE---------------------------------------------
#  flex_co2(fits)
#  plot_fit2(fits)

## ----population models, eval=FALSE--------------------------------------------
#  FCO:::pop_mod(mod = HS.model, x = lavaan::HolzingerSwineford1939)$pop.mod
#  FCO:::pop_mod(mod = HS.model, x = lavaan::HolzingerSwineford1939, type = "HB")$pop.mod
#  FCO:::pop_mod(mod = HS.model, x = lavaan::HolzingerSwineford1939, type = "EM")$pop.mod

## ----regression model, eval=FALSE---------------------------------------------
#  cmod <- "F1 =~ x1 + x2 + x3
#            F2 =~ x4 + x5 + x6
#            F3 =~ x7 + x8 + x9
#            F3 ~ F1 + F2
#            F1 ~~ .0 * F2"
#  
#  fit2 <- sem(model = cmod, data = lavaan::HolzingerSwineford1939)
#  
#  fitmeasures(fit2)
#  
#  fits2 <- gen_fit2(fit = fit2, cfa = FALSE, type = "NM", es.f2 = "moderate", rep = 100)
#  
#  flex_co2(fits2)

## ----regression pop.mod, eval=FALSE-------------------------------------------
#  FCO:::pop_mod_reg(mod = cmod, x = lavaan::HolzingerSwineford1939, type = "NM", es.f2 = "low")$pop.mod
#  FCO:::pop_mod_reg(mod = cmod, x = lavaan::HolzingerSwineford1939, type = "NM", es.f2 = "moderate")$pop.mod
#  FCO:::pop_mod_reg(mod = cmod, x = lavaan::HolzingerSwineford1939, type = "NM", es.f2 = "large")$pop.mod

## ----data types, eval=FALSE---------------------------------------------------
#  dat <- lavaan::HolzingerSwineford1939
#  cdat <- dplyr::select(dat, x1, x2, x3, x4, x5, x6, x7, x8, x9)
#  #For demo purposes, some variables are changed:
#  cdat <- cdat %>% mutate(
#    x1 = round(x1, digits = 0),
#    x3 = round(x3, digits = 0),
#    x2 = ifelse(x2 > 4, 1, 0),
#    x4 = ifelse(x4 > 2, 1, 0),
#    x5 = round(x5, digits = 0),
#    x8 = round(x8, digits = 0)
#  )
#  cfit <- lavaan::cfa(model = HS.model, data = cdat)
#  cfits <- gen_fit2(fit = cfit,
#                    data.types = c("C", "B", "C", "B", "O", "N", "N", "O", "N"), rep = 100)
#  flex_co2(cfits)
#  plot_fit2(cfits)

## ----mc, eval=FALSE-----------------------------------------------------------
#  gen_fit2(fit, cores = 4, rep = 100)
#  gen_fit2(fit, cores = 1, rep = 100)

## ----decision rules, eval=FALSE-----------------------------------------------
#  #Default evaluation:
#  flex_co2(fits)
#  #Changed alpha and beta values:
#  flex_co2(fits, alpha = .05, beta = .05)
#  flex_co2(fits, alpha = .10, beta = .20)
#  #Different fit indices:
#  flex_co2(fits, index = c("CFI", "SRMR", "RMSEA"))

## ----plotting, eval=FALSE-----------------------------------------------------
#  #Default plot:
#  plot_fit2(fits)
#  #Changed alpha and beta values:
#  plot_fit2(fits, alpha = .05, beta = .05)
#  plot_fit2(fits, alpha = .10, beta = .20)
#  #Different fit indices:
#  plot_fit2(fits, index = c("CFI", "SRMR", "RMSEA"))

## ----recommend1, eval=FALSE---------------------------------------------------
#  data(bb1992)
#  mod <- "
#  F1 =~ Q5 + Q7 + Q8
#  F2 =~ Q2 + Q4
#  F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#  F4 =~ Q1 + Q17
#  F5 =~ Q6 + Q14 + Q15 + Q16
#  "
#  fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 100)
#  recommend(fits.single)

## ----recommend2, eval=FALSE---------------------------------------------------
#  recommend(fits.single, purpose = "established")

## ----recommend3, eval=FALSE---------------------------------------------------
#  recommend(fits.single, override = TRUE, index = c("CFI", "SRMR"))

## ----constrained model, eval=FALSE--------------------------------------------
#  data(bb1992)
#  mod <- "
#  F1 =~ Q5 + Q7 + Q8
#  F2 =~ Q2 + Q4
#  F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#  F4 =~ Q1 + Q17
#  F5 =~ Q6 + Q14 + Q15 + Q16
#  "
#  
#  mod.con <- "
#  F1 =~ Q5 + Q7 + Q8
#  F2 =~ Q2 + Q4
#  F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#  F4 =~ Q1 + Q17
#  F5 =~ Q6 + Q14 + Q15 + Q16
#  F1 ~~ 0 * F2
#  "
#  
#  fits.con <- gen_fit(mod1 = mod, mod2 = mod.con, x = bb1992, rep = 100)
#  
#  flex_co(fits = fits.con, index = c("CFI", "SRMR"), alpha.lev = .05)
#  
#  fitmeasures(cfa(model = mod, data = bb1992), fit.measures = c("cfi", "srmr")) - fitmeasures(cfa(model = mod.con, data = bb1992), fit.measures = c("cfi", "srmr"))

## ----constrain merge, eval=FALSE----------------------------------------------
#  data(bb1992)
#  mod <- "
#  F1 =~ Q5 + Q7 + Q8
#  F2 =~ Q2 + Q4
#  F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#  F4 =~ Q1 + Q17
#  F5 =~ Q6 + Q14 + Q15 + Q16
#  "
#  
#  fits.dv.con <- gen_fit(mod1 = mod, x = bb1992, rep = 100, dv = TRUE, dv.factors = c("F4", "F5"), dv.cutoff = .9)
#  
#  fits.dv.merge <- gen_fit(mod1 = mod, x = bb1992, rep = 100, dv = TRUE, dv.factors = c("F4", "F5"), merge.mod = TRUE)
#  
#  flex_co(fits = fits.dv.con, index = "CFI", alpha.lev = .05)
#  flex_co(fits = fits.dv.merge, index = "CFI", alpha.lev = .05)
#  
#  mod.dv.con <- "
#  F1 =~ Q5 + Q7 + Q8
#  F2 =~ Q2 + Q4
#  F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#  F4 =~ Q1 + Q17
#  F5 =~ Q6 + Q14 + Q15 + Q16
#  F4 ~~ .9 * F5
#  "
#  
#  fitmeasures(cfa(model = mod.dv.con, data = bb1992, auto.fix.first = FALSE, std.lv = TRUE), fit.measures = "cfi")
#  
#  mod.dv.merge <- "
#  F1 =~ Q5 + Q7 + Q8
#  F2 =~ Q2 + Q4
#  F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
#  F4 =~ Q1 + Q17 + Q6 + Q14 + Q15 + Q16
#  "
#  
#  fitmeasures(cfa(model = mod.dv.merge, data = bb1992), fit.measures = "cfi")

## ----recommend_dv, eval=FALSE-------------------------------------------------
#  recommend_dv(fits.dv.con)
#  recommend_dv(fits.dv.merge)

