% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flex_co.R
\name{flex_co}
\alias{flex_co}
\title{Obtain flexible cutoffs for one or two models}
\usage{
flex_co(fits, index, alpha.lev = 0.05, gof = NULL)
}
\arguments{
\item{fits}{A list of simulated fit indices obtained from gen_fit. Based on the structure of fits, the number of models is derived.}

\item{index}{A vector of fit indices or measures provided by function fitmeasures in package lavaan}

\item{alpha.lev}{The predefined uncertainty
For example, if the default uncertainty of .05 (5 percent) is accepted a-priori, the 5 percent stats::quantile (of type 8, see ?stats::quantile) of the simulated distribution for correctly specified CFA models with the given model and sample characteristics determines the flexible cutoff.
Options are .001, .01, .05, and .10. Higher values are more conservative.}

\item{gof}{An optional vector as to whether the indices are GoF (Goodness-of-Fit index)? If TRUE, a GoF is assumed. If FALSE, a BoF is assumed.
Depending on the nature of the underlying fit index, the appropriate lower (GoF) or upper (BoF) width of the respective confidence interval as defined by the stats::quantile is used to derive the flexible cutoff.
If not provided or not equal to the number of fit indices, the function guesses the type for known fit indices (e.g., SRMR is a BoF).}
}
\value{
A list of information regarding the selected fit index providing its flexible cutoff for the given parameters.
}
\description{
Obtain flexible cutoffs for one or two models
}
\examples{
#Note: Demonstration only! Please use higher numbers of replications for your applications (>= 500).
#A single model to obtain fit indices for
mod <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
"
fits.single <- gen_fit(mod1 = mod, x = bb1992, rep = 10, standardized = FALSE)
flex_co(fits = fits.single, index = c("CFI", "SRMR"))
\donttest{
#Two models, an unconstrained and a constrained model to compare fit indices
mod.con <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
F1 ~~ 0 * F2
"
fits.con <- gen_fit(
 mod1 = mod,
 mod2 = mod.con,
 x = bb1992,
 rep = 10
)
flex_co(fits = fits.con,
       index = c("CFI", "SRMR"),
       alpha.lev = .05)

#Two models for discriminant validity testing, this resembles constraining with a cutoff of .9
fits.dv.con <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 10,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 dv.cutoff = .9
)
flex_co(fits = fits.dv.con,
index = "CFI",
alpha.lev = .05)
mod.dv.con <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17
F5 =~ Q6 + Q14 + Q15 + Q16
F4 ~~ .9 * F5
"
lavaan::fitmeasures(
 lavaan::cfa(
   model = mod.dv.con,
   data = bb1992,
   auto.fix.first = FALSE,
   std.lv = TRUE
 ),
 fit.measures = "cfi"
)
#Two models for discriminant validity testing, this resembles merging.
fits.dv.merge <- gen_fit(
 mod1 = mod,
 x = bb1992,
 rep = 10,
 dv = TRUE,
 dv.factors = c("F4", "F5"),
 merge.mod = TRUE)

flex_co(fits = fits.dv.merge,
index = "CFI",
alpha.lev = .05)
mod.dv.merge <- "
F1 =~ Q5 + Q7 + Q8
F2 =~ Q2 + Q4
F3 =~ Q10 + Q11 + Q12 + Q13 + Q18 + Q19 + Q20 + Q21 + Q22
F4 =~ Q1 + Q17 + Q6 + Q14 + Q15 + Q16
"
lavaan::fitmeasures(
 lavaan::cfa(
   model = mod.dv.merge,
   data = bb1992
 ),
 fit.measures = "cfi"
)
}
}
