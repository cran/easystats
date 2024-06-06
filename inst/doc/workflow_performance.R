## ----include=FALSE------------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  dpi = 300,
  fig.width = 7,
  fig.height = 5,
  out.width = "100%",
  out.height = "100%",
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = TRUE
)
options(knitr.kable.NA = "")
options(digits = 2)

pkgs <- c("DHARMa", "glmmTMB", "patchwork")
successfully_loaded <- vapply(pkgs, requireNamespace, FUN.VALUE = logical(1L), quietly = TRUE)
can_evaluate <- all(successfully_loaded)

if (can_evaluate) {
  knitr::opts_chunk$set(eval = TRUE)
  vapply(pkgs, require, FUN.VALUE = logical(1L), quietly = TRUE, character.only = TRUE)
} else {
  knitr::opts_chunk$set(eval = FALSE)
}

## -----------------------------------------------------------------------------
library(performance)
model1 <- glmmTMB::glmmTMB(
  count ~ mined + spp + (1 | site),
  family = poisson,
  data = glmmTMB::Salamanders
)

## -----------------------------------------------------------------------------
library(parameters)
model_parameters(model1)

## ----fig.height=12, fig.width=10----------------------------------------------
check_model(model1)

## -----------------------------------------------------------------------------
check_overdispersion(model1)

check_zeroinflation(model1)

## ----fig.height=12, fig.width=10----------------------------------------------
model2 <- glmmTMB::glmmTMB(
  count ~ mined + spp + (1 | site),
  ziformula = ~ mined + spp,
  family = poisson,
  data = glmmTMB::Salamanders
)
check_model(model2)

## -----------------------------------------------------------------------------
check_overdispersion(model2)

check_zeroinflation(model2)

## ----fig.height=12, fig.width=10----------------------------------------------
model3 <- glmmTMB::glmmTMB(
  count ~ mined + spp + (1 | site),
  ziformula = ~ mined + spp,
  family = glmmTMB::nbinom1,
  data = glmmTMB::Salamanders
)
check_model(model3)

## -----------------------------------------------------------------------------
check_overdispersion(model3)

check_zeroinflation(model3)

## -----------------------------------------------------------------------------
result <- compare_performance(
  model1, model2, model3,
  metrics = c("AIC", "AICc", "BIC", "SCORE")
)
result

plot(result)

## -----------------------------------------------------------------------------
test_performance(model1, model2, model3)

## -----------------------------------------------------------------------------
test_performance(model2, model3)

test_likelihoodratio(model2, model3)

## -----------------------------------------------------------------------------
model_parameters(model3)

## ----fig.height=12, fig.width=10----------------------------------------------
model4 <- glmmTMB::glmmTMB(
  count ~ mined + spp + (1 | site),
  ziformula = ~ mined + spp,
  family = glmmTMB::nbinom2,
  data = glmmTMB::Salamanders
)
check_model(model4)

check_overdispersion(model4)

check_zeroinflation(model4)

test_likelihoodratio(model3, model4)

model_parameters(model4)

