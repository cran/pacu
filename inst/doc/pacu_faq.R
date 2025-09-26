## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(pacu)

## ----message = F--------------------------------------------------------------
x <- 40:45
y <- -90:-95
yield <- 100:105
dat <- data.frame(x = x, 
                  y = y,
                  yield = yield)
class(dat) ## data.frame

dat <- sf::st_as_sf(dat,
             coords = c('x', 'y')) 
class(dat) ## sf and data.frame


## -----------------------------------------------------------------------------
pacu_options(suppress.warnings = TRUE,
             suppress.messages = TRUE)

