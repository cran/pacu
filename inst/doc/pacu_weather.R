## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(apsimx)
library(pacu)

## ----download-weather, eval = FALSE-------------------------------------------
# weather.met <- pa_get_weather_sf(area.of.interest, '1990-01-01', '2020-12-31')

## ----include=FALSE------------------------------------------------------------
extd.dir <- system.file("extdata", package = "pacu")
weather.met <- read_apsim_met('example-weather.met', extd.dir, verbose = FALSE)

## ----simple-met-plot, fig.width=6---------------------------------------------
## Precipitation (or rain)
plot(weather.met, met.var = "rain", cumulative = TRUE, 
     climatology = TRUE, years = 2017:2020)
## Temperature
plot(weather.met, cumulative = TRUE, 
     climatology = TRUE, years = 2017:2020)

## ----summary-weather-met------------------------------------------------------
## Selecting just a few columns (1, 6, 7, 10) for simplicity
summary(weather.met, years = 2017:2020)[, c(1, 6, 7, 10)]

## ----summarizing-weather-data, fig.width=6, fig.height=5----------------------
pa_plot(weather.met,
        plot.type = 'climate_normals', 
        unit.system = 'int')
pa_plot(weather.met,
        plot.type = 'monthly_distributions', 
        unit.system = 'int', months = 5:10)



