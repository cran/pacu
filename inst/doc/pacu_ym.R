## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(sf)
library(pacu)

## ----reading-the-data---------------------------------------------------------
extd.dir <- system.file("extdata", package = "pacu")
raw.yield <- st_read(file.path(extd.dir, '2012-basswood.shp'), quiet = TRUE)
boundary <- st_read(file.path(extd.dir, 'boundary.shp'), quiet = TRUE)

## -----------------------------------------------------------------------------
names(raw.yield)

## ----plotting-the-raw-data,  fig.width=6, fig.height=5------------------------
cols <- function(n) hcl.colors(n, 'Temps', rev = TRUE)
plot(raw.yield['DRY_BU_AC'], pal = cols)

## ----boxplot-raw-yield, fig.width=6, fig.height=5-----------------------------
boxplot(raw.yield$DRY_BU_AC)

## ----check-yield--------------------------------------------------------------
chk <- pa_check_yield(input = raw.yield,
               algorithm = 'all')
chk


## ----example-missing-col------------------------------------------------------
toy.example <- raw.yield 
names(toy.example) <- gsub('DRY_BU_AC', 'NOT_A_COMMON_NAME', names(toy.example))
chk <- pa_check_yield(toy.example, algorithm = 'simple')
chk

## -----------------------------------------------------------------------------
names(toy.example) <- gsub('NOT_A_COMMON_NAME', 'yield', names(toy.example))
chk <- pa_check_yield(toy.example, algorithm = 'simple')
chk

## ----first-ymp, message=FALSE-------------------------------------------------
ymp1 <- pa_yield(input = raw.yield,
                 boundary = boundary,
                 algorithm = 'simple',
                 lbs.per.bushel = 56,
                 unit.system = 'metric',
                 verbose = FALSE)

## ----first-ymp-attr-----------------------------------------------------------
ymp1

## ----plotting-first-ymp, fig.width=6, fig.height=5----------------------------
pa_plot(ymp1)

## ----ymp2, message=FALSE------------------------------------------------------
ymp2 <- pa_yield(input = raw.yield,
                 boundary = boundary,
                 algorithm = 'simple',
                 unit.system = 'standard',
                 moisture.adj = 15.5,
                 lbs.per.bushel = 56,
                 verbose = FALSE)

## ----ymp2-attr----------------------------------------------------------------
ymp2

## ----plotting-ymp2, fig.width=6, fig.height=5---------------------------------
pa_plot(ymp2)

## ----ymp3, message=FALSE------------------------------------------------------
ymp3 <- pa_yield(input = raw.yield,
                 boundary = boundary,
                 algorithm = 'simple',
                 unit.system = 'metric',
                 clean = TRUE,
                 clean.sd = 3,
                 lbs.per.bushel = 56,
                 verbose = FALSE)

## ----plotting-ymp3, fig.width=6, fig.height=5---------------------------------
pa_plot(ymp3)

## ----ymp4, message=FALSE------------------------------------------------------
ymp4 <- pa_yield(input = raw.yield,
                 boundary = boundary, 
                 algorithm = 'simple',
                 unit.system = 'metric',
                 clean = TRUE,
                 clean.sd = 3,
                 smooth.method = 'idw',
                 lbs.per.bushel = 56,
                 verbose = FALSE)

## ----plotting-ymp4, fig.width=6, fig.height=5---------------------------------
ymp4
pa_plot(ymp4)

## ----ymp5, eval = FALSE-------------------------------------------------------
# ymp5 <- pa_yield(input = raw.yield,
#                  boundary = boundary,
#                  algorithm = 'simple',
#                  unit.system = 'metric',
#                  clean = TRUE,
#                  clean.sd = 3,
#                  smooth.method = 'krige',
#                  lbs.per.bushel = 56,
#                  verbose = FALSE,
#                  maxdist = 50)

## ----include=FALSE------------------------------------------------------------
extd.dir <- system.file("extdata", package = "pacu")
ymp5 <- readRDS(file.path(extd.dir, 'yield-map-5.rds'))

## ----plotting-ymp5, fig.width=6, fig.height=5---------------------------------
ymp5
pa_plot(ymp5, plot.var = 'yield')

## ----plot-ymp5-variogram, fig.width=6, fig.height=5---------------------------
pa_plot(ymp5, plot.type = 'variogram')

## ----ritas-initial-example, eval =FALSE---------------------------------------
# ymp6 <- pa_yield(input = raw.yield,
#                  algorithm = 'ritas',
#                  lbs.per.bushel = 56,
#                  unit.system = 'metric',
#                  verbose = FALSE)

## ----ritas-sp-units, eval=FALSE-----------------------------------------------
# ymp6 <- pa_yield(input = raw.yield,
#                  data.columns = c(flow = 'FLOW', moisture = 'MOISTURE', interval = 'CYCLES', width = 'SWATH', distance = 'DISTANCE'),
#                  data.units = c(flow = 'lb/s', moisture = '%', interval = 's', width = 'in', distance = 'in'),
#                  unit.system = 'metric',
#                  algorithm = 'ritas',
#                  verbose = FALSE)
# 

## ----include=FALSE------------------------------------------------------------
ymp6 <- readRDS(file.path(extd.dir, 'yield-map-6.rds'))

## ----plotting-ymp6, fig.width=6, fig.height=5---------------------------------
pa_plot(ymp6)

## ----ritas-final, eval=FALSE--------------------------------------------------
# ymp7 <- pa_yield(input = raw.yield,
#                  boundary = boundary,
#                  algorithm = 'ritas',
#                  smooth.method = 'krige',
#                  unit.system = 'metric',
#                  lbs.per.bushel = 56,
#                  verbose = FALSE,
#                  maxdist = 50)
# 

## ----include=FALSE------------------------------------------------------------
ymp7 <- readRDS(file.path(extd.dir, 'yield-map-7.rds'))

## ----plotting-ymp7, fig.width=6, fig.height=5---------------------------------
ymp7
pa_plot(ymp7)

