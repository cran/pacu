## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(sf)
library(pacu)

## ----initializing-data-space, eval = FALSE------------------------------------
# un <- 'my-username'
# pw <- 'my-password'
# pa_initialize_dataspace(username = un, password = pw)

## ----defining-area-of-interest------------------------------------------------
extd.dir <- system.file("extdata", package = "pacu")
area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'), quiet = TRUE)

## ----browsing-data-space, eval = FALSE----------------------------------------
# available.images <- pa_browse_dataspace(aoi = area.of.interest,
#                                         max.cloud.cover = 50,
#                                         start.date = '2023-01-01',
#                                         end.date = '2023-12-31',
#                                         collection.name = 'SENTINEL-2')
# 

## ----reading-browse-ds, include=FALSE-----------------------------------------
available.images <- readRDS(file.path(extd.dir, 'ds-browse-object.rds'))

## ----inspecting-browse-ds-----------------------------------------------------
available.images

## ----summary-browse-ds--------------------------------------------------------
summary(available.images)

## ----downloading-an-image, eval=FALSE-----------------------------------------
# out.dir <- tempdir()
# available.images <- available.images[32:33, ]
# pa_download_dataspace(x = available.images,
#                       dir.path = out.dir,
#                       aoi = area.of.interest,
#                       verbose = FALSE)

## ----out-equal-ext, include=FALSE---------------------------------------------
out.dir <- extd.dir

## ----rgb-img, fig.width=7, fig.height=5---------------------------------------
s2.files <- list.files(out.dir, '\\.zip', full.names = TRUE)
rgb.img <- pa_get_rgb(s2.files,
                      aoi = area.of.interest,
                      verbose = FALSE)

pa_plot(rgb.img)

## ----ndvi-img,  fig.width=7, fig.height=5-------------------------------------
ndvi.img <- pa_compute_vi(s2.files,
                          vi = 'ndvi', 
                          aoi = area.of.interest,
                          verbose = FALSE)

ndre.img <- pa_compute_vi(s2.files, 
                          vi = 'ndre',
                          aoi = area.of.interest,
                          verbose = FALSE)

pa_plot(ndvi.img, main = 'NDVI')
pa_plot(ndre.img, main = 'NDRE')

## ----ndvi-img-summary,  fig.width=7, fig.height=5-----------------------------
split.aoi <- st_make_grid(area.of.interest, n = c(4, 1))
split.aoi <- st_as_sf(split.aoi)
split.aoi <- st_transform(split.aoi, st_crs(ndvi.img))
ndvi.mean <- summary(ndvi.img, by = split.aoi, fun = mean)
pa_plot(ndvi.mean)

## ----registering-oauth, eval = FALSE------------------------------------------
# cid <- 'my-client-id'
# cs <- 'my-client-secret'
# pa_initialize_oauth(client_id = cid, client_secret = cs)

## ----requesting-ndvi-statistics, eval = FALSE---------------------------------
# ndvi.statistics <- pa_get_vi_stats(aoi = area.of.interest,
#                                    start.date = '2022-01-01',
#                                    end.date = '2022-12-31',
#                                    vegetation.index = 'ndvi',
#                                    agg.time = 'P1D')

## ----reading-ndvi-statistics, include = FALSE---------------------------------
ndvi.statistics <- readRDS(file.path(extd.dir, 'example-ndvi-stats.rds'))

## ----plotting-ndvi-statistics-paplot, fig.width=7, fig.height=5---------------
pa_plot(ndvi.statistics)

## ----vis-ndvi-stats, fig.width=7, fig.height = 5------------------------------
pa_plot(ndvi.statistics, 
        plot.type = 'timeseries')

## ----requesting-ndre-statistics, eval = FALSE---------------------------------
# ndvi.statistics.2 <- pa_get_vi_stats(aoi = split.aoi,
#                                      start.date = '2022-01-01',
#                                      end.date = '2022-05-01',
#                                      vegetation.index = 'ndvi',
#                                      agg.time = 'P1D',
#                                      by.feature = TRUE)

## ----reading-ndre-statistics, include = FALSE---------------------------------
ndvi.statistics.2 <- readRDS(file.path(extd.dir, 'example-ndvi-stats-2.rds'))

## ----plotting-ndvi-statistics-multiple, fig.width=7, fig.height=5-------------
pa_plot(ndvi.statistics.2)

## ----plotting-ndvi-statistics-multiple-timeseries, fig.width=7, fig.height=5----
pa_plot(ndvi.statistics.2, 
        plot.type = 'timeseries',
        by = c('id'),
        legend.outside = TRUE)

