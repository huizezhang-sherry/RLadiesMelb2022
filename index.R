## ----setup-----------------------------------------------------------------
library(tidyverse)
library(ozmaps)
library(sf)
library(tsibble)
# remotes::install_github("https://github.com/huizezhang-sherry/cubble")
library(cubble)


## ----------------------------set up data needed-------------------------------
stations_sf <- cubble::climate_subset %>% select(-ts) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4283, remove = FALSE)

oz <- ozmaps::abs_ste %>% filter(NAME != "Other Territories")
oz_simp <- oz %>% rmapshaper::ms_simplify(keep = 0.05) 
load(here::here("data/ts.rda"))

## --------------------------------------------------------------------------
dt <- stations_sf %>% head(2)

dt # simple feature (sf)
pnt_sfc <- dt$geometry # simple feature column (sfc)
pnt_sfg <- pnt_sfc[[1]] # simple feature geometry (sfg) - POINTS

# simple feature geometry (sfg) - POINTS
pnt_sfg 
typeof(pnt_sfg) 
unclass(pnt_sfg)
attributes(pnt_sfg)

# simple feature column (sfc)
pnt_sfc 
typeof(pnt_sfc)
names(attributes(pnt_sfc))
unclass(pnt_sfc)[1:2]

# LINESTRING
ls_sfc <- oz_simp[8,]$geometry %>% st_cast("POLYGON") %>% st_cast("LINESTRING")
(ls_sfg <- ls_sfc[[1]])
typeof(ls_sfg)
unclass(ls_sfg)

# POLYGON
pol_sfc <- oz_simp[8,]$geometry %>% st_cast("POLYGON")
(pol_sfg <- pol_sfc[[1]])
typeof(pol_sfg)
unclass(pol_sfg)

# MULTIPOLYGON
mpol_sfc <- rmapshaper::ms_simplify(oz_simp[2,]$geometry, keep = 0.4)
(mpol_sfg <- mpol_sfc[[1]])
typeof(mpol_sfg)
length(unclass(mpol_sfg))
unclass(mpol_sfg)

## ---------------------Aggregate time series with tsibble---------------------
ts %>% 
  ggplot(aes(x = date, y = tmax, group = id)) + 
  geom_line(alpha = 0.6) + 
  theme_bw()

missings <- missings %>% group_by(id) %>% mutate(.n = sum(.n))
missings %>%
  ggplot() +
  geom_errorbar(
    aes(xmin = .from, xmax = .to,
        y = fct_reorder(id, .n)),
    width = 0.2) +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylab("Station ID")

# fill gaps
single <- ts_tsibble %>% filter(id == "ASN00003057")
single_filled <- single %>% tsibble::fill_gaps()
single %>% filter(date == "2020-02-17") %>% pull(tmax)
single_filled %>% filter(date == "2020-02-17") %>% pull(tmax)

# summarise daily data
ts %>% 
  group_by(month = lubridate::month(date), id) %>% 
  summarise(tmax = mean(tmax, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = tmax, group = id)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1, 12, 1)) + 
  theme_bw()

## ------------------------Space and time at the same time --------------------
cb <- as_cubble(
  list(spatial = stations_sf, temporal = ts),
  key = id, index = date, coords = c(long, lat)
)

set.seed(0927)
cb_space <- cb %>% slice_sample(n = 20)

cb_tm <- cb_space %>% 
  face_temporal() %>% 
  group_by(month = lubridate::month(date)) %>% 
  summarise(tmax = mean(tmax, na.rm = TRUE))

cb_glyph <- cb_tm %>% unfold(long, lat)

ggplot() + 
  geom_sf(data = oz_simp, fill = "grey95", color = "white") +
  geom_glyph(
    data = cb_glyph,
    aes(x_major = long, x_minor = month, y_major = lat, y_minor = tmax),
    width = 2, height = 0.7) + 
  ggthemes::theme_map()

