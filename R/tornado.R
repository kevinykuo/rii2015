library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(xts)
library(dygraphs)

# list.files("download/")
# read_csv("download/stormdata_2010.csv") %>%
#   glimpse

colsToExtract <- c("BEGIN_YEARMONTH", "BEGIN_DAY", "EVENT_TYPE", "STATE",
                   "DEATHS_DIRECT", "DEATHS_INDIRECT", "BEGIN_LAT", "BEGIN_LON",
                   "CZ_NAME")
stormData <- list.files("download/") %>%
  grep("^storm", ., value = TRUE) %>%
  lapply((function(x) read_csv(paste0("download/", x)) %>%
            select_(~ one_of(colsToExtract)) %>%
            transmute_(date = ~ paste(BEGIN_YEARMONTH, BEGIN_DAY, sep = "-") %>%
                         as.POSIXct(format = "%Y%m-%d", tz = "GMT"),
                       type = ~ EVENT_TYPE,
                       state = ~ STATE,
                       deaths = ~ DEATHS_DIRECT + DEATHS_INDIRECT,
                       lat = ~ as.integer(BEGIN_LAT),
                       long = ~ as.integer(BEGIN_LON),
                       county = ~ paste(CZ_NAME, STATE, sep = ", ")))
         ) %>%
  bind_rows

stormTS <- stormData %>%
  filter(type %in% c("Tornado")) %>%
  group_by(date, type) %>%
  summarize(count =  n()) %>%
  spread(type, count) %>%
  (function(x) xts(x[, -1], order.by = x[[1]]))

# dygraph(stormTS) %>%
#   dyOptions(stackedGraph = TRUE)

outbreakSummary <- stormData %>%
  filter(type == "Tornado",
         date >= ymd("2011-4-25"), date <= ymd("2011-4-28")) %>%
  group_by(state) %>%
  summarize(count = n(),
            deaths = sum(deaths)) %>%
  arrange(desc(count)) %>%
  head


library(leaflet)
tornadoMapData <- stormData %>%
  filter(type == "Tornado", deaths > 0,
         date >= ymd("2011-4-25"), date <= ymd("2011-4-28")) %>%
  group_by(lat, long) %>%
  summarize(deaths = sum(deaths),
            county = first(county))
deathsPopup <- paste0("<strong>Deaths: </strong>", 
                      tornadoMapData$deaths, "<br />",
                      tornadoMapData$county)
# tornadoMapData %>%
#   leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(lng = ~ long, lat = ~ lat, radius = ~ deaths,
#                    fillOpacity = 0.2, color = "red", stroke = FALSE, popup = deathsPopup)
