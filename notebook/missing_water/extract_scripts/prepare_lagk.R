## ---- messages=FALSE-----------------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)

## ------------------------------------------------------------------------
hourly.df <- data.table::fread("H:/Projects/COOP Data/flows/usgs/hourly/hourly_flows.csv",
                               data.table = FALSE,
                               na.strings = c("", " ", "Eqp", "#N/A", "-999999"),
                               showProgress = FALSE) %>% 
  filter(site %in% c("goose", "lfalls", "mon_jug", "por", "seneca")) %>% 
  mutate(date_time = ymd_hms(date_time)) %>% 
  filter(!is.na(flow))

## ------------------------------------------------------------------------
klag.df <- file.path(rprojroot::find_rstudio_root_file(), "data/parameters/k_lag.csv") %>% 
  data.table::fread(data.table = FALSE) %>% 
  rename_all(tolower) %>% 
  mutate(site = tolower(site))

## ------------------------------------------------------------------------
source(file.path(rprojroot::find_rstudio_root_file(), "functions/variable_lagk_func.R"))

## ------------------------------------------------------------------------
date_frame <- function(start.date, end.date, seq.by = "hour") {
  
  data.frame(date_time = seq.POSIXt(start.date, end.date, by = seq.by))
}

## ------------------------------------------------------------------------
variable_confluence <- function(long1.df, gage1, lag1,
                                long2.df, gage2, lag2, klag.df) {
  g1.df <- variable_lagk(long1.df, gage1, lag1, klag.df)
  g2.df <- variable_lagk(long2.df, gage2, lag2, klag.df)
  final.df <- bind_rows(long2.df, g1.df, g2.df) %>% 
    group_by(date_time, site) %>% 
    summarize(flow = sum(flow)) %>% 
    ungroup()
  return(final.df)
}

## ------------------------------------------------------------------------
pred.df <- variable_confluence(hourly.df, "por", "por_1", 
                               hourly.df, "mon_jug", "mon_jug", klag.df) %>% 
  variable_confluence("predicted", "por_2",
                      hourly.df, "goose", "goose", klag.df) %>% 
  variable_confluence("predicted", "por_3",
                      hourly.df, "seneca", "seneca", klag.df) %>% 
  variable_lagk("predicted", "por_4", klag.df)

## ------------------------------------------------------------------------
lagk.df <- bind_rows(hourly.df, pred.df)

## ------------------------------------------------------------------------
with.df <- data.table::fread("D:/ZSmith/Projects/COOP/lagk/lagk/data_ts/current/withdrawals/withdrawals_wma_daily_mgd_1990_2017.csv",
                      data.table = FALSE,
                      na.strings = c("", " ", "Eqp", "#N/A", "-999999")) %>% 
    dplyr::filter(!rowSums(is.na(.)) == ncol(.))

## ------------------------------------------------------------------------
 pot.total <- with.df %>% 
    dplyr::filter(location == "Potomac River"#,
                  #day == "yesterday",
                  #measurement == "daily average withdrawals"
    ) %>% 
    dplyr::group_by(measurement, date_time, units) %>% 
    dplyr::summarize(value = sum(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(unique_id = "potomac_total") %>% 
    dplyr::filter(!rowSums(is.na(.)) == ncol(.))

## ------------------------------------------------------------------------
withdrawals.df <- dplyr::bind_rows(with.df, pot.total) %>% 
    dplyr::rename(site = unique_id,
                  flow = value) %>% 
    #    dplyr::mutate(date_time = as.Date(date_time, "%m/%d/%Y"))
    dplyr::mutate(date_time = as.Date(date_time, "%Y-%m-%d")) %>% 
    dplyr::filter(!stringr::str_detect(site, "usable storage|usable capacity"))

## ------------------------------------------------------------------------
withdrawals.sub <- withdrawals.df %>% 
  # Yesterday or Today??????????????????????????????????????????????????????????????????????
  dplyr::filter(measurement == "daily average withdrawals",
                day == "yesterday") %>% 
  dplyr::select(date_time, site, flow) %>% 
  tidyr::spread(site, flow) %>% 
  dplyr::mutate(withdrawals = rowSums(.[, c('WA Potomac River at Great Falls daily average withdrawals',
                                            'WA Potomac River at Little Falls daily average withdrawals',
                                            'FW Potomac River daily average withdrawals',
                                            'WSSC Potomac River daily average withdrawals')],
                                      na.rm = TRUE)) %>% 
  dplyr::select(date_time, withdrawals) %>% 
  dplyr::rename(date = date_time)

## ------------------------------------------------------------------------
lagk.df <- lagk.df %>% 
  tidyr::separate(date_time, into = c("date", "time"), convert = TRUE,
                  sep = " ", remove = FALSE) %>% 
  dplyr::mutate(date = as.Date(date)) %>% 
  dplyr::left_join(withdrawals.sub, by = "date") %>% 
  dplyr::select(-date, -time) %>% 
  dplyr::mutate(flow = if_else(site == "predicted" & !is.na(withdrawals),
                                flow - withdrawals,
                                flow)) %>% 
  dplyr::select(-withdrawals)

## ------------------------------------------------------------------------
rm(hourly.df, klag.df, pot.total, pred.df, with.df, withdrawals.df, withdrawals.sub, date_frame, variable_confluence, variable_lagk)

