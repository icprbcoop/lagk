## ------------------------------------------------------------------------
hourly.df <- file.path(rprojroot::find_rstudio_root_file(), "data_ts/historical/flows_obs/hourly_flows.zip") %>%
  unzip(exdir = file.path(rprojroot::find_rstudio_root_file(), "data_ts/historical/flows_obs/hourly_flows"),
        overwrite = TRUE) %>% 
  data.table::fread(
    data.table = FALSE,
    na.strings = c("", " ", "Eqp", "#N/A", "-999999"),
    showProgress = FALSE) %>% 
  dplyr::filter(site %in% c("goose", "lfalls", "mon_jug", "por", "seneca")) %>% 
  mutate(date_time = ymd_hms(date_time)) %>% 
  dplyr::filter(!is.na(flow))

#close.connection(file.path(rprojroot::find_rstudio_root_file(), "data_ts/historical/flows_obs/hourly_flows"))

## ------------------------------------------------------------------------
hourly.df <- hourly.df %>% 
  select(date_time, site, flow) %>% 
  group_by(date_time, site) %>% 
  summarize(flow = mean(flow, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(flow))

## ------------------------------------------------------------------------
hourly.adj.df <- hourly.df %>% 
  dplyr::mutate(
    flow = case_when(
      site == "por" ~ flow * 1.046,
      site == "mon_jug" ~ flow * 1.189,
      site == "goose" ~ flow * 1.164,
      site == "seneca" ~ flow * 1.270,
      TRUE ~ flow * 1
    )
  )

## ------------------------------------------------------------------------
with.df <- file.path(rprojroot::find_rstudio_root_file(), "data_ts/historical/withdrawals/withdrawals_wma_daily_mgd_1990_2017.csv") %>%
  data.table::fread(data.table = FALSE,
                    na.strings = c("", " ", "Eqp", "#N/A", "-999999"))

## ------------------------------------------------------------------------
pot.wma.total.daily <- with.df %>% 
  filter(grepl("Potomac River", location)) %>% 
#  dplyr::filter(location %in% c("Potomac River", "Potomac River at Little Falls","Potomac River at Great Falls")) %>% 
  dplyr::group_by(measurement, date_time, units) %>% 
  dplyr::summarize(value = sum(value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(unique_id = "potomac_wma_total", date = as.Date(date_time),
                withdr_pot_tot_mgd = value) %>% 
  dplyr::select(date, withdr_pot_tot_mgd)

## ------------------------------------------------------------------------
# klag.df columns are: site, flow, ..., lag, loss
klag.df <- file.path(rprojroot::find_rstudio_root_file(),
                     "data/parameters/k_lag.csv") %>% 
  data.table::fread(data.table = FALSE) %>%
  rename_all(tolower) %>%
  mutate(site = tolower(site))

## ------------------------------------------------------------------------
klag.reach <- file.path(rprojroot::find_rstudio_root_file(),
                     "data/parameters/k_lag_reaches.csv") %>% 
  data.table::fread(data.table = FALSE) %>%
  rename_all(tolower) %>%
  mutate_if(is.character, tolower) %>% 
  rename(lag = reach_lag_hours,
         loss = reach_loss_fraction,
         flow = flow_cfs)

klag.subreach <- file.path(rprojroot::find_rstudio_root_file(),
                     "data/parameters/k_lag_subreaches.csv") %>% 
  data.table::fread(data.table = FALSE) %>%
  rename_all(tolower) %>%
  mutate_if(is.character, tolower)

## ------------------------------------------------------------------------
#rm(with.df, withdrawals.df, pot.total)

