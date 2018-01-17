## ------------------------------------------------------------------------
hourly.df <- file.path(rprojroot::find_rstudio_root_file(), "data_ts/historical/flows_obs/hourly_flows.csv") %>%
  data.table::fread(
    data.table = FALSE,
    na.strings = c("", " ", "Eqp", "#N/A", "-999999"),
    showProgress = FALSE) %>% 
  dplyr::filter(site %in% c("goose", "lfalls", "mon_jug", "por", "seneca")) %>% 
  mutate(date_time = ymd_hms(date_time)) %>% 
  dplyr::filter(!is.na(flow))

## ------------------------------------------------------------------------
hourly.df <- hourly.df %>% 
  select(date_time, site, flow) %>% 
  group_by(date_time, site) %>% 
  summarize(flow = mean(flow, na.rm = TRUE)) %>% 
  ungroup() %>%
  spread(site, flow) %>% 
  dplyr::filter(!is.na(goose) & !is.na(lfalls) & 
                  !is.na(mon_jug) & !is.na(por) & !is.na(seneca)) %>% 
  gather(site, flow, goose:seneca)

## ------------------------------------------------------------------------
hourly_adjusted.df <- hourly.df %>% 
  dplyr::mutate(flow = case_when(
    site == "por" ~ flow * 1.046,
    site == "mon_jug" ~ flow * 1.189,
    site == "goose" ~ flow * 1.164,
    site == "seneca" ~ flow * 1.270,
    TRUE ~ flow * 1
  ))

## ------------------------------------------------------------------------
with.df <- file.path(rprojroot::find_rstudio_root_file(), "data_ts/historical/withdrawals/withdrawals_wma_daily_mgd_1990_2017.csv") %>%
  data.table::fread(data.table = FALSE,
                    na.strings = c("", " ", "Eqp", "#N/A", "-999999")) %>% 
  dplyr::filter(!rowSums(is.na(.)) == ncol(.))

## ------------------------------------------------------------------------
pot.wma.total.daily <- with.df %>% 
  dplyr::filter(location %in% c("Potomac River", "Potomac River at Little Falls",
                                "Potomac River at Great Falls") ) %>% 
  dplyr::group_by(measurement, date_time, units) %>% 
  dplyr::summarize(value = sum(value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(unique_id = "potomac_wma_total", date = as.Date(date_time),
                withdr_pot_tot_mgd = value) %>% 
  dplyr::filter(!rowSums(is.na(.)) == ncol(.)) %>%
  dplyr:: select(date, withdr_pot_tot_mgd)

## ------------------------------------------------------------------------
#rm(with.df, withdrawals.df, pot.total)

