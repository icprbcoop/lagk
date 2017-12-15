
variable_lagk <- function(long.df, site.name, por.lag, klag.df) {
  # long.df is name of time series hourly flow data file
  # site.name is name of node (e.g. gage) of hydrograph to be lagged (from time series hourly flow data file)
  # por.lag is name of the subreach for which the lag/loss applies
  # klag.df is the table giving subreach, subreach_lag, subreach_loss
  #
  klag.sub <- klag.df %>% 
    dplyr::filter(subreach == por.lag) %>% 
#    dplyr::select(flow, lag)
    dplyr::mutate(lag = subreach_lag, loss = subreach_loss, flow_bin = flow_cfs) %>% 
    dplyr::select(flow_bin, lag, loss)

  #------------------------------------------------------------------------------
  predicted.df <- long.df %>% 
    dplyr::filter(site == site.name) %>% 
    dplyr::mutate(range = as.character(cut(flow, breaks = klag.sub$flow_bin)),
                  lower_flow = as.numeric(gsub("\\(|,.*", "", range)),
                  upper_flow = as.numeric(gsub(".*,|\\]", "", range))) %>% 
    dplyr::left_join(klag.sub, by = c("lower_flow" = "flow_bin")) %>% # changed "flow" to "flow_bin" here?
    dplyr::left_join(klag.sub, by = c("upper_flow" = "flow_bin")) %>% 
    dplyr::rename(lower_lag = lag.x,
                  lower_loss = loss.x,
                  upper_lag = lag.y,
                  upper_loss = loss.y) %>% 
    dplyr::mutate(lag = lower_lag + ((upper_lag - lower_lag) * (flow - lower_flow) / (upper_flow - lower_flow)),
                  date_time_lagged = date_time + lag * 3600) %>% 
    dplyr::mutate(loss = lower_loss + ((upper_loss - lower_loss) * (flow - lower_flow) / (upper_flow - lower_flow)),
                  flow2 = flow*(1 - loss)) %>% 
  # Grabbing a lot right now for QA'ing purposes:
  #  dplyr::select(date_time, date_time_lagged, site, lower_flow, upper_flow, flow, lag, loss, flow2) %>% 
    dplyr::select(date_time_lagged, site, flow2) %>%
    dplyr::rename(date_time = date_time_lagged) %>% 
    dplyr::mutate(site = "predicted") %>% 
    dplyr::mutate(date_time = as.POSIXct(round(date_time, units = "hours"))) %>% 
    dplyr::group_by(date_time, site) %>% 
    dplyr::summarise(flow = mean(flow2))
  # 
  #------------------------------------------------------------------------------
  start.date <- min(predicted.df$date_time, na.rm = TRUE)
  end.date <- max(predicted.df$date_time, na.rm = TRUE)
  date.frame <- date_frame(start.date, end.date, "hour")
  final.df <- left_join(date.frame, predicted.df, by = "date_time") %>% 
    mutate(flow = c(rep(NA, which.min(is.na(flow)) - 1),
                         zoo::na.approx(flow)),
           site = "predicted") %>% 
    dplyr::filter(!is.na(flow))
  #------------------------------------------------------------------------------
  return(final.df)
}
