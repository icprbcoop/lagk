
variable_lagk <- function(long.df, site.name, por.lag, klag.df) {
  #------------------------------------------------------------------------------
  # To reduce number of parameters to be adjusted, focus on lag_to_lfalls
  #    and loss_to_lfalls
  klag.sub <- klag.df %>% 
    dplyr::filter(site == por.lag) %>% 
#    dplyr::select(flow, lag)
    dplyr::mutate(lag = lag_to_lfalls*pct_of_stream_length,
                  loss = loss_to_lfalls*pct_of_stream_length) %>%
    dplyr::select(flow, lag, loss)

  #------------------------------------------------------------------------------
  predicted.df <- long.df %>% 
    dplyr::filter(site == site.name) %>% 
    dplyr::mutate(range = as.character(cut(flow, breaks = klag.sub$flow)),
                  lower_flow = as.numeric(gsub("\\(|,.*", "", range)),
                  upper_flow = as.numeric(gsub(".*,|\\]", "", range))) %>% 
    dplyr::left_join(klag.sub, by = c("lower_flow" = "flow")) %>% 
    dplyr::left_join(klag.sub, by = c("upper_flow" = "flow")) %>% 
    dplyr::rename(lower_lag = lag.x,
                  lower_loss = loss.x,
                  upper_lag = lag.y,
                  upper_loss = loss.y) %>% 
    dplyr::mutate(lag = lower_lag + ((upper_lag - lower_lag) * (flow - lower_flow) / (upper_flow - lower_flow)),
                  arrival_time = date_time + lag * 3600) %>% 
    dplyr::mutate(loss = lower_loss + ((upper_loss - lower_loss) * (flow - lower_flow) / (upper_flow - lower_flow)),
                  flow2 = flow*(1 - loss)) %>% 
  # Grabbing a lot right now for QA'ing purposes:
    dplyr::select(date_time, arrival_time, site, lower_flow, upper_flow, flow, lag, loss, flow2) %>% 
    dplyr::rename(date_time = lag) %>% 
    dplyr::mutate(site = "predicted") %>% 
    dplyr::mutate(date_time = as.POSIXct(round(date_time, units = "hours"))) %>% 
    dplyr::group_by(date_time, site) %>% 
    dplyr::summarise(flow = mean(flow)) %>%
  # CS: apply loss:
  #  dplyr::mutate(flow = flow*)
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
