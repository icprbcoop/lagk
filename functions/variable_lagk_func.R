
variable_lagk <- function(long.df, site.name, subreach.name, klag.df) {
  #------------------------------------------------------------------------------
  klag.sub <- klag.df %>% 
    dplyr::filter(subreach == subreach.name) %>% 
    dplyr::select(flow, lag)
  #------------------------------------------------------------------------------
  predicted.df <- long.df %>% 
    dplyr::filter(site == site.name) %>% 
    dplyr::mutate(range = as.character(cut(flow, breaks = klag.sub$flow)),
                  lower_flow = as.numeric(gsub("\\(|,.*", "", range)),
                  upper_flow = as.numeric(gsub(".*,|\\]", "", range))) %>% 
    dplyr::left_join(klag.sub, by = c("lower_flow" = "flow")) %>% 
    dplyr::left_join(klag.sub, by = c("upper_flow" = "flow")) %>% 
    dplyr::rename(lower_lag = lag.x,
                  upper_lag = lag.y) %>% 
    dplyr::mutate(lag = lower_lag + ((upper_lag - lower_lag) * (flow - lower_flow) / (upper_flow - lower_flow)),
                  lag = date_time + lag * 3600) %>% 
    dplyr::select(lag, site, flow) %>% 
    dplyr::rename(date_time = lag) %>% 
    dplyr::mutate(site = "predicted") %>% 
    dplyr::mutate(date_time = as.POSIXct(round(date_time, units = "hours"))) %>% 
    dplyr::group_by(date_time, site) %>% 
    dplyr::summarise(flow = mean(flow))
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
#==============================================================================
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
#==============================================================================
fast_variable_lagk <- function(long.df, site.name, subreach.name, klag.df) {
  #------------------------------------------------------------------------------
  klag.sub <- data.table::data.table(klag.df)
  klag.sub <- klag.sub[subreach == subreach.name]
  klag.sub <- klag.sub[, c('flow', 'lag', 'loss'), with = FALSE]
  #------------------------------------------------------------------------------
  long.df <- data.table::data.table(long.df)
  predicted.dt <- long.df
  predicted.dt <- predicted.dt[site == site.name]
  predicted.dt[, site := "predicted"]
  
  range.vec <- as.character(cut(predicted.dt$flow, breaks = klag.sub$flow))
  
  predicted.dt[, lower_flow := as.numeric(gsub("\\(|,.*", "", range.vec))]
  predicted.dt[, upper_flow := as.numeric(gsub(".*,|\\]", "", range.vec))]
  predicted.dt[, lower_lag := klag.sub$lag[match(predicted.dt$lower_flow, klag.sub$flow)]]
  predicted.dt[, upper_lag := klag.sub$lag[match(predicted.dt$upper_flow, klag.sub$flow)]]
  predicted.dt[, lower_loss := klag.sub$loss[match(predicted.dt$lower_flow, klag.sub$flow)]]
  predicted.dt[, upper_loss := klag.sub$loss[match(predicted.dt$upper_flow, klag.sub$flow)]]
  predicted.dt[, lag := lower_lag + ((upper_lag - lower_lag) * (flow - lower_flow) / (upper_flow - lower_flow))]
  predicted.dt[, date_time := date_time + lag * 3600]
  predicted.dt[, date_time := as.POSIXct(round(date_time, units = "hours"))]
  predicted.dt[, loss := lower_loss + ((upper_loss - lower_loss) * (flow - lower_flow) / (upper_flow - lower_flow))]
  predicted.dt[, flow := flow * (1 - loss)]
  predicted.dt <- predicted.dt[, c('date_time', 'site', 'flow'), with = FALSE]
  predicted.dt[, flow := mean(flow), by = .(date_time, site)]
  #------------------------------------------------------------------------------
  start.date <- min(predicted.dt$date_time, na.rm = TRUE)
  end.date <- max(predicted.dt$date_time, na.rm = TRUE)
  date.frame <- data.table::data.table(date_time = seq.POSIXt(start.date, end.date, by = "hour"))
  #------------------------------------------------------------------------------
  setkey(date.frame, date_time)
  setkey(predicted.dt, date_time)
  final.dt <- merge(date.frame, predicted.dt, all.x = TRUE)
  #------------------------------------------------------------------------------
  final.dt[, flow := c(rep(NA, which.min(is.na(flow)) - 1), zoo::na.approx(flow))]
  setkey(final.dt)
  final.dt <- unique(final.dt)
  final.dt[, site := "predicted"]
  setcolorder(final.dt, c("site", "date_time", "flow"))
  #------------------------------------------------------------------------------
  final.df <- as.data.frame(final.dt, stringsAsfactors = FALSE)
  return(final.df)
}
#==============================================================================
fast_variable_confluence <- function(long1.df, gage1, lag1,
                                     long2.df, gage2, lag2, klag.df) {
  g1.dt <- data.table::data.table(fast_variable_lagk(long1.df, gage1, lag1, klag.df))
  g2.dt <- data.table::data.table(fast_variable_lagk(long2.df, gage2, lag2, klag.df))
  final.dt <- rbindlist(list(g1.dt, g2.dt))
  final.dt[, flow := sum(flow), , by = .(date_time, site)]
  final.dt <- unique(final.dt)
  final.df <- as.data.frame(final.dt, stringsAsfactors = FALSE)
  return(final.df)
}
#==============================================================================
pred_lfalls <- function(long.df, klag.df) {
  conf.1 <- fast_variable_confluence(long.df, "por", "por_1", 
                                 long.df, "mon_jug", "monoc_1", klag.df)
  
  conf.2 <- fast_variable_confluence(conf.1,"predicted", "por_2",
                                 long.df, "goose", "goose_1", klag.df)
  conf.3 <- fast_variable_confluence(conf.2, "predicted", "por_3",
                                 long.df, "seneca", "seneca_1", klag.df)
  
  pred.df <- fast_variable_lagk(conf.3, "predicted", "por_4", klag.df)
  final.dt <- data.table::rbindlist(list(long.df, pred.df))
  final.df <- as.data.frame(final.dt)
  return(final.df)
}
#==============================================================================
calc_rmse <- function(pred.df, remove.days = 3) {
  lagk.results.dt <- data.table::data.table(pred.df)
  lagk.results.dt <- lagk.results.dt[site %in% c("lfalls", "predicted")]
  #----------------------------------------------------------------------------
  # Remove the first day and any trailing predictions. 
  # The first 3 days is generally poorly predicted because there are no upstream data.
  # Also, the prediction should go beyond the final observed lfalls value by at least an hour.
  # Therefore, these leading and trailing values are not appropriate for the RMSE measure
  # and could skew results.
  lfalls.dt <- lagk.results.dt[site == "lfalls"]
  min.date <- min(lfalls.dt$date_time) + lubridate::days(remove.days)
  max.date <- max(lfalls.dt$date_time)
  lagk.results.dt <- lagk.results.dt[date_time >= min.date & date_time <= max.date]
  #----------------------------------------------------------------------------
  lagk.results.wide <- data.table::dcast(lagk.results.dt,
                                         date_time ~ site, value.var = "flow")
  lagk.results.wide <- lagk.results.wide[!is.na(lfalls) & !is.na(predicted)]
  #----------------------------------------------------------------------------
  se <- (lagk.results.wide$lfalls - lagk.results.wide$predicted)^2
  rmse <- -sqrt(mean(se))
  return(rmse)
}
#==============================================================================

