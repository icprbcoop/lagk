library(tidyverse)

flow.df <- data.table::fread("data_ts/current/flows_obs/flow_hourly_cfs.csv") %>% 
  filter(site %in% c("por", "lfalls")) %>% 
  mutate(date_time = str_replace_all(date_time, "T|Z", ""),
    date = as.POSIXct(date_time, "%Y-%m-%d%H:%M:%S", tz = "EST"),
         month = as.character(lubridate::month(date, label = TRUE))) %>% 
  select(-date_time) %>% 
  rename(date_time = date) %>% 
  arrange(site, date_time)
  filter(date < )
  filter(month == "May")


ggplot(flow.df, aes(date, flow, group = site)) +
  geom_line()


por <- flow.df %>% 
  filter(site == "por") %>% 
  pull(flow)

lfalls <- flow.df %>% 
  filter(site == "lfalls") %>% 
  pull(flow)
## Find the best match with the canonical recursion formula
library(dtw)
alignment<-dtw(lfalls, por, step.pattern = asymmetric, keep = TRUE)

## Display the warping curve, i.e. the alignment curve
plot(alignment)

## Align and plot with the Rabiner-Juang type VI-c unsmoothed recursion
plot(
  dtw(lfalls, por, keep=TRUE,
      step=rabinerJuangStepPattern(6,"c")),
  type="twoway",offset=-2)

## See the recursion relation, as formula and diagram
rabinerJuangStepPattern(6,"c")
plot(rabinerJuangStepPattern(6,"c"))


cost.mat <- alignment$costMatrix
dir.mat <- alignment$directionMatrix
step.pat <- alignment$stepPattern
localcost.mat <- as.matrix(alignment$localCostMatrix)

alignment$index1
alignment$index2
dtw(por, lfalls, step.pattern = asymmetric, keep = TRUE)$distance
stats:::dist(rbind(por[1,alignment$index1], lfalls[2,alignment$index2]))
stats:::dist(rbind(data.frame(site = por, index = alignment$index1), data.frame(site = lfalls, index = alignment$index2)))


por.df <- flow.df %>% 
  filter(site == "por") %>% 
  mutate(num = 1:nrow(.))

lfalls.df <- flow.df %>% 
  filter(site == "lfalls")
index.df <- data.frame(num = alignment$index2,
                       date_time = as.POSIXct(lfalls.df$date_time),
                       org_date_time = as.POSIXct(head(por.df$date_time, 5246)),
                       site = "predicted")

join.df <- left_join(index.df, por.df[, c("num", "flow")], by = "num") %>% 
  bind_rows(flow.df) %>% 
  mutate(date_diff = date_time - org_date_time) %>% 
  mutate(month = as.character(lubridate::month(date_time, label = TRUE)))


ggplot(join.df, aes(date_time, flow, group = site, color = site)) +
  geom_line(size = 2, alpha = 0.5)

join.df2 <- join.df %>% 
  filter(month == "May")
ggplot(join.df2, aes(date_time, flow, group = site, color = site)) +
  geom_line(size = 2, alpha = 0.5)

test <- join.df %>% 
  filter(!is.na(org_date_time)) %>% 
  mutate(bins = cut(flow, breaks = seq(0, 100000, 500))) %>% 
  group_by(bins) %>% 
  summarize(mean = mean(date_diff),
            abs_mean = mean(abs(date_diff)))
ggplot(test, aes(bins, abs_mean)) +
  geom_point()
