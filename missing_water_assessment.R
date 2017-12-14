## ------------------------------------------------------------------------
lagk.df %>% 
  filter(site %in% c("lfalls", "predicted")) %>% 
ggplot(aes(date_time, flow, group = site)) +
  geom_line(aes(color = site))

## ------------------------------------------------------------------------
lagk.wide <- lagk.df %>%
  select(date_time, site, flow) %>% 
  group_by(date_time, site) %>% 
  summarize(flow = mean(flow, na.rm = TRUE)) %>% 
  spread(site, flow) %>% 
  mutate(residuals = predicted - lfalls,
         year = year(date_time)) %>% 
  select(date_time, year, month, residuals)

## ------------------------------------------------------------------------
ggplot(lagk.wide, aes(date_time, residuals)) +
  geom_line()

## ----fig.width=10, fig.height=20-----------------------------------------
ggplot(lagk.wide, aes(date_time, residuals)) +
  geom_line() +
  facet_wrap(~year, ncol = 2, scales = "free")

## ------------------------------------------------------------------------
resid.pos <- lagk.wide %>% 
  mutate(month = month(date_time, label = TRUE)) %>% 
  #filter(residuals > 0) %>% 
  group_by(month) %>% 
  summarize(mean = mean(residuals, na.rm = TRUE),
            sd = sd(residuals, na.rm = TRUE),
            median = median(residuals, na.rm = TRUE))

## ------------------------------------------------------------------------
ggplot(resid.pos, aes(month, mean)) +
  geom_bar(stat = "identity") +
   geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                  width = 0.2,                    # Width of the error bars
                  position = position_dodge(0.9))

