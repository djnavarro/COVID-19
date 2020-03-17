library(tidyverse)

path <- here::here(
  "csse_covid_19_data", 
  "csse_covid_19_time_series", 
  "time_series_19-covid-Confirmed.csv"
)

confirmed <- path %>%
  read_csv() %>% 
  select(-`Province/State`,-Lat,-Long) %>%
  rename(location = `Country/Region`) %>%
  group_by(location) %>%
  summarise_all(sum) %>%
  pivot_longer(cols = -location, names_to = "date", values_to = "cases") %>%
  mutate(date = lubridate::mdy(date))

threshold <- 1

nation_list <- confirmed %>% 
  group_by(location) %>%
  summarise(total_cases = max(cases)) %>%
  filter(total_cases >= threshold) %>%
  filter(location != "Cruise Ship") %>%
  pull(location)

pic <- ggplot(
  data = confirmed %>% 
    filter(
      location %in% nation_list,
      location != "China",
      cases >= threshold
    ),
  mapping = aes(x = date, y = cases, colour = location)
) + 
  geom_line(size = 1.25, show.legend = FALSE) + 
  gghighlight::gghighlight(
    location %in% c("Italy", "US", "United Kingdom", "Germany", "Singapore", "Australia"),
    label_params = list(
      colour = "black", 
      nudge_x = .5, 
      nudge_y = -.15, 
      segment.size = 0
    ), 
    use_direct_label = FALSE,
    unhighlighted_params = list(colour = "grey60")
  ) +
  theme_dark() + 
  xlab("Date") + 
  ylab("Confirmed Cases") + 
  ggtitle(
    label = "Confirmed COVID-19 cases by date", 
    subtitle = "source: https://github.com/CSSEGISandData/COVID-19"
  ) + 
  scico::scale_color_scico_d(name = NULL, palette = "lajolla") + 
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~location) 

pic1 <- pic + scale_y_log10() 
pic2 <- pic + coord_cartesian(ylim = c(0,1000)) 

ggsave("~/Desktop/covid1.png", pic1, width = 8, height = 6)
ggsave("~/Desktop/covid2.png", pic2, width = 8, height = 6)



australia <- path %>%
  read_csv() %>%
  rename(location = `Country/Region`, state = `Province/State`) %>%
  filter(location == "Australia" & state != "From Diamond Princess") %>%
  select(-location,-Lat,-Long) %>%
  pivot_longer(cols = -state, names_to = "date", values_to = "cases") %>%
  mutate(date = lubridate::mdy(date))


pic3 <- ggplot(
  data = australia,
  mapping = aes(x = date, y = cases, colour = state)
) +
  geom_line(size = 1.25) +
  theme_dark() +
  xlab("Date") +
  ylab("Confirmed Cases") +
  ggtitle(
    label = "Confirmed COVID-19 cases by Australian state",
    subtitle = "source: https://github.com/CSSEGISandData/COVID-19"
  ) +
  scico::scale_color_scico_d(name = NULL, palette = "lajolla") +
  theme(panel.grid.minor = element_blank())

pic4 <- pic3 + 
  scale_y_log10() + 
  gghighlight::gghighlight(
    state %in% c("New South Wales", "Victoria", "Queensland", "South Australia"),
    label_params = list(
      colour = "black", 
      nudge_x = .5, 
      nudge_y = -.15, 
      segment.size = 0
    ), 
    use_direct_label = FALSE,
    unhighlighted_params = list(colour = "grey60")
  ) + 
  facet_wrap(~state)
  

ggsave("~/Desktop/covid3.png", pic3, width = 8, height = 6)
ggsave("~/Desktop/covid4.png", pic4, width = 8, height = 6)


ss <- bind_rows(
  australia %>% 
    filter(state == "New South Wales") %>%
    rename(location = state),
  confirmed %>% 
    filter(location == "Singapore")
)


thresh <- 25

ss0 <- ss %>%
  mutate(over = cases > thresh) %>% 
  group_by(location) %>% 
  filter(over == TRUE) %>%
  slice(1) %>%
  select(location, date) %>%
  rename(day0 = date)

ss <- full_join(ss, ss0) %>%
  mutate(days = date - day0)

pic5 <-  ggplot(
  data = ss %>% filter(days >= 0),
  mapping = aes(x = days, y = cases, colour = location)
) + 
  geom_line(show.legend = FALSE, size = 2) + 
  theme_dark() + 
  ylab("Confirmed cases") + 
  xlab(paste0("Days since ", thresh, " confirmed cases")) + 
  geom_label(
    data = ss %>% group_by(location) %>% slice(n()),
    mapping = aes(label = location, group = location),
    hjust = "inward",
    show.legend = FALSE
  ) + 
  ggtitle(
    label = "Confirmed COVID-19 cases, Singapore vs New South Wales",
    subtitle = "source: https://github.com/CSSEGISandData/COVID-19"
  )
  

ggsave("~/Desktop/covid5.png", pic5, width = 8, height = 6)

