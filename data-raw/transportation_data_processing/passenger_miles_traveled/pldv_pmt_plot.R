library(ghg.sp)
library(ggplot2)
library(plotly)
library(dplyr)
source("data-raw/transportation_data_processing/passenger_miles_traveled/_plotly_layout.R")
source("data-raw/demographic/thrive_designation.R")

# regional vehicle miles traveled up to 2050
# some behavioral changes, its not enough to counterbalance the growth
# Minneapolis,
# 10,026,040.35 in 2018
#
# school bus increases?
# vintage of TAZ data used for modeling that included lower VMT,
# there was one period of time
# because, demographic details was that school-aged children jumped up,
# lot more school bus rides
# one vintage of socioeconomic forecasts, with lots of schoolchildren
#
# some buggy taz data that got used in 2018.
# they used some sort of OD data
# scenario VMT
#

10026040.35 * 340
pmt_dat <- transportation_data$passenger %>%
  filter(
    mode == "PLDV",
    var == "PMT"
  ) %>%
  left_join(thrive) %>%
  mutate(
    com_des_urban = case_when(
      com_des %in% c("Urban Center", "Urban") ~ "Urban, Urb. Core",
      TRUE ~ "Other"
    ) %>%
      factor(
        levels = c(
          "Urban, Urb. Core",
          "Other"
        ),
        ordered = TRUE
      ),
    is_msp = ifelse(ctu %in% c("Minneapolis", "St. Paul"), TRUE, FALSE)
  )

pmt_dat %>%
  filter(
    ctu == "Minneapolis",
    year %in% c(
      "2018",
      "2040"
    )
  )

# PLDV PMT -----


pmt_pct_change <- pmt_dat %>%
  group_by(ctu, var, mode) %>%
  arrange(year) %>%
  mutate(pct_change = (value - lag(value)) / lag(value))



pmt_dat %>%
  filter(ctu == "Minneapolis") %>%
  View()
pmt_pct_change %>%
  filter(
    ctu %in% c(
      "Minneapolis",
      "St. Paul",
      "Bloomington",
      "Lake Elmo"
    )
  ) %>%
  ggplot(aes(
    x = year,
    y = value,
    color = ctu,
    group = ctu,
    label = scales::percent(pct_change, accuracy = 0.1)
  )) +
  geom_point() +
  geom_line() +
  geom_text(
    vjust = -1,
    hjust = 0
  ) +
  theme_council(
    use_showtext = TRUE,
    use_manual_font_sizes = TRUE
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Passenger vehicle PMT",
    subtitle = "Minneapolis decreases over time, St. Paul slight increase",
    caption = Sys.Date(),
    x = "Year",
    y = "Miles",
    color = "City"
  )

pmt_pct_change %>%
  filter(
    ctu %in% c(
      "Minneapolis",
      "St. Paul",
      "Bloomington",
      "Lake Elmo"
    )
  ) %>%
  select(mode, value, ctu, year) %>%
  pivot_wider(
    names_from = year,
    values_from = value
  ) %>%
  knitr::kable(output = "markdown")



com_des_pmt <- pmt_pct_change %>%
  mutate(
    net_increase = ifelse(pct_change > 0, TRUE, FALSE),
    avg_pct_change = mean(pct_change, na.rm = T)
  ) %>%
  # select(ctu, var, mode, net_increase, avg_pct_change) %>%
  filter(!is.na(net_increase)) %>%
  arrange(desc(is_msp), desc(com_des_urban)) %>%
  unique()

## plot avg pct change by ctu -----
plot_ly(
  data = com_des_pmt,
  type = "scatter",
  mode = "markers",
  y = ~ reorder(ctu, avg_pct_change),
  x = ~avg_pct_change,
  fillcolor = ~com_des_urban,
  fill = "Dark2",
  opacity = 0.7,
  size = 5,
  hovertemplate =
    ~ paste0(
      "<b>", ctu, "</b><br>",
      scales::percent(avg_pct_change, accuracy = 0.1),
      " <extra></extra>"
    ),
  marker = list(
    line = list(
      color = "lightgray",
      width = 0.1
    )
  )
) %>%
  add_trace(
    showlegend = FALSE,
    inherit = FALSE,
    data = com_des_pmt %>%
      filter(is_msp == TRUE),
    type = "scatter",
    mode = "markers",
    y = ~ reorder(ctu, avg_pct_change),
    x = ~avg_pct_change,
    fillcolor = ~com_des_urban,
    fill = "Dark2",
    # opacity = 0.7,
    # size = 5,
    marker = list(
      symbol = "star",
      size = 17,
      color = "#7570b3",
      line = list(
        color = "white",
        width = 0.1
      )
    ),
    hovertemplate =
      ~ paste0(
        "<b>", ctu, "</b><br>",
        scales::percent(avg_pct_change, accuracy = 0.1),
        " <extra></extra>"
      )
  ) %>%
  plotly_layout(
    main_title = "Change in Passenger Vehicle PMT",
    subtitle = "Minneapolis and St. Paul shown as stars",
    x_title = "Avg. % change, 2015-2040"
  ) %>%
  layout(
    legend = list(
      orientation = "h",
      traceorder = "reversed"
    ),
    # hovermode = "x unified",
    xaxis = list(
      tickformat = "1%"
    )
  )

# all modes pmt -----

total_pmt_modes <- transportation_data$passenger %>%
  filter(var == "PMT") %>%
  left_join(transportation_index$modes, by = c("mode" = "mode_abbrev")) %>%
  select(-aeo_mode, -type, -mode_id, -mode_description_2, -var, -mode) %>%
  group_by(ctu, year) %>%
  pivot_wider(
    names_from = mode_description_1,
    values_from = value
  ) %>%
  group_by(ctu, year) %>%
  mutate(total_pmt = sum(across(1:9))) %>%
  left_join(thrive) %>%
  mutate(
    com_des_urban = case_when(
      com_des %in% c("Urban Center", "Urban") ~ "Urban, Urb. Core",
      TRUE ~ "Other"
    ) %>%
      factor(
        levels = c(
          "Urban, Urb. Core",
          "Other"
        ),
        ordered = TRUE
      ),
    is_msp = ifelse(ctu %in% c("Minneapolis", "St. Paul"), TRUE, FALSE)
  )

## plot avg pct change by ctu -----
plot_ly(
  data = total_pmt_modes %>%
    filter(is_msp == TRUE),
  type = "scatter",
  mode = "markers",
  y = ~total_pmt,
  x = ~year,
  fillcolor = ~ctu,
  fill = "Dark2",
  opacity = 0.7,
  size = 5,
  hovertemplate =
    ~ paste0(
      "<b>", ctu, "</b><br>",
      scales::comma(total_pmt, accuracy = 10),
      " <extra></extra>"
    ),
  marker = list(
    line = list(
      color = "lightgray",
      width = 0.1
    )
  )
) %>%
  plotly_layout(
    main_title = "Change in Passenger PMT",
    subtitle = "Minneapolis and St. Paul shown as stars",
    # x_title = "Avg. % change, 2015-2040"
  ) %>%
  layout(
    legend = list(
      orientation = "h",
      traceorder = "reversed"
    )
  )
# hovermode = "x unified",




## plot pmt by mode -----
transportation_data$passenger %>%
  filter(var == "PMT") %>%
  left_join(transportation_index$modes, by = c("mode" = "mode_abbrev")) %>%
  left_join(thrive) %>%
  mutate(
    com_des_urban = case_when(
      com_des %in% c("Urban Center", "Urban") ~ "Urban, Urb. Core",
      TRUE ~ "Other"
    ) %>%
      factor(
        levels = c(
          "Urban, Urb. Core",
          "Other"
        ),
        ordered = TRUE
      ),
    is_msp = ifelse(ctu %in% c("Minneapolis", "St. Paul"), TRUE, FALSE)
  ) %>%
  group_by(ctu, var, mode) %>%
  mutate(pct_change = (value - lag(value)) / lag(value)) %>%
  filter(is_msp == TRUE) %>%
  filter(ctu == "Minneapolis") %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    y = ~value,
    x = ~year,
    fillcolor = ~mode_description_1,
    fill = "Dark2",
    opacity = 0.7,
    size = 5,
    hovertemplate =
      ~ paste0(
        "<b>", ctu, "</b><br>",
        mode_description_1, " : ", scales::comma(value, accuracy = 10), " miles<br>", scales::percent(pct_change, accuracy = 0.1), " change",
        " <extra></extra>"
      ),
    marker = list(
      line = list(
        color = "lightgray",
        width = 0.2
      )
    )
  ) %>%
  plotly_layout(
    main_title = "Passenger Miles Traveled by Mode",
    subtitle = "Minneapolis",
    x_title = "Year"
  ) %>%
  layout()



transportation_data$passenger %>%
  filter(var == "PMT") %>%
  left_join(transportation_index$modes, by = c("mode" = "mode_abbrev")) %>%
  left_join(thrive) %>%
  mutate(
    com_des_urban = case_when(
      com_des %in% c("Urban Center", "Urban") ~ "Urban, Urb. Core",
      TRUE ~ "Other"
    ) %>%
      factor(
        levels = c(
          "Urban, Urb. Core",
          "Other"
        ),
        ordered = TRUE
      ),
    is_msp = ifelse(ctu %in% c("Minneapolis", "St. Paul"), TRUE, FALSE)
  ) %>%
  group_by(ctu, var, mode) %>%
  mutate(pct_change = (value - lag(value)) / lag(value)) %>%
  filter(ctu == "Minneapolis") %>%
  ggplot(
    aes(
      x = year,
      y = value,
      fill = mode_description_1,
      color = mode_description_1,
      group = mode_description_1,
      label = value
    )
  ) +
  geom_col() +
  # geom_point() +
  # geom_line() +
  # geom_text(vjust = -1,
  #           hjust = 0 ) +
  theme_council(
    use_showtext = TRUE,
    use_manual_font_sizes = TRUE
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_discrete(
    labels = scales::label_wrap(width = 20),
    aesthetics = c("fill", "color")
  ) +
  labs(
    title = "Passenger PMT",
    subtitle = "Minneapolis decreases total PMT over time, with increasing portions of other modes",
    caption = Sys.Date(),
    x = "Year",
    y = "Miles",
    color = "Mode",
    fill = "Mode"
  )
