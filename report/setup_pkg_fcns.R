library(ghg.sp)
library(tidyverse)
library(knitr)
library(kableExtra)
library(shiny)
library(patchwork)
library(tibble)
library(magrittr)
library(dplyr)
library(DT)
library(ghg.sp)
library(rsvg)
library(scales)

devtools::load_all(".") # remove once ghg.sp package is up to date with changes

options(scipen = 999, digits = 2)
load(file = file.path(here::here(), "../data-raw/transportation_report_data.rda"))

if (params$desired_format == "html") {
  tableformat <- function(data, .caption) {
    data %>%
      kableExtra::kable(
        caption = .caption,
        format = "html",
        escape = FALSE,
        format.args = list(big.mark = ",", digits = 6)
      ) %>%
      kableExtra::kable_classic(
        html_font = "Arial",
        full_width = F,
        font_size = 12
      )
  }
} else {
  tableformat <- function(data, .caption) {
    data %>%
      mutate(Variables = str_replace_all(Variables, fixed("<sub>2</sub>"), "2"),
      Variables = str_replace_all(Variables, fixed("<sup>2</sup>"), "2")) %>%
      flextable::flextable() %>%
      flextable::autofit(add_w = 0, add_h = 0) %>%
      flextable::set_table_properties(layout = "autofit") %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::align(align = "center", part = "header", i = 1) %>%
      flextable::colformat_double(digits = 0) %>%
      flextable::set_caption(.caption)
  }
}

add_variable_names <- function(data, .year) {
  data %>%
    left_join(.,
      variables,
      by = "var"
    ) %>%
    mutate(desc = str_remove_all(desc, " - Forecast")) %>%
    select(desc, value) %>%
    rename(!!.year := value, Variables = desc)
}

# chartformat function creates chart for scenario outputs with table structure expected as in report
chartformat <- function(tb) {
  chart_colors <- RColorBrewer::brewer.pal(5, "Blues")[c(2, 4:5)]
  tb %>%
    mutate(scinotation = case_when(
      `2018 Baseline` < 1e3 ~ ")",
      `2018 Baseline` < 1e6 ~ ", thousands)",
      `2018 Baseline` < 1e9 ~ ", millions)",
      `2018 Baseline` < 1e13 ~ ", billions)",
      TRUE ~ "",
    )) %>%
    pivot_longer(`2018 Baseline`:`2040 Scenario`, names_to = c("Year", "Future"), names_sep = " ", values_to = "value") %>%
    mutate(
      Variables = str_replace_all(Variables, fixed("<sub>2</sub>"), "2"),
      Variables = str_replace_all(Variables, fixed("<sup>2</sup>"), "2"),
      Variables = str_remove_all(Variables, "\\)"),
      Variables = paste0(Variables, scinotation),
      value = case_when(
        scinotation == ", thousands)" ~ value / 1e3,
        scinotation == ", millions)" ~ value / 1e6,
        scinotation == ", billions)" ~ value / 1e9,
        TRUE ~ value
      ),
      Year = as.numeric(Year)
    ) %>%
    ggplot(., aes(x = Year, y = value, fill = Future)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
    facet_wrap(~Variables, scales = "free", labeller = label_wrap_gen(width = 25)) +
    scale_x_continuous(breaks = c(2018, 2040)) +
    scale_fill_manual(values = chart_colors) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "", y = "", fill = "") +
    theme(legend.position = "bottom") +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 14),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(size = 10)
    )
}
