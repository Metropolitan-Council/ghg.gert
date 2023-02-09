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

devtools::load_all('..') # remove once ghg.sp package is up to date with changes

options(scipen = 1, digits = 2)
load(file = file.path(here::here(),"../data-raw/transportation_report_data.rda"))

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
  tableformat <- function(data, .caption){
    data %>%
      flextable::flextable() %>%
      flextable::autofit(add_w = 0, add_h = 0) %>%
      flextable::set_table_properties(layout = "autofit") %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::align(align = "center", part = "header", i = 1) %>%
      flextable::set_caption(.caption)
  }
}

add_variable_names <- function(data, .year) {
  data %>%
    left_join(.,
              variables,
              by = "var") %>%
    mutate(desc = str_remove_all(desc, " - Forecast")) %>%
    select(desc, value) %>%
    rename(!!.year := value, Variables = desc)
}

# chartformat function creates chart for scenario outputs with table structure expected as in report
chartformat <- function(tb) {
  chart_colors <-  RColorBrewer::brewer.pal(5, "Blues")[c(2, 4:5)]
  tb %>% pivot_longer(`2018 Baseline`:`2040 Scenario`, names_to = c('Year', 'Future'), names_sep = ' ', values_to = 'value') %>%
    #tidyr::separate_wider_delim(Variables, delim = ("("), names = c('Variables','Units')) %>%
    mutate(Variables = str_replace_all(Variables, fixed("<sub>2</sub>"), "2"),
         #  Units = str_remove_all(Units, fixed(")")),
           Year = as.numeric(Year)) %>%
    # mutate(level = paste0(as.numeric(level) * 100, "%")) %>%
    ggplot(., aes(x = Year, y = value, fill = Future)) + #fill = level, 
    geom_bar(stat = 'identity', position = position_dodge2(preserve = 'single')) +
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
