
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

chartformat <- function(startrange, endrange, moveby, tb, ...) {
 seq(startrange, endrange, moveby) %>%
    purrr::set_names() %>%
    purrr::map(~ghg.sp::run_scenario_building(res_tb = tb,
                                              .selected_ctu = params$ctu_selection, ...) 
    ) %>% bind_rows(.id = "level") %>%
    filter(
      var %in% c(
        "residential_mwh",
        "residential_therms",
        "residential_natural_gas_emissions_kg_co",
        "residential_electricity_emissions_kg_co"
      )) %>%
    left_join(., variables, by = "var") %>%
    mutate(desc = str_replace_all(desc, "ft<sup>2</sup>", "ft2"),
           year = as.numeric(year)) %>%
    rename("Variables" = desc) %>% 
    mutate(level = paste0(as.numeric(level) * 100, "%")) %>% 
   ggplot(., aes(x = year, y = value, fill = level, color = scen)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    facet_wrap(~Variables, scales = "free", labeller = label_wrap_gen(width = 25)) +
    scale_x_continuous(breaks = c(2018, 2040)) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(6, "Blues")[2:6]) +
    labs(x = "", y = "", color = "Percentage of\nsingle family\nfloor area\nreduced due to\nincreased energy\nprices", shape = 'Future') +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.title = element_text(size = 14),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(size = 10)
    )
}
