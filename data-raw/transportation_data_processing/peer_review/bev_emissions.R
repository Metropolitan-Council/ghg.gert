pkgload::load_all()
library(ggplot2)


transportation_data$passenger %>%
  filter(
    ctu == "St. Paul",
    mode == "PLDV"
  ) %>%
  View()

grid_percent_seq <- seq(0, 1, 0.2)

library(furrr)
future::plan(future::multisession)

bev_grid_combo <- furrr::future_map(
  seq(0, 1, 0.2),
  function(bev_pct) {
    purrr::map(
      grid_percent_seq,
      function(grid_pct) {
        run_module_transportation(
          .selected_ctu = "St. Paul",
          .calc_transp_ghg_embodied = TRUE,
          .bev_pct_sales = bev_pct,
          .grid_decarbonization_pct = grid_pct,
          .scenario = paste0("bev", bev_pct, "_grid", grid_pct)
        )
      }
    )
  }
)

bev_grid <- purrr::map_dfr(
  bev_grid_combo,
  function(bev_list_item) {
    # browser()
    item_summary <- bind_rows(
      bev_list_item[[1]]$passenger_all,
      bev_list_item[[2]]$passenger_all,
      bev_list_item[[3]]$passenger_all,
      bev_list_item[[4]]$passenger_all,
      bev_list_item[[5]]$passenger_all,
      bev_list_item[[6]]$passenger_all
    ) %>%
      filter(
        year %in% c("2040"),
        type == "P",
        mode == "PLDV"
      ) %>%
      group_by(ctu, scenario, year, class) %>% # mode, sector
      summarise(
        emissions = sum(dir_ghg, na.rm = T),
        # ghg_embodied = sum(ghg_embodied, na.rm = T),
        .groups = "keep"
      )


    item_summary %>%
      mutate(
        bev_pct = stringr::str_split(scenario, pattern = "_", n = 2, simplify = TRUE)[, 1],
        grid_pct = stringr::str_split(scenario, pattern = "_", n = 2, simplify = TRUE)[, 2]
      ) %>%
      return()
  }
)

ggplot(
  data = bev_grid,
  aes(
    x = grid_pct,
    y = emissions,
    color = class,
    shape = bev_pct
  )
) +
  geom_point()
