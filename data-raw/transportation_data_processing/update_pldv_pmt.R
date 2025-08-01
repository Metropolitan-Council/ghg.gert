# Update PLDV forecast using most recent regional model outputs
#


pldv_avo <- ghg.ccap::transportation_data$passenger %>%
  filter(mode == "PLDV",
         var == "AVO") %>%
  mutate(AVO = value)


mean(pldv_avo$AVO)

coctu_vmt_forecast <- readRDS("../../MTS/metc_travel_model/data/ctu_vmt_forecast.RDS") %>%
  mutate(geog_id = gnis)


ctu_vmt_forecast <- coctu_vmt_forecast %>%
  filter(geog_id %in% geog_index$geog_id) %>%
  group_by(geog_id, vmt_year) %>%
  summarize(annual_passenger_vmt = sum(network_passenger_vmt) * 340, .groups = "keep") %>%
  ungroup() %>%
  right_join(pldv_avo %>%
              select(geog_id, geog_name, year, AVO) %>%
              unique(),
            by = c("geog_id", "vmt_year" = "year")) %>%
  arrange(geog_name, vmt_year) %>%
  filter(vmt_year >= 2025) %>%
  group_by(geog_id, geog_name) %>%
  mutate(
    annual_passenger_vmt_full = zoo::na.approx(annual_passenger_vmt),
    annual_passenger_pmt = annual_passenger_vmt_full * AVO
  )


ctu_vmt_forecast
