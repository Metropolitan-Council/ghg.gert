####### Create plots for the BAU results #######

## Cumulative VMT plot by mode for personal vehicles
mode_summary <- bau_summary %>%
  filter(type == "P", output == "VMT") %>%
  group_by(mode) %>%
  select(append("mode", YRS)) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(YRS) %>%
  pivot_wider(name, mode)
pvmtfig <- plot_ly(mode_summary, x = ~name, y = ~PLDV, name = "Private Car", type = "scatter", mode = "none", stackgroup = "one")
pvmtfig <- pvmtfig %>% add_trace(y = ~BU, name = "Bus")
pvmtfig <- pvmtfig %>% add_trace(y = ~BRT, name = "BRT")
pvmtfig <- pvmtfig %>% add_trace(y = ~RU, name = "LRT")
pvmtfig <- pvmtfig %>% add_trace(y = ~RI, name = "Northstar")
pvmtfig <- pvmtfig %>% add_trace(y = ~BS, name = "School Bus")
pvmtfig <- pvmtfig %>% add_trace(y = ~BIKE, name = "Bike")
pvmtfig <- pvmtfig %>% add_trace(y = ~WALK, name = "Walk")
pvmtfig <- pvmtfig %>% layout(
  title = sprintf("%s Passenger VMT by Mode - Scenario %s", ch_ctu, scen),
  xaxis = list(
    title = "",
    showgrid = FALSE
  ),
  yaxis = list(
    title = "VMT (100 million miles)",
    showgrid = FALSE
  )
)

pvmtfig


## Cumulative TVMT plot by mode for freight vehicles
mode_summary <- bau_summary %>%
  filter(type == "F", output == "TVMT") %>%
  group_by(mode) %>%
  select(append("mode", YRS)) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(YRS) %>%
  pivot_wider(name, mode)
fvmtfig <- plot_ly(mode_summary, x = ~name, y = ~CUT, name = "Combination Truck", type = "scatter", mode = "none", stackgroup = "one")
fvmtfig <- fvmtfig %>% add_trace(y = ~SUT, name = "Single Unit Truck")
fvmtfig <- fvmtfig %>% add_trace(y = ~FR, name = "Rail")
fvmtfig <- fvmtfig %>% add_trace(y = ~MM, name = "Multimodal")
fvmtfig <- fvmtfig %>% add_trace(y = ~AIR, name = "Air")
fvmtfig <- fvmtfig %>% add_trace(y = ~WAT, name = "Water")
fvmtfig <- fvmtfig %>% layout(
  title = sprintf("%s Freight TVMT by Mode - Scenario %s", ch_ctu, scen),
  xaxis = list(
    title = "",
    showgrid = FALSE
  ),
  yaxis = list(
    title = "TVMT (100 million ton-miles)",
    showgrid = FALSE
  )
)

fvmtfig


## Person travel GHG by fuel class
pldv_ghg_summary <- bau_summary %>%
  filter(type == "P", output == "DIR-GHG") %>%
  group_by(class) %>%
  select(append("class", YRS)) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(YRS) %>%
  pivot_wider(name, class)
pghgfig <- plot_ly(pldv_ghg_summary, x = ~name, y = ~SI, name = "Gasoline", type = "scatter", mode = "none", stackgroup = "one")
pghgfig <- pghgfig %>% add_trace(y = ~CI, name = "Diesel")
pghgfig <- pghgfig %>% add_trace(y = ~BCI, name = "Biodiesel")
pghgfig <- pghgfig %>% add_trace(y = ~HEV, name = "HEV")
pghgfig <- pghgfig %>% add_trace(y = ~PHEV, name = "PHEV")
pghgfig <- pghgfig %>% add_trace(y = ~BEV, name = "BEV")
pghgfig <- pghgfig %>% layout(
  title = sprintf("%s GHG for Passenger Cars by Fuel Type - Scenario %s", ch_ctu, scen),
  xaxis = list(
    title = "",
    showgrid = FALSE
  ),
  yaxis = list(
    title = "GHG (thousand metric tonnes CO2eq)",
    showgrid = FALSE
  )
)

pghgfig

## Freight vehicle GHG by fuel class
freight_ghg_summary <- bau_summary %>%
  filter(type == "F", output == "DIR-GHG") %>%
  group_by(class) %>%
  select(append("class", YRS)) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(YRS) %>%
  pivot_wider(name, class)
fghgfig <- plot_ly(freight_ghg_summary, x = ~name, y = ~CI, name = "Diesel", type = "scatter", mode = "none", stackgroup = "one")
fghgfig <- fghgfig %>% add_trace(y = ~BEV, name = "BEV")
fghgfig <- fghgfig %>% layout(
  title = sprintf("%s GHG for Freight by Fuel Type (Trucks Only) - Scenario %s", ch_ctu, scen),
  xaxis = list(
    title = "",
    showgrid = FALSE
  ),
  yaxis = list(
    title = "GHG (thousand metric tonnes CO2eq)",
    showgrid = FALSE
  )
)

fghgfig


# Embodied GHG from personal vehicles and transit vehicles
pass_emb_summary <- bau_summary %>%
  filter(type == "P", output == "INDIR-GHG") %>%
  group_by(mode, class) %>%
  select(append("class", YRS)) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(YRS, names_to = "YRS", values_to = "GHG")
pass_emb_summary <- pass_emb_summary %>%
  unite("cat", mode:class) %>%
  pivot_wider(names_from = cat, values_from = GHG)

embfig <- plot_ly(pass_emb_summary, x = ~YRS, y = ~PLDV_SI, name = "LDV-Gasoline", type = "bar")
embfig <- embfig %>% add_trace(y = ~PLDV_CI, name = "LDV-Diesel")
embfig <- embfig %>% add_trace(y = ~PLDV_HEV, name = "LDV-HEV")
embfig <- embfig %>% add_trace(y = ~PLDV_PHEV, name = "LDV-PHEV")
embfig <- embfig %>% add_trace(y = ~PLDV_BEV, name = "LDV-BEV")
embfig <- embfig %>% add_trace(y = ~BU_BCI, name = "Bus-Diesel")
embfig <- embfig %>% add_trace(y = ~BU_HEV, name = "Bus-HEV")
embfig <- embfig %>% add_trace(y = ~BU_BEV, name = "Bus-BEV")
embfig <- embfig %>% add_trace(y = ~BRT_BCI, name = "BRT-Diesel")
embfig <- embfig %>% add_trace(y = ~BRT_HEV, name = "BRT-HEV")
embfig <- embfig %>% add_trace(y = ~BRT_BEV, name = "BRT-BEV")

embfig <- embfig %>% layout(
  title = sprintf("%s Embodied GHG (Manufacturing + Maintenance) for Passenger Modes - Scenario %s", ch_ctu, scen),
  yaxis = list(
    title = "GHG (thousand metric tonnes CO2eq)",
    showgrid = FALSE
  ),
  barmode = "stack", colorway = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#000000")
)

embfig
