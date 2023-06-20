climate <- read_csv("data/climate.csv")
glimpse(climate)
climate%>% group_by(DFG_year, sowing_date ) %>% filter(DayTime==min(DayTime)) %>% select( DayTime)

climate_sub <- climate %>% 
  dplyr::select(DayTime,DailyMean_Temperature,DFG_year,sowing_date)

thermal_time <-mutate(climate_sub, thermal_time=DailyMean_Temperature )
