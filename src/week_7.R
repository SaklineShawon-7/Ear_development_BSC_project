climate <- read_csv("data/climate.csv")
climate %>% 
  dplyr::filter(DFG_year%in%c("DFG2019","DFG2020")) %>% 
  group_by(DFG_year,sowing_date) %>% 
  mutate(DayTime=as.Date(DayTime,format="%Y-%m-%d"),
         DAS=as.numeric(DayTime-min(DayTime))) %>% 
  ggplot(aes(x=DAS,y=Acc_Temperature,color=DFG_year,
             group=interaction(sowing_date,DFG_year)))+
  # geom_point()+
  geom_line(aes(linetype=sowing_date),linewidth=1)+
  theme_bw()+
  theme(legend.position = c(.1,.65))+
  labs(x="Days after sowing",y= "Thermal sum (°Cd)")+
  guides(color=guide_legend(title="Year"))

# climate %>%glimpse()
climate_long <- climate %>% 
  tidyr::pivot_longer(names_to = "Daily_Terms",
                      values_to = "Daily_value",
                      cols = contains("Daily")) 
# climate_long%>%   names()

#select cols by position
# grep("(Daily|Acc)",names(climate))
climate_long <- climate %>% 
  tidyr::pivot_longer(names_to = "Terms",
                      values_to = "value",
                      # select both patterns
                      cols = grep("(Daily|Acc)",names(.)))

# climate_long%>% names()


for (variable in vector) {
  
}

x<-c(1:10)
m=x+2
## data processing example
climate_long_subset<- climate_long %>% 
  filter(Terms%in%c('Acc_Temperature','Acc_Precipitation')) %>% 
  group_by(DFG_year,sowing_date,Terms) %>%
  summarise(Value=mean(value))

climate_long_subset