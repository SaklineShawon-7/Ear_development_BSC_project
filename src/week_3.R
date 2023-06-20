df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))
df %>% dplyr::glimpse() 
names(df)
# extract column from dataframe
df$thermal_time
df[,3]
df[,'thermal_time']

df[['thermal_time']]

# not work
df['thermal_time']
# different error message
#!! name space conflict
df['time']
time

# summarize dataframe (starting and ending)
lapply(df, range)
# turn as data frame
lapply(df, range) %>% data.frame()

summary(df)

####
read.csv("")

climate <- read_csv("data/climate.csv")

climate %>% dplyr::glimpse() 
names(climate)
climate$y
climate[4]
# summarize dataframe (starting and ending)
lapply(climate, range)
lapply(climate, unique)
climate$Acc_Temperature='2018-10-25'
climate_filtered <- climate %>% filter(time == as.Date("2018-10-25"))
climate %>% dplyr::filter(time==as.Date('2018-10-25')).$Acc_Temperature
#########

df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))
# with same length dataframe
ear_df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                     ear_weight=c(20,40,50))
merge(df,ear_df,by="time")
dplyr::left_join(df,ear_df,by="time")
# combind with vector of same length 
cbind(df, ear_weight=c(20,40,50))
df$ear_weight <- c(20,40)

# with differnt length 
short_ear_df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,2,1),
                           ear_weight=c(20,40))
merge(df,short_ear_df,by="time")
dplyr::left_join(df,short_ear_df,by="time")

# combind with vector of different length 
cbind(df, ear_weight=c(20,40))
df$ear_weight <- c(20,40)


#######
read.csv("")

phenology_short <- read_csv("data/phenology_short.csv")

phenology_short %>% 
  ggplot(aes(x=date,y=weight,color=var))+
  geom_point()+
  geom_line(aes(group=group))+ # link the point by group.
  xlab("date of harvest")+ #x axis title
  ylab("ear weight(g)")+   #y axis title
  guides(color=guide_legend(title="Cultivar")) #change legend title 


data <- read_csv("data/ear_summarized.csv")
data %>% 
  ggplot(aes(x=date,y=weight,color=var))+
  geom_point()+
  geom_line(aes(group=group))+ # link the point by group.
  xlab("date of harvest")+ #x axis title
  ylab("ear weight(g)")+   #y axis title
  guides(color=guide_legend(title="Cultivar")) ##theme
#change legend title 

