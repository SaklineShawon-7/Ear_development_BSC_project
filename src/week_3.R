df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))
df
df %>% dplyr::glimpse()

df %>% glimpse
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

# summarize dataframe (starting and ending)####
lapply(df, range)
# turn as data frame
lapply(df, range) %>% data.frame()

lapply(df,range) %>% data.frame()

summary(df)

####
read.csv("")

climate <- read_csv("data/climate.csv")
climate

climate %>% dplyr::glimpse() 
names(climate)
climate$y
climate[,4]
# summarize dataframe (starting and ending)
lapply(climate, range)
lapply(climate, unique)

str(climate$DayTime)
climate_filtered <- climate[,'DayTime']
climate_filtered
#["2018-10-25"]

climate %>% filter(DayTime== as.Date("2018-10-25", format="%Y-%m-%d")) %>% .$Acc_Temperature
climate $Acc_Temperature

#########
# syntax of using pipe

print('object')

'object' %>% print(.)
'object' %>% print()
'object' %>% print() %>% .
'object' %>% print(.) %>% .

#embedded functions
fun2(fun1(object))
# pipe
object %>% 
  fun1(3) %>% 
  fun2()



df1 <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                 temp=c(20,15,13), 
                 thermal_time=cumsum(c(20,15,13)))
df1
# with same length dataframe
ear_df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,3,1),
                     ear_weight=c(20,40,50))
ear_df
merge(df,ear_df,by="time")
dplyr::left_join(df,ear_df,by="time")
# combind with vector of same length 
cbind(df, ear_weight=c(20,40,50))

df1$ear_weight1 <- c(20,40,50)


# with differnt length 
short_ear_df <- data.frame(time=as.Date("2023-04-16",format="%Y-%m-%d")+seq(1,2,1),
                           ear_weight=c(20,40))
short_ear_df
merge(df,short_ear_df,by="time")
dplyr::left_join(df,short_ear_df,by="time") #dplyr is more specific

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


#Day_04####

df <- expand.grid(x=letters[1:4],
                  y=1:2)
library(readr)
climate <- read_csv("data/climate.csv")
g<-climate%>% mutate(l=paste(DFG_year,Acc_Temperature,sep = "-"))
View(g)

df%>% mutate(z=paste(x,y))
df%>% mutate(z=paste(x,y,sep = "-"))

df %>% tidyr::unite(data = .,col = "z",c(x,y))
df <- df %>% mutate(z=interaction(x,y))

k<-climate%>% mutate(o=interaction(DFG_year,Acc_Temperature,sep = "-"))
View(k)


#Day_05####
de <- data.frame(
  x1=1:3,
  x2=letters[1:3],
  x3=c("2a","2b","2c")
)
# or condition separate by |
de$x1==2|df$x3=="2c"

de %>% filter(x1==2|x3=="2c")
de %>% with(.,x1==2|x3=="2c")

# when not specifying the comma, it will be treated like column
df %>% with(.,.[x1==2|x3=="2c"])
# specify the rows
df %>% with(.,.[x1==2|x3=="2c",])
