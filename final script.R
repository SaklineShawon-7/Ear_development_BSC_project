rm(list = ls())
library(dplyr)
library(ggplot2)
readx<- function(p,sh){
  df <- readxl::read_xlsx(p,sheet = sh) %>% 
    mutate(across(starts_with("kernel"),function(x)as.character(x))) %>% 
    tidyr::pivot_longer(starts_with("kernel"),names_to = "kernel.type",values_to = "floret.pos") %>% 
    mutate(floret.pos=strsplit(floret.pos,",")) %>% 
    tidyr::unnest(floret.pos) %>% 
    mutate(floret.pos=as.numeric(floret.pos) %>%replace(., .==0, NA),
           kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
             # create contrast
             ifelse(.==3,5,.))
  return(df)
}

plot_fun <- function(df){
  # find floret where more than two types were recorded for same position  
  sdf <- df %>% group_by(rep,spike,floret.pos) %>% 
    summarise(n=n()) %>% 
    filter(n>1) %>% 
    mutate(lab=paste0("Check\n(Nsp,Nf)\n(",spike,',',floret.pos,')'))
  
  p <- df %>%
    ggplot(aes(floret.pos,spike))+
    geom_point(alpha=.4,aes(size=kernel.size,fill=kernel.size),
               shape=21)+
    facet_wrap(~rep)+
    theme_bw()+
    ggrepel::geom_label_repel(data = sdf, mapping=aes(floret.pos,spike, label = lab),color="red",
                              box.padding = 1,size=3)+
    scale_fill_viridis_c(guide = "legend",breaks = c(1, 2, 5),
                         labels = paste0("kernel.",c("S","M","L"))) +
    scale_size_continuous(breaks = c(1, 2, 3), range = c(1, 5),
                          labels = paste0("kernel.",c("S","M","L")))+
    theme(panel.grid.minor.x=element_blank(),strip.background = element_blank(),
          legend.position = "bottom")
  return(p)
  
}
# ------------------------------------------------------------------------








#hypothesis: Grain size may varies along the spike position in a spike####

p <- "data/Grain Counting/gc_48_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd1<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))
graindd1 %>% plot_fun()
View(graindd1)

p <- "data/Grain Counting/gc_47_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd2<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))
graindd2 %>% plot_fun()
View(graindd2)

#combine data set for plot: 47,48  Var: pioner,patras, batch: 11
graindd  <- rbind(graindd1,graindd2) 
View(graindd)

graindd %>%
  mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
  mutate(kernel.size=NULL) %>%
  # calculate total kernel number of each kernel type
  group_by(var,plot_id,spike,kernel.type,rep,batch,timeid) %>%
  reframe(var, kernel.num = sum(kernel.num, na.rm = FALSE), kernel.type) %>%
  # calculate mean kernel number of each kernel type 
  group_by( var,spike, kernel.type, batch,timeid) %>%
  reframe(plot_id,kernel.num = mean(kernel.num, na.rm=FALSE)) %>%
  ungroup()%>%
  mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
  mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                kernel.pos == 2 ~"central",
                                T ~"apical")) %>%
  mutate(kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
           # create contrast for kernel size
           ifelse(.==3,5,.)) %>% unique() %>%
  mutate(kernel.num = ifelse(kernel.num == 0, NA, kernel.num)) %>%
  #plotting
  ggplot(aes(kernel.num, spike))+
  geom_path(alpha = 0.3)+
  geom_point(alpha=.5,aes(size=kernel.size,fill=kernel.pos),
             shape=21)+
  #facet_wrap(var~kernel.type)+
  facet_grid(var~kernel.type)+
  theme_classic()+
  theme(strip.background = element_rect(linewidth = 0.5),
        panel.grid.major.y = element_line(linewidth = 0.5),
        legend.position = "bottom")+
  labs(
    x= 'Different sized grain(avg) number',
    y='spike position',
    title = "Hypothesis: Grain size may varies along the spike position in a spike",
    
    subtitle = 'Data Set: plot_47,48  Var: Pioner,Patras Batch: 11'
  )

###2nd graph###
graindd %>% group_by(spike,plot_id) %>% 
  summarise(batch,var,mean.kernel=mean(`total kernel`)) %>% 
  group_by(batch, plot_id, var) %>% 
  mutate(type = cut(as.numeric(spike), 3) %>% as.numeric(),
         type = case_when(type == 1 ~ "basal",
                          type == 2 ~ "central",
                          TRUE ~ "apical"))%>% 
  group_by(batch,var,plot_id,type,spike) %>% 
  ggplot(aes(mean.kernel,spike,color=type))+ ### ggplot(aes(mean.kernel,spike,color=plot_id))
  geom_point()+
  facet_grid(plot_id~var)+
  theme_classic()+
  geom_path(alpha = 0.3)+
  geom_point(alpha=.5,shape=21)+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")+
  geom_path()+
  labs(
    y='spike position',
    title = "Hypothesis:01",
    
    subtitle = 'Data Set: plot_47,48  Var: Pioner,Patras Batch: 11'
  )

############################################################################

#hypothesis_2:treatment may affect the number of fertile floret in a spike####

p <- "data/Grain Counting/gc_47_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd2<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))
View(graindd2)

x<-graindd2[,1:10]
View(x)

p <- "data/Grain Counting/gc_47_1.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd5<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
})
View(graindd5)
y<-graindd5[,1:10]
View(y)

p <- "data/Grain Counting/gc_47_6.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd6<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
})
View(graindd6)
z<-graindd6[,1:10]
View(z)

#combine data set for plot: 47  Var: Pioner batch: 11+1+6
graindd  <- rbind(x,y,z) 
View(graindd)

#pltoting####
graindd %>% group_by(spike,plot_id,nitrogen) %>% 
  summarise(batch,var,mean.flower=mean(flower)) %>% 
  group_by(batch, plot_id, var,nitrogen) %>% 
  mutate(type = cut(as.numeric(spike), 3) %>% as.numeric(),
         type = case_when(type == 1 ~ "basal",
                          type == 2 ~ "central",
                          TRUE ~ "apical"))%>% 
  group_by(batch,var,plot_id,type,spike) %>% 
  ggplot(aes(y=mean.flower,x=spike,color=type))+
  geom_point()+
  facet_grid(batch~nitrogen)+
  theme_classic()+
  geom_path(alpha = 0.3)+
  geom_point(alpha=.5,shape=21)+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")+
  geom_path()+
  labs(
    title = "Hypothesis:treatment may affect the number of fertile floreta in a spike",
    
    subtitle = 'Data Set: plot_47  Var: Pioner Nitrogen:176 Batch: 1+6+11'
  )


#pltoting####2nd graph####

p <- "data/Grain Counting/gc_48_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd2<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))
View(graindd1)

p <- "data/Grain Counting/gc_47_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd2<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
})%>% filter(!is.na(floret.pos))
View(graindd2)

#combine data set for plot: 47  Var: Pioner batch: 11+1+6
graindd  <- rbind(graindd1,graindd2) 
View(graindd)

graindd %>% group_by(spike,plot_id,nitrogen) %>% 
  summarise(batch,var,mean.flower=mean(flower)) %>% 
  group_by(batch, plot_id, var,nitrogen) %>% 
  mutate(type = cut(as.numeric(spike), 3) %>% as.numeric(),
         type = case_when(type == 1 ~ "basal",
                          type == 2 ~ "central",
                          TRUE ~ "apical"))%>% 
  group_by(batch,var,plot_id,type,spike) %>% 
  ggplot(aes(x=mean.flower,y=spike,color=type))+
  geom_boxplot()+
  facet_grid(nitrogen~var)+
  theme_classic()+
  geom_path(alpha = 0.3)+
  geom_point(alpha=.5,shape=21)+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")+
  geom_path()+
  labs(
    title = "Hypothesis:treatment may affect the number of fertile floret in a spike",
    
    subtitle = 'Data Set: plot_47,48  Var: Pioner,Patras Batch: 11'
  )+
  stat_compare_means(method = "anova", label = "p.signif")


#hypothesis_3:sowing time may have affect on the number of floret fertility in a spike####

p <- "data/Grain Counting/gc_48_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd1<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
})
View(graindd1)
#subsetting the table
x<-graindd1[,c(1,2,3,4,5,6,10)]

View(x)

p <- "data/Grain Counting/gc_50_1.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd2<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
})
View(graindd2)

#subsetting the table
y<-graindd2[,c(1,2,3,4,5,6,10)]

View(y)

p <- "data/Grain Counting/gc_47_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd3<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
})%>% filter(!is.na(floret.pos))
View(graindd3)

#subsetting the table
z<-graindd3[,c(1,2,3,4,5,6,10)]

View(z)

##combine data set
graindd<-rbind(x,y)


library(ggplot2)

graindd %>% 
  group_by(spike, plot_id, timeid) %>% 
  summarise(batch, var, mean.flower = mean(flower)) %>% 
  group_by(batch, spike, timeid) %>% 
  ggplot(aes(y = mean.flower, x = timeid, color = timeid)) +  # Adding color aesthetic for better distinction
  geom_boxplot() +
  theme_classic() +
  facet_grid(batch~var)+
  theme(
    strip.background = element_blank(),
    panel.grid.major.x = element_line(),
    legend.position = "bottom"
  ) +
  geom_path() +
  labs(
    title = "Hypothesis: Sowing Time may affect the number of floret fertility in a spike",
    subtitle = "Data Set: plot_48,50  Var:Patras Batch: 11+1"
  ) +
  xlab("Sowing Time") +
  ylab("Mean Floret Fertility") +
  scale_color_discrete(name = "Sowing Time")  # Customizing the color legend

library(ggplot2)
library(ggpubr)



##plot_02
##combine data set
graindd7<-rbind(y,z)

graindd7 %>% group_by(spike,plot_id,timeid) %>% 
  summarise(batch,var,mean.flower=mean(flower)) %>% 
  group_by(batch, plot_id, var,timeid) %>% 
  ggplot(aes(y=mean.flower,x=timeid))+
  geom_boxplot()+
  facet_grid(plot_id~var)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")+
  geom_path()+
  labs(
    title = "Hypothesis:treatment may affect the number of fertile floret in a spike",
    
    subtitle = 'Data Set: plot_47,48  Var: Pioner,Patras Batch: 11+1'
  )



library(ggplot2)
library(ggpubr)

graindd %>% 
  group_by(spike, plot_id, timeid) %>% 
  summarise(batch, var, mean.flower = mean(flower)) %>% 
  group_by(batch, spike, timeid) %>% 
  ggplot(aes(x = mean.flower, y = timeid, color = timeid)) +
  geom_boxplot() +
  theme_classic() +
  facet_grid(batch ~ var) +
  theme(
    strip.background = element_blank(),
    panel.grid.major.x = element_line(),
    legend.position = "bottom"
  ) +
  geom_path() +
  labs(
    title = "Hypothesis: Sowing Time may affect the number of floret fertility in a spike",
    subtitle = "Data Set: plot_48,50  Var: Patras Batch: 11+1"
  ) +
  xlab("Sowing Time") +
  ylab("Mean Floret Fertility") +
  scale_color_discrete(name = "Sowing Time") +
  stat_compare_means(method = "t.test", label = "p.signif")  # Adding t-test comparison labels



library(ggplot2)
library(ggpubr)

# Modify the code to ensure the data frame used in plotting is correct
graindd %>%
  group_by(spike, plot_id, timeid) %>%
  summarise(batch, var, mean.flower = mean(flower)) %>%
  group_by(batch, spike, timeid) %>%
  ungroup() %>%
  ggplot(aes(y = mean.flower, x = timeid, color = timeid)) +
  geom_boxplot() +
  theme_classic() +
  facet_grid(batch ~ var) +
  theme(
    strip.background = element_blank(),
    panel.grid.major.x = element_line(),
    legend.position = "bottom"
  ) +
  geom_path() +
  labs(
    title = "Hypothesis: Sowing Time may affect the number of floret fertility in a spike",
    subtitle = "Data Set: plot_48,50  Var: Patras Batch: 11+1"
  ) +
  xlab("Sowing Time") +
  ylab("Mean Floret Fertility") +
  scale_color_discrete(name = "Sowing Time") +
  stat_compare_means(method = "t.test", label = "p.signif")

p <- ggboxplot(graindd, x = "supp", y = "len",
               color = "supp", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means()
# Change method
p + stat_compare_means(method = "t.test")











##the aborted kernels for each spikelet position
graindd$AbortedKernelsPerSpikelet <- abs(graindd$`total kernel` - graindd$flower)

# the abortion rate for each spikelet position
graindd$AbortionRatePerSpikelet <- round((graindd$AbortedKernelsPerSpikelet / graindd$flower) * 100,0)


##plotting
graindd %>% group_by(spike,plot_id,timeid) %>% 
  summarise(batch,var,mean.AbortionRatePerSpikelet=mean(AbortionRatePerSpikelet)) %>% 
  group_by(timeid,var,plot_id,spike) %>% 
  ggplot(aes(spike,y=mean.AbortionRatePerSpikelet, color=timeid))+
  geom_boxplot()+
  facet_grid(~var)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")+
  geom_path()+
  labs(
    title = "Hypothesis:treatment may affect the number of fertile floret in a spike",
    
    subtitle = 'Data Set: plot_47,48  Var: Pioner,Patras Batch: 11'
  )



library(datasets)
library(ggplot2)
library(multcompView)
library(dplyr)

anova_result <- aov(spike ~ flower, data = graindd)
  
  # Extract the p-value from the ANOVA result
  p_value <- summary(anova_result)[["Pr(>F)"]][1]
  
  View(p_value)

# loading and checking the data
str(graindd)

# analysis of variance
# Perform ANOVA
anova_result <- aov(spike ~ flower, data = graindd)

# Extract the p-value from the ANOVA result
p_value <- summary(anova_result)[["Pr(>F)"]][1]

# Create the ggplot with the p-value annotation
ggplot(merged_df, aes(x = spikelet_position, y = flower_number)) +
  geom_boxplot() +
  labs(x = "Spikelet Position", y = "Flower Number") +
  theme_classic() +
  annotate("text", x = 1, y = max(merged_df$flower_number) * 0.9,
           label = paste("p-value =", round(p_value, 3)), hjust = 0, size = 4)

View(df)
merged_df <- inner_join(df, df_flower, by = "spikelet_position")

anova <- aov(`total kernel`~flower, data = df)
summary(anova)
# compact letter display
cld <- multcompLetters(anova)
print(cld)
# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)
#sowing date can effect the ultimate yield on different varitiess


##the aborted kernels for each spikelet position and floret number
ak<-graindd$AbortedKernelsPerSpikelet <- abs(graindd$`total kernel` - graindd$flower)
View(ak)

# the abortion rate for each spikelet position
akp<- graindd$AbortionRatePerSpikelet <- round((graindd$AbortedKernelsPerSpikelet / graindd$flower) * 100,0)

graindd %>% group_by(spike,plot_id) %>% 
  summarise(batch,var,mean.abortionrate=mean(AbortionRatePerSpikelet)) %>% 
  group_by(batch, plot_id, var) %>% 
  mutate(type = cut(as.numeric(spike), 3) %>% as.numeric(),
         type = case_when(type == 1 ~ "basal",
                          type == 2 ~ "central",
                          TRUE ~ "apical"))%>% 
  group_by(batch,var,plot_id,type,spike) %>% 
  ggplot(aes(mean.abortionrate,spike,color=type))+ ### ggplot(aes(mean.kernel,spike,color=plot_id))
  geom_point()+
  #facet_grid(~student)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")+
  facet_grid(~var)+
  geom_path()





#####





#Kernel Abortion Rate by Spike Position
ggplot(graindd, aes(x = spike, y = AbortionRatePerSpikelet)) +
  geom_point(fill = "lightblue", color = "black") +
  labs(x = "Spike Position", y = "Kernel Abortion Rate") +
  ggtitle("Kernel Abortion Rate by Spike Position")
View(graindd)
ggplot(graindd, aes(y = spike, x = AbortionRatePerSpikelet, color= batch)) +
  geom_point() +
  geom_path(alpha=.5)+
  #geom_smooth(method = "lm", se = FALSE) +
  labs(y = "spike", x = "Abortion Percentage") +
  theme_classic()

View(graindd)
