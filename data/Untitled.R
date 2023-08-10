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
p <- "data/Grain Counting/gc_48_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))

graindd
View(graindd)

##the aborted kernels for each spikelet position and floret number
ak<-graindd$AbortedKernelsPerSpikelet <- abs(graindd$`total kernel` - graindd$flower)
View(ak)

# the abortion rate for each spikelet position
akp<- graindd$AbortionRatePerSpikelet <- round((graindd$AbortedKernelsPerSpikelet / graindd$flower) * 100,0)

View(graindd)

#Kernel Abortion Rate by Spike Position
ggplot(graindd, aes(x = spike, y = AbortionRatePerSpikelet)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Spike Position", y = "Kernel Abortion Rate") +
  ggtitle("Kernel Abortion Rate by Spike Position")

# scatter plot for spikelet position vs. abortion rate
ggplot(graindd, aes(y = spike, x = AbortionRatePerSpikelet, color= batch)) +
  geom_point() +
  geom_path(alpha=.5)+
  #geom_smooth(method = "lm", se = FALSE) +
  labs(y = "spike", x = "Abortion Percentage") +
  theme_classic()

# Perform ANOVA
model <- aov(AbortionRatePerSpikelet ~ spike, data = graindd)

# Check the ANOVA table
anova_table <- summary(model)
print(anova_table)


library(dplyr)

graindd1 <- graindd %>% 
  group_by(batch, plot_id, var) %>% 
  mutate(type = cut(as.numeric(spike), 3) %>% as.numeric(),
         type = case_when(type == 1 ~ "basal",
                          type == 2 ~ "central",
                          TRUE ~ "apical"))
View(graindd1)

graindd1%>% 
  group_by(batch,var,plot_id,type,spike) %>% 
  ggplot(aes(kernel.size,spike,color=type))+
  geom_point()+
  #facet_grid(~student)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")+
  geom_path()


df_long <- pivot_longer(graindd, cols = c(kernel.type, kernel.size), names_to = "variable", values_to = "value")


graindd %>% 
  group_by(batch, plot_id, var) %>% 
  mutate(type = cut(as.numeric(spike), 3) %>% as.numeric(),
         type = case_when(type == 1 ~ "basal",
                          type == 2 ~ "central",
                          TRUE ~ "apical"))
graindd %>% 
  pivot_longer(starts_with("kernel"),
               values_to = "kernel",
               names_to="kerneltype")%>% 
  #group_by(student,spike) %>% 
  ggplot(aes(kerneltype,spike))+
  geom_point()+
  geom_path()+
  #facet_grid(kerneltype~student)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "none")



graindd%>% 
  #group_by(type,spike) %>% 
  summarise(kernel.type=max(kernel.L)) %>% 
  ggplot(aes(flower,spike,color=type))+
  geom_point()+
  #facet_grid(~student)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")+
  geom_path()


p <- "data/Grain Counting/gc_50_1.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd2<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
})

graindd2 %>% plot_fun()

graindd2



x  <- rbind(graindd,graindd2) 
View(x)
dy<-x[,c(1,2,4,5)]
dy

dy %>%pivot_longer(starts_with("flower"),
               values_to = "flower",
               names_to="plot_id") %>% 
  group_by(var,spike) %>% 
  ggplot(aes(flower,spike,color=var))+
  geom_point()+
  geom_path()+
  facet_grid(plot_id~var)+
  theme_classic()

  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "none")
z= melt(y, id.vars = 'var')
view(z)
x %>%
  ggplot(aes(x = flower, y = spike)) +
  geom_line()+
  facet_grid(~plot_id)

x %>% 
  group_by(var, spike ) %>% 
  ggplot(aes(spike,flower,color=var ))+
  geom_line()+
  geom_point()+
  #geom_path(alpha=.5)+
  facet_grid(~plot_id)+
  theme(legend.position = "bottom")

df <- df %>% mutate(abortion_rate = round((empty_floret / total_floret_number) * 100, 2))


df%>% 
  group_by(student,var,plot_id,type,spike) %>% 
  summarise(fertile_flower=max(kernel.full)) %>% 
  ggplot(aes(fertile_flower,spike,color=type))+
  geom_point()+
  facet_grid(~student)+
  theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom")+
  geom_path()