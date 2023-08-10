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
#data set from batch:01, plot: 47, var: Pionier
p <- "data/Grain Counting/gc_47_1.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd1<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))
graindd1 %>% plot_fun()
graindd1
View(graindd1)

#data set from batch:06, plot: 47, var: Pionier

p <- "data/Grain Counting/gc_47_6.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd2<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))
graindd2 %>% plot_fun()
graindd2
View(graindd2)



#combine data set for plot: 47, pioner,batch: 1+6+11

graindd  <- rbind(graindd2,graindd3) 
View(graindd)








#hypothesis: Grain Size may varies along the spike position in a spike####

#data set from batch:11, plot: 47, var: Pionier
p <- "data/Grain Counting/gc_47_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd3<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))
graindd1 %>% plot_fun()
graindd1
View(graindd1)

#data set from batch:11, plot: 48, var: Patras
p <- "data/Grain Counting/gc_47_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd3<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))
graindd2 %>% plot_fun()
graindd2
View(graindd2)

#combine data set for plot: 47,48  Var: pioner,patras, batch: 11
graindd  <- rbind(graindd1,graindd2) 
View(graindd)
graindd %>%
  # 1. document kernel number
  mutate(kernel.num = ifelse(floret.pos == 0, 0, 1)) %>%
  mutate(kernel.size=NULL) %>%
  # 2. calculate total kernel number of each kernel type
  #    on each spike for each treatment in each rep for each batch
  group_by(var,plot_id,spike,kernel.type,rep,batch,timeid) %>%
  reframe(var, kernel.num = sum(kernel.num, na.rm = FALSE), kernel.type) %>%
  # 3. calculate mean kernel number of each kernel type 
  #    on each spike
  group_by( var,spike, kernel.type, batch,timeid) %>%
  reframe(plot_id,kernel.num = mean(kernel.num, na.rm=FALSE)) %>%
  # 4. restore kernel position    
  ungroup()%>%
  mutate(kernel.pos = as.numeric(cut(spike,breaks=3))) %>%
  mutate(kernel.pos = case_when(kernel.pos == 1 ~"basal",
                                kernel.pos == 2 ~"central",
                                T ~"apical")) %>%
  mutate(kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
           # create contrast for kernel size
           ifelse(.==3,5,.)) %>%
  unique() %>% #remove repetitive row
  mutate(kernel.num = ifelse(kernel.num == 0, NA, kernel.num)) %>%
  # 5. plot data 
  ggplot(aes(kernel.num, spike))+
  geom_path(alpha = 0.3)+
  geom_point(alpha=.5,aes(size=kernel.size,fill=kernel.pos),
             shape=21)+
  facet_grid(batch~kernel.type)+
  theme_classic()+
theme(strip.background = element_rect(linewidth = 0.5),
      panel.grid.major.y = element_line(linewidth = 0.5),
      legend.position = "bottom")


