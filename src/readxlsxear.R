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
# -------------------------------------------------------------------------
p <- "data/Grain Counting/gc_48_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df<-readx(p,10)
graindd<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
}) %>% filter(!is.na(floret.pos))

graindd %>% plot_fun()

graindd

library(dplyr)
library(ggplot2)
readx<- function(p,sh){
  df1 <- readxl::read_xlsx(p,sheet = sh) %>% 
    mutate(across(starts_with("kernel"),function(x)as.character(x))) %>% 
    tidyr::pivot_longer(starts_with("kernel"),names_to = "kernel.type",values_to = "floret.pos") %>% 
    mutate(floret.pos=strsplit(floret.pos,",")) %>% 
    tidyr::unnest(floret.pos) %>% 
    mutate(floret.pos=as.numeric(floret.pos) %>%replace(., .==0, NA),
           kernel.size=factor(kernel.type,levels=paste0("kernel.",c("S","M","L"))) %>% as.numeric() %>% 
             # create contrast
             ifelse(.==3,5,.))
  return(df1)
}

plot_fun1 <- function(df1){
  # find floret where more than two types were recorded for same position  
  sdf <- df1 %>% group_by(rep,spike,floret.pos) %>% 
    summarise(n=n()) %>% 
    filter(n>1) %>% 
    mutate(lab=paste0("Check\n(Nsp,Nf)\n(",spike,',',floret.pos,')'))
  
  p <- df1 %>%
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
# -------------------------------------------------------------------------
p <- "data/Grain Counting/gc_50_11.xlsx"
readxl::read_xlsx(p,sheet = 1)
df1<-readx(p,9)
graindd2<- purrr::map_dfr(1:length(readxl::excel_sheets(p)),~{
  #
  readx(p,.x)
})

graindd2 %>% plot_fun1()

graindd2

summary(graindd[, c("spike", "flower",'var')])
avg<- average
library(ggplot2)
graindd %>%
ggplot(aes(x = flower, y = spike)) +
  geom_point()

x  <- rbind(graindd,graindd2) 
View(x)
x %>%
  ggplot(aes(x = flower, y = spike)) +
  geom_line()+
  facet_grid(~plot_id)

