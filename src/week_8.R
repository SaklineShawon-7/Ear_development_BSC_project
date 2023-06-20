path <- "./data/student/"
filenames <- list.files(path,pattern=".xlsx")
#create empty list
df <- vector(mode="list",length=length(filenames))

for(i in 1:length(filenames)){
  #i<-2
  fullpath <- paste0(path,filenames[i])
  df[[i]] <- read_xlsx(fullpath) %>% 
    names()
}

df 

strsplit(filenames)
strs

"grain_counting_practice_clement.xlsx" 

('grain_counting_practice')
library('purrr')
df<- map_dfr(list.files("./data/student"),~{
  #.x <- list.files("./data/student") [1]
  file<- read_xlsx(paste0("./data/student/",.x))
 file
  
})
df %>% 
  glimpse()

ggplot(data=df, aes(x=flower, y= spike,color=student))+
  geom_path()+
  geom_point()
  
