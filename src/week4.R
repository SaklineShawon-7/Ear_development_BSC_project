
df <- expand.grid(x=letters[1:4],
                  y=1:2)
##combine column##
df%>% mutate(paste(x,y))
df%>% mutate(z=paste(x,y))
df%>% mutate(z=paste(x,y,sep = "-"))
df %>% tidyr::unite(data = .,col = "z",c(x,y))
df <- df %>% mutate(z=interaction(x,y))

###add column###

# add identifier based on row numbers
df %>% mutate(id=1:n())
df %>% mutate(id=1:nrow(.))
# row names
rownames(df)
df %>% filter()
rownames(df) <- LETTERS[1:nrow(df)]
rownames(df)

####
df <- data.frame(x = c("a", "c"), y = c(1, 2))

# Subset the rows
df[(df$x == "a" & df$y == 1) | (df$x == "c" & df$y == 2), ]

##filter
# Subset the rows
df %>% filter((x == "a" & y == 1) | (x == "c" & y == 2))

##

