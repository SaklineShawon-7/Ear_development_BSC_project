climate %>% 
  select(ends_with("Temperature")) %>% 
  head(4,) %>%
  glimpse()


climate %>% 
  mutate(across(where(is.numeric),~{round(.x, digits = 2)})) %>%
  # mutate(across(where(is.numeric),function(x){round(x, digits = 2)})) %>%
  select(ends_with("Temperature")) %>% 
  head(.,3) %>% 
  glimpse()

# reduce your code chunk by using function
display <- function(x){
  # subset dataframe and summarized for displaying purporse
  # x: input data frame
  x %>% 
    dplyr::select(ends_with("Temperature")) %>% 
    head(.,3) %>% 
    dplyr::glimpse()
}

climate %>% 
  mutate(across(where(is.numeric),function(x){round(x, digits = 2)})) %>%
  display()



by_cyl <- mtcars %>% group_by(cyl)


# grouping doesn't change how the data looks (apart from listing
# how it's grouped):
by_cyl

df <- data.frame(A = c(1, 2, 3),
                 B = c(4, 5, 6),
                 C = c(7, 8, 9))

# Compute the mean of columns A and B
df_mean <- df %>% mutate(across(c(A, B), mean))
df_mean

# It changes how it acts with the other dplyr verbs:
by_cyl %>% summarise(
  disp = mean(disp),
  hp = mean(hp)
)
by_cyl %>% filter(disp == max(disp))

by_cyl %>% summarise(across(displ,hp), mean)

# Each call to summarise() removes a layer of grouping
by_vs_am <- mtcars %>% group_by(vs, am)
by_vs <- by_vs_am %>% summarise(n = n())
by_vs
by_vs %>% summarise(n = sum(n))

# To removing grouping, use ungroup
by_vs %>%
  ungroup() %>%
  summarise(n = sum(n))

# By default, group_by() overrides existing grouping
by_cyl %>%
  group_by(vs, am) %>%
  group_vars()



# Use add = TRUE to instead append
by_cyl %>%
  group_by(vs, am, .add = TRUE) %>%
  group_vars()

# You can group by expressions: this is a short-hand
# for a mutate() followed by a group_by()
mtcars %>%
  group_by(vsam = vs + am)

# The implicit mutate() step is always performed on the
# ungrouped data. Here we get 3 groups:
mtcars %>%
  group_by(vs) %>%
  group_by(hp_cut = cut(hp, 3))

# If you want it to be performed by groups,
# you have to use an explicit mutate() call.
# Here we get 3 groups per value of vs
mtcars %>%
  group_by(vs) %>%
  mutate(hp_cut = cut(hp, 3)) %>%
  group_by(hp_cut)

# when factors are involved and .drop = FALSE, groups can be empty
tbl <- tibble(
  x = 1:10,
  y = factor(rep(c("a", "c"), each  = 5), levels = c("a", "b", "c"))
)
tbl %>%
  group_by(y, .drop = FALSE) %>%
  group_rows()



#day 6 practice####

climate <- read_csv("data/climate.csv")

climate %>%
  group_by(y, .drop = FALSE) %>%
  group_rows()