vec <- c(1,3,5)
vec[-2]

vec[4]
vec %>% .[length(.)+1]
vec[1:4]
vec[4:1]
empty_vec[1]
empty_vec[0]
vec[0]

#####
a<-(2:3)
vec[1:4]
####
# find specific element or position
vec[c(F,T,F)]
vec[vec==5]
# when codition not match at all, it will return? 
vec[vec==2]
vec[c(F,F,F)]
vec %>% .[c(F)]
vec[vec=="a"]

######
vec <- c(1, 2, 3, 4, 5)
logical_vec <- c(TRUE, FALSE)
subset_vec <- vec[logical_vec]
subset_vec
vec[TRUE]

###
long_list_example[[2]][2]
