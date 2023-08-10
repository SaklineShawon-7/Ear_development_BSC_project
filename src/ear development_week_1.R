#variable####
# assignment str"v" to name "variable"
## "" and unquote str and variable 

variable <- "v"
Variable <- 1
variable +1
Variable +1 

# str??
str
?str
str(variable)
str(Variable)
# data type coersion
str(NA)
str(c(NA,1,"c"))
str(c(NA,"a"))
str(c(NA,TRUE))
str(c(1,"a"))

#install.packages(tidyrverse)
install.packages("tidyrverse")
library(dplyr)

# syntax of using pipe
morning_schedule<- wakeup(self) %>% 
  put_on('clothes') %>% eat_1('breakfast')

fg<-c(1,2,3,4,5)
fg %>% str()
g

fun1(object)

object %>% fun1(.)
object %>% fun1()
object %>% fun1() %>% .
object %>% fun1(.) %>% .

#embedded functions
fun2(fun1(object))
# pipe
object %>% 
  fun1() %>% 
  fun2()

# how many ways of creating a sequence?
df1<-c('a','b','c')
seq(1,3,1)
df1 %>% str() %>% select('b')
# embedded function : fun2(fun1())
length(c(1,2,3))
# use pipe, "." is the result of previous step
c(1,2,3) %>% length()

# replicate element as vector
rep(2,3)
# remove duplicates
rep(1,3) %>% unique()
# cumulative sum 
rep(1,3) %>% cumsum()

# is there any difference?
paste(c("a","1"),collapse = "")
paste0(c("a","1"))
paste0("a","1")

#Use str() to check the data type of above line.
str(df1)

#You have two vectors, c("a","b") and c("1","2")

paste(rep(c("a","b"), each=2), rep(c("1","2"), times=2))

paste(rep(c("a","b"), times=2), rep(c("1","2"), each=2))

#function####
#format: function_name(argument1, argument2) {code}
Variable <- 1
variable +1
plusone <- function(x){
  x+1
}
plusone

v1<-c(1,7,8)
m1<-function(v1){
  avg<-mean(v1)
  std<- sd(v1)
  t2<-str(c(avg,std))
  return(t2)
}
m1(v1)

avg<-mean(v1)
std<- sd(v1)
# is function data type sensitive?
plusone(variable)
plusone(Variable) 


#Date####
as.Date("2023-04-17")
as.Date("2023-04-17",format="%Y-%m-%d")
# is ther any error?
as.Date("20230417")
as.Date("17042023")
# additive properties of Date 
as.Date("2023-04-17")-7
as.Date("2023-04-17")+2

#Since type Date is additive, how to create successive date vector of length 5? 
  #Vector date start with “2023-04-17”

start_date<- as.Date("2023-04-17")
end_date<-as.Date("2028-04-17")

date_vec<- seq (as.Date("2023-04-17"),as.Date("2028-04-17"), by="years")

date_vec1<- seq (as.Date("2023-04-17"), by="1 month",length.out = 5)

date_vec1


# check if pattern exist in vector
3%in%c(1,3) 
2%in%c(1,3) 

1==2 
!1==2 #! operator: The ! operator in R is the logical negation operator.
#It reverses the logical value of its operand. 
#For example, !TRUE evaluates to FALSE, and !FALSE evaluates to TRUE.
1!=2 # '!=' means not equal to
c(1,3)==2

which(c(1,3)==3) 

# what will be the difference?
order(c(3,1,2)) 
c(3,1,2) %>% .[order(.)]

# what will be the data type? check with str()
c(1,2,NA) %>% is.na() 
c(1,2,NA) %>% is.na() %>% which() 
which(is.na(c(1,2,NA)))
c(1,2,NA) %>% is.na() %>% !.
c(1,2,NA) %>% !is.na() 
!is.na(c(1,2,NA))

# check if data type match
arg <- 1
is.character(arg)
if(is.character(arg)){
  print("character")
}

if(is.character(arg)){
  print("character")
}else{("type other than character")
}

if(is.character(arg)){
  warning("wrong")
}

if(is.character(arg)){
  stop("wrong")
}
