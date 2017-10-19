library(dplyr)
library(nycflights13)

flight_data <- flights(3:5)

# dplyr filter 
r<- dplyr::filter(flight_data, month == 1 & day ==1 )
dim(r)
r<- flight_data[flight_data$month == 1 & flight_data$day == 1, ]
dim(r)

#extract a number of rows from data in table 
slice(flight_data, 1:2)

#order the data work as similar to filter but assceding and deceding order
r<- arrange(flight_data, sched_dep_time)

#select a specific column 
r<- select(flight_data, year, carrier)
r

#select all column between year and day 
select(flight_data, sched_dep_time:carrier)

#select all column except 
select(flight_data, -(sched_dep_time:carrier))

#disticnt unique data find
distinct(select(flight_data, dest))

#add a new column to table
select(mutate(flight_data, gain=0), gain)




arran <- select(slice(arrange(flight_data,arr_delay), 1:40), arr_delay)

rename(flight_data, year = year)

k<- transform(flight_data, gain = 0)
k$gain


plyr::summarise(flight_data, sd(arr_delay, na.rm = TRUE))
dplyr::summarise(dplyr::group_by(flights, dest), M = sd(arr_delay, na.rm = TRUE))


sample.n<-sample_n(flight_data,10)
sample.fr<- sample_frac(flight_data,0.0001)

one_col <- select(flight_data, dest)
one_col

by_tailnum <- group_by(one_col, dest )
delay <- summarise(by_tailnum, count = n())
delay
by_tailnum


a<- data.frame(a=c(1:10), b=seq(1,10,1))

dplyr::tbl_df(a)


dplyr::glimpse(a)
tidyr::gather(a,"a",convert = TRUE, value="ccc")
dplyr::cumall(a$a)
dplyr::bind_rows(a,a[1])


fruit <- c("apple", "banana", "pear", "pineapple")
fruit
s