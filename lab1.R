name <- "Rabnawaz Jansher"
liuid <- "rabsh696"
library(markmyassignment)
# Set assignment Path 
lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)


#1.1.1
my_num_vector <- function()
{
  v_result <- c(log10(11),cos(pi/5), exp(pi/3),(1173 %% 7)/19)
  v_result
}
my_num_vector()
mark_my_assignment(tasks="my_num_vector")

#1.1.2
filter_my_vector<- function(x, leq)
{
  x[x >=leq ] <- NA
  x
}
filter_my_vector(x=c(2,9,2,4,102), 4)
mark_my_assignment(tasks="filter_my_vector")

#1.1.3
dot_prod<- function(a, b)
{
  product_of_vectors <- a*b
  scaler_result <- sum(product_of_vectors)
  scaler_result
}

dot_prod(a=c(3,1,12,2,4), b=c(1,2,3,4,5))
dot_prod(a = c(-1,3), b = c(-3,-1))
mark_my_assignment(tasks="dot_prod")

#1.1.4
approx_e <- function(N)
{
  scaler_result<- sum(1/factorial(0:N) )  
  scaler_result
}
approx_e(2)
approx_e(4)
mark_my_assignment(tasks="approx_e")

#1.2.1
my_magic_matrix <- function()
{
  magic_matrix<-matrix(c(4,3,8,9,5,1,2,7,6),nrow = 3)
  magic_matrix
}

my_magic_matrix()
mark_my_assignment(tasks="my_magic_matrix")

#1.2.2
calculate_elements<- function(A)
{
  scaler_result<- length(A)
  scaler_result
}

mat <- my_magic_matrix()

calculate_elements(mat)
new_mat <- cbind(mat, mat)
calculate_elements(new_mat)
mark_my_assignment(tasks="calculate_elements")

#1.2.3
row_to_zero <- function(A, i)
{
  A[i,1:ncol(A)] <- 0
  A
}
mat <- my_magic_matrix()
print(row_to_zero(mat,3))
mark_my_assignment(tasks="row_to_zero")

#1.2.4
add_elements_to_matrix <- function(A, x,i,j)
{
  A[i,j] <- A[i,j] + x
  A
}
mat <- my_magic_matrix()
print(add_elements_to_matrix(mat,-2,1:3,2:3))
mark_my_assignment(tasks="add_elements_to_matrix")


#1.3.1
my_magic_list <- function()
{
  megic_list<-list( info="my own list", my_num_vector()  , my_magic_matrix() )
  megic_list
}

my_magic_list()
mark_my_assignment(tasks="my_magic_list")

#1.3.2
change_info <- function(x, text)
{
  x$info <- text
  x
}
a_list <- my_magic_list()
change_info(a_list,"Some new info")
mark_my_assignment(tasks="change_info")

#1.3.3
sum_numeric_parts <- function(x)
{
  scaler_result <- sum(as.numeric(unlist(x)),na.rm = NA)
  scaler_result
}

a_list <- my_magic_list()
sum_numeric_parts(x = a_list)
sum_numeric_parts(a_list[2])
mark_my_assignment(tasks="sum_numeric_parts")

#1.4.1
my_data.frame<- function()
{
  my_data<-data.frame("id" = c(1,2,3), "name" = c("John","Lisa","Azra"), "income" = c(7.30,0.00,15.21) , "rich"= c(FALSE,FALSE,TRUE) )
}

my_data.frame()
mark_my_assignment(tasks="my_data.frame")

#1.4.2
sort_head<- function(df, var.name, n)
{
  data_frame_sort <- df[order(df[var.name],decreasing = TRUE),]
  data_frame_result<- data_frame_sort[1:n,]
  data_frame_result
}

data(iris)
print(sort_head(iris, "Petal.Length" , n=5))
mark_my_assignment(tasks="sort_head")

#1.4.3
add_median_variable<-function(df, j)
{
  df["compared_to_median"]<-df[,j] - median(df[,j])
  df["compared_to_median"]<- replace(df["compared_to_median"], df["compared_to_median"]>0, "Greater")
  df["compared_to_median"]<- replace(df["compared_to_median"], df["compared_to_median"]<0, "Smaller")
  df["compared_to_median"]<- replace(df["compared_to_median"], df["compared_to_median"]==0, "Median")
  data_frame_result<- df
  data_frame_result
}

data(faithful)
head(add_median_variable(faithful, 1))
tail(add_median_variable(faithful, 2))
mark_my_assignment(tasks="add_median_variable")

#1.4.4
analyze_columns<- function(df, j)
{
  index_x <- j[1]; index_y <- j[2]
  colname_corelation_matrix <- c(names(df[index_x]), names(df[index_y]),"correlation_matrix")
  v_colname <- c("mean","median", "sd")
  col1_unlist<- as.numeric(unlist(df[index_x]))
  col2_unlist<- as.numeric(unlist(df[index_y]))
  resultant_list<-list("mean"<-c(mean(col1_unlist),median(col1_unlist),sd(col1_unlist)),"median"<-c(mean(col2_unlist),median(col2_unlist),sd(col2_unlist)),
                       "corelation"<-cor(subset(df,select=j)))
  
  names(resultant_list[[1]]) <- names(resultant_list[[2]])<- v_colname
  names(resultant_list)[1:length(colname_corelation_matrix)] <- colname_corelation_matrix
  resultant_list
}

data(faithful)
analyze_columns(faithful, c(1:2))
mark_my_assignment(tasks="analyze_columns")