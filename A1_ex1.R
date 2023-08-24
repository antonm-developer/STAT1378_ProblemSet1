getwd()

X <- read.csv("A1_Ex1.csv", header = TRUE, sep = ",") ##Read the file A1_Ex1.csv in a data frame or tibble X
str(X)##Shows structure of X(str(X))
total_exp <- sum(X$expenditure)##Sums total expenditure then stores in object
total_exp##prints total expenditure
cat("Total Expenditure:", total_exp, "\n")##Then prints total expenditure in cat command
exp_mar <- sum(X$expenditure[X$month == "Mar"])##Total sum of expenditures in March stored in object then printed
exp_mar
cat("Expenditure in March:", exp_mar, "\n")## Another cat command displaying in console sum of expenditures in March
exp_mar_cloudy <- sum(X$expenditure[X$month == "Mar" & X$weather == "cloudy"])##Sum of expenditures in March when its cloudy stored in object then printed
exp_mar_cloudy
cat("Expenditure in March, with cloudy weather:", exp_mar_cloudy,"\n")##Sum is then put in cat command then printed
highest_mnth <- X %>% ##X is stored in an obje
  group_by(month) %>%
  summarise(total_exp = sum(expenditure)) %>%
  arrange(desc(total_exp)) %>%
  slice(1) %>%
  pull(month)
highest_mnth  
cat("Month with highest expenditure:", highest_mnth,"\n")
