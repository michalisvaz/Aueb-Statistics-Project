# Read the grade statistics
grades <- read.csv("grade_stats.csv")
grades$year_grads <-grades$X5to599 + grades$X6to699 + grades$X7to799 + grades$X8to899 + grades$X9to10

# Initialize a list with AUEB's departments
departments <- c("DEOS", "OIK", "DET", "M&E", "ODE", "LOXRI", "CS", "STAT")

# Create a list of functions for the CDFs
cdf_functions <- list()

# These will be used to store percentiles of the grade distribution
# and mean grades of each department and of AUEB as a total.
percentiles <- matrix(0, nrow = 9, ncol = 1001)
mean_grades <- c(1:9)

# Iterate over departments and create the CDF and inverse CDF functions
for (i in 1:8) {
  dept <- departments[i]
  temp_grades <- grades[grades$department==dept, ]
  temp_grades_5to599 <- sum(temp_grades$X5to599)
  temp_grades_6to699 <- sum(temp_grades$X6to699)
  temp_grades_7to799 <- sum(temp_grades$X7to799)
  temp_grades_8to899 <- sum(temp_grades$X8to899)
  temp_grades_9to10 <- sum(temp_grades$X9to10)
  temp_totals <- c(temp_grades_5to599, temp_grades_6to699, temp_grades_7to799,
                   temp_grades_8to899, temp_grades_9to10)
  temp_graduates <- sum(temp_totals)
  xs <- c(4, 5, 5.99, 6.99, 7.99, 8.99, 10, 11)
  ys <- c(0, 0)
  sum_grades <- 0
  for (j in temp_totals) {
    sum_grades <- sum_grades + j
    ys <- c(ys, sum_grades/temp_graduates)
  }
  ys <- c(ys, 1)
  cdf_functions[[i]] <- splinefun(xs, ys, method = 'hyman')
  # if you want to use linear interpolation uncomment the next line
  # cdf_functions[[i]] <- approxfun(xs, ys, method = 'linear')
  mean_grades[i] <- weighted.mean(temp_grades$avg_grade, 
                                  temp_grades$year_grads)
  inverse_func <- function(y){
    if(y > 1 | y < 0){
      return(NULL)
    }else{
      return(uniroot((function (x) cdf_functions[[i]](x) - y),
                     lower = 5, upper = 10)$root)
    }
  }
  for (j in 1:1001) {
    percentiles[i, j] = inverse_func((j-1)/1000)
  }
}

# Repeat the above for the whole AUEB
totals <- c(sum(grades$X5to599), sum(grades$X6to699), sum(grades$X7to799), 
            sum(grades$X8to899), sum(grades$X9to10))
graduates <- sum(totals)
xs <- c(4, 5, 5.99, 6.99, 7.99, 8.99, 10, 11)
ys <- c(0, 0)
sum_grades <- 0
for (j in totals) {
  sum_grades <- sum_grades + j
  ys <- c(ys, sum_grades/graduates)
}
ys <- c(ys, 1)
cdf_functions[[9]] <- splinefun(xs, ys, method = 'hyman')
# if you want to use linear interpolation uncomment the next line
# cdf_functions[[9]] <- approxfun(xs, ys, method = 'linear')
mean_grades[9] <- weighted.mean(grades$avg_grade, grades$year_grads)
inverse_func <- function(y){
  if(y > 1 | y < 0){
    return(NULL)
  }else{
    return(uniroot((function (x) cdf_functions[[9]](x) - y), 
                   lower = 5, upper = 10)$root)
  }
}
for (j in 1:1001) {
  percentiles[9, j] = inverse_func((j-1)/1000)
}
departments <- c(departments, "AUEB")

# Create a function for user to experiment with grades
cdf_of_grades <- function(x, dept){
  for (i in 1:9) {
    if(departments[i]==dept){
      return(cdf_functions[[i]](x))
    }
  }
  return(NULL)
}

# And a function for the user to experiment with percentiles
inverse_cdf_of_grades <- function(y, dept){
  d_ind <- 0
  for (i in 1:9) {
    if(departments[i]==dept){
      d_ind <- i
      break
    }
  }
  if(d_ind==0){
    return(NULL)
  }
  if(y > 1 | y < 0){
    return(NULL)
  }else{
    return(uniroot((function (x) cdf_functions[[d_ind]](x) - y), 
                   lower = 5, upper = 10)$root)
  }
}

# Output the mean grade per department
temp_df <- data.frame(Department = departments, Average_Grade = mean_grades)
temp_df

boxplot(percentiles[1, 2:1000], percentiles[2, 2:1000], percentiles[3, 2:1000], 
        percentiles[4, 2:1000], percentiles[5, 2:1000], percentiles[6, 2:1000], 
        percentiles[7, 2:1000], percentiles[8, 2:1000], percentiles[9, 2:1000], 
        col = c("chartreuse4", "chartreuse4", "darkorange",  "darkorange", 
              "darkorange", "darkorange", "deepskyblue3", "deepskyblue3", 
              "darkred"), main = "AUEB grades per department", 
        ylab = "Final grade", xlab = "Department", 
        names = c("DEOS", "OIK", "DET", "M&E", "ODE", "LOXRI", 
                  "CS", "STAT", "AUEB"), cex.axis = 0.8)

# create a vector with AUEB's final grade per year
years <- c(2016, 2017, 2018, 2019, 2020)
aueb_avgs <- c()
for(i in years){
  temp_grades <- grades[grades$year==i,]
  temp_avg <- weighted.mean(temp_grades$avg_grade, temp_grades$year_grads)
  aueb_avgs <- c(aueb_avgs, temp_avg)
}

# And now a dataframe with the mean per year for all departments and for AUEB
# Each department and AUEB will be a column in this new dataframe
# Comment the lines corresponding to the department you want to ommit
# Be careful when you comment the final line
year_evol <- data.frame(time = years, 
                        CS = grades[grades$department=="CS",]$avg_grade, 
#                        DEOS = grades[grades$department=="DEOS",]$avg_grade, 
                        DET = grades[grades$department=="DET",]$avg_grade, 
                        LOXRI = grades[grades$department=="LOXRI",]$avg_grade,
#                        ME = grades[grades$department=="M&E",]$avg_grade,
                        ODE = grades[grades$department=="ODE",]$avg_grade,
#                        OIK = grades[grades$department=="OIK",]$avg_grade,
#                        STAT = grades[grades$department=="STAT",]$avg_grade,
                        AUEB = aueb_avgs)
# And now plot the results
require(ggplot2)
require(reshape2)
year_evol <- melt(year_evol ,  id.vars = 'time', variable.name = 'departments')
p <- ggplot(year_evol, aes(time,value)) + geom_line(aes(colour = departments), size=1.2)
p + labs(title = "Evolution of average grades", 
       x = "Year of graduation", 
       y = "Average grade")

# After running all the above script you can experiment 
# with commands like the following:
cdf_of_grades(8, "CS")
# to see what percentile you'd be with a grade of x in a certain department
# or like the following:
inverse_cdf_of_grades(0.6, "CS")
# to see what grade one would need to be above 
# the y percent of a department's graduates

