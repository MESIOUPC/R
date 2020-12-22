### ===========================================================
### Màster Universitari en Estadística i Investigació Operativa
### Computación en Estadística y en Optimización
### Final Project 2020-2021
### Nuria Saavedra Miró and Ferran Vilar Pagès
### ===========================================================

library(rvest)
library(xml2)
library(Hmisc)
library(openxlsx)
library(Epi)
library(doBy)

#set file's directory as working directory 
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
getwd()

# Exercise 1

# a
# Import the study data (sea_temperature.xls) 
# and place them in a data frame named sea.deep.
# If you can, import the data directly from the following link:
# https://www.idescat.cat/pub/?id=aec&n=218&t=2000

# each year has a different url that is stored in the url vector
years <- 2000:2019
url <- paste("https://www.idescat.cat/pub/?id=aec&n=218&t=", years, sep = "")
tbls <- vector(mode = "list")

# reads the html of each url, after cleansing and parsing the data is stored in
# a data frame.
for (i in 1:length(url)) {
  webpage <- read_html(url[i])
  tbl <- html_nodes(webpage, "table")

  # removing unwanted HTML code to make parsing easier
  xml_remove(xml_find_all(tbl[[1]], "//caption"))
  xml_remove(xml_find_all(tbl[[1]], "//tfoot"))
  xml_remove(xml_find_all(tbl[[1]], "//colgroup"))
  xml_remove(xml_child(xml_child(tbl[[1]], 1), 1))
  xml_remove(xml_find_all(tbl[[1]], "//tr[@class=' total']"))
  xml_remove(xml_find_all(tbl[[1]], "//tr[@class=' primera total']"))
  xml_remove(xml_find_all(tbl[[1]], "//tr[@class=' darrera total']"))
  xml_remove(xml_find_all(tbl[[1]], "//th[@colspan]"))
  
  # parsing from HTML table to R
  tbls[i] <-  html_table(tbl, fill = TRUE, header = TRUE, trim = FALSE) 
  tbls[[i]] <- tbls[[i]][-6:-9]
  tbls[[i]] <- na.exclude(tbls[[i]])
  
  tbls[[i]] <- head(tbls[[i]], 12)
  rownames(tbls[[i]]) <- 1:12
  tbls[[i]] <- tbls[[i]][-1]
  
  # adds date column for each observation
  tbls[[i]] <- cbind(
    date = as.Date(
      paste(years[i], 1:12, 1, sep = "-")
    ),
    tbls[[i]]
  )
}

# consolidates all years in a single data frame
sea.deep <- do.call(rbind, tbls[1:length(url)])
rm(tbl)
rm(tbls)
rm(i)

# converts char columns to numeric columns
for (i in 2:5) {
  sea.deep[, i] <- as.numeric(sub(",", ".", sea.deep[, i]))
}
rm(i)

# Name the variables according to the specifications in the data
# set and place labels describing units and what each variable
# and case is (years, months, depth, etc.). You should place it
# properly according to the format of the data.

# b
# Use the function label of package Hmsic to tag variables
# (using of data frame sea.deep).
label(sea.deep) <- "Catalonia's seawater monthly average temperature at different depths (2000-2019)"
label(sea.deep$date) <- "Observation date (yyyy-mm-dd)"
label(sea.deep$'0') <- "Average temperature at 0m (in Celsius)"
label(sea.deep$'-20') <- "Average temperature at -20m (in Celsius)"
label(sea.deep$'-50') <- "Average temperature at -50m (in Celsius)"
label(sea.deep$'-80') <- "Average temperature at -80m (in Celsius)"

# c
# Indicate the dimension of the data-frame and make a descriptive
# summary of the variables.

# A previous statments: 
View(sea.deep)
colSums(is.na(sea.deep)) # We assure there is no empty spaces in the dataframe, thus, 
                         # the analysis can aboid worrying about it.

# Dimension and descriptive summary:
dim(sea.deep) # 240 observations of 5 variables
summary(sea.deep) 

# Descriptve analysis of the data set.
attributes(sea.deep)
describe(sea.deep)
str(sea.deep)

# to do: agregar mas funciones/graficas de GrB_R8_DescriptiveAnalysis
# To analize and compare results, first we create a new factorized variable form the original dates.
sea.deep$date
date.ch <- format(sea.deep$date, "%Y")
date.fac <- factor(date.ch, unique(date.ch))
sea.deep$datefactor <- date.fac
levels(sea.deep[,6])

# With library Epi
stat.table(list(Year = datefactor), list(N = count(), 
"Mean Tempreature at 0m (in Celcius)" = mean(sea.deep$`0`),
"Mean Tempreature at -20m (in Celcius)" = mean(sea.deep$`-20`),
"Mean Tempreature at -50m (in Celcius)" = mean(sea.deep$`-50`),
"Mean Tempreature at -80m (in Celcius)" = mean(sea.deep$`-80`)),
data = sea.deep, margins = T)

# d
# Represent by means of Boxplots the average temperatures by 
# depth and year.

# finish later
boxplot(sea.deep$'0' ~ format(sea.deep$date, '%Y'),
        main = "Average temperatures by depth and year",
        xlab = "Year", ylab = "Avg. temperature (C)")

# Igual así te parece bien
# Here its represented the average temperature by depth and years. Means of boxplots.
for (i in c(2,3,4,5)){
  boxplot(sea.deep[,i]~sea.deep$datefactor, ylab = 'Temperature', xlab = 'Study years', col = 3:7, pch=16)
  title(label(sea.deep[i]))
}
# e
# Calculate the mean, median, standard deviation, and the
# interquartile range for each of the previous groups (or other
# statistics if necessary).

# repeats the process for each depth
for (i in c(2,3,4,5)) {
  cat(paste("\nstats for ", names(sea.deep[i]), "m depth:", sep = ""))
  cat(paste("\nmean: ", round(mean(sea.deep[, i]), 2)))
  cat(paste("\nmedian: ", median(sea.deep[, i])))
  cat(paste("\nstandard dev: ", round(sd(sea.deep[, i]), 2)))
  cat(paste("\ninterquartile range: ", IQR(sea.deep[, i])))
  cat("\n")
}

# f
# Properly represent the data to be able to see the annual variations
# of the average temperature in the total depths and years.

sea.deep$diff_mean_0 <- with(sea.deep, as.vector(trunc(sea.deep$`0`-mean(sea.deep$`0`))))
sea.deep$dev_mean_minus20 <- with(sea.deep, as.vector(trunc(sea.deep$`-20`-mean(sea.deep$`-20`))))
sea.deep$dev_mean_minus50 <- with(sea.deep, as.vector(trunc(sea.deep$`-50`-mean(sea.deep$`-50`))))
sea.deep$dev_mean_minus80 <- with(sea.deep, as.vector(trunc(sea.deep$`-80`-mean(sea.deep$`-80`))))

# g
# Export data frame with the new variables created to a new file
# sheet, for example in NUEVO.xlsx

write.xlsx(sea.deep, "NUEVO.xlsx") # stored in working directory

# Exercise 2

# Make two or more graphs with the data from Exercise 1 using
# functions from two of the following packages:
# gplots https://cran.r-project.org/web/packages/gplots/index.html
# plotrix https://cran.r-project.org/web/packages/plotrix/index.html
# vcd https://cran.r-project.org/web/packages/vcd/index.html
# For at least one of the graphs you should use the study group
# variable (depth or year) and for the other one a couple of 
# numerical variables. Present the syntax and interpret both
# graphs.

# Exercise 3

# Schedule a function that does the following:

# a
# Calculate the temperature between one month and the following month
# for each year and depth, offer a graph per year and the average of
# all years studied. You should be able to indicate what years or
# depths it should represent.

# b
# Calculate the difference between the temperature of each month and
# the same month of the previous 30 years. Offer a graph for each
# year and the average of all years studied. You should also be able
# to indicate what year or depths it should represent.

# c
# Verify the operation of the function with the data frame sea.deep
# and ensure that everything is correct (optionally you can
# invent a data frame to verify it, with simulated temperatures
# using a normal truncated distribution of temperatures and with
# the same depths).

# d
# Comment on the results found and if you think they have effects on
# climate change. Optionally, you can build a library to project the
# future of seawater temperature at a certain temperature for the
# following years.

# e
# Optionally, look for other data (public repositories) if you think
# it is necessary and cross them with those already available,
# especially if they help to better explain the possible climate
# change that is taking place.

# Optionally, create a library in R to store the previous functions,
# the source data and place it in a public repository such as
# Github (https://github.com/).
