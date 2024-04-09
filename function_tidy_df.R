
########### R function to convert wide data with n header rows to tidy ###############
# written by @joachimgoedhart; First version on september 2020
# The input is a wide dataframe with n rows that define experimental parameters - column names are ignored and replaced
# All other rows of the input contain data (from one type of measuerement)
# The output is a tidy dataframe, with n columns for the parameters and one column with measured data

tidy_df = function(df, n=1, labels=NULL) {

require(magrittr)
require(tidyr)
require(dplyr)
  
df_wide <- as.data.frame(df)

number_of_parameters <- n

# parameters <- c('Treatment', 'Concentration', 'Replicate')

# Generate parameter names, these will be used as default column names
parameters <-  paste0("parameter_", 1:number_of_parameters)

# Read the first row, convert into a vector
combined_labels<- df_wide[1,] %>% mutate_all(as.character) %>% unlist(use.names=FALSE)

# Repeat the reading of subsequent rows to extract experimental parameters and concatenate with underscore
if (number_of_parameters >1) {
    for (i in 1:(number_of_parameters-1)) {
      param_next <- df_wide[(i+1),] %>% mutate_all(as.character) %>% unlist(use.names=FALSE)
      combined_labels <- paste(combined_labels, param_next, sep="_")
    }
}

# Verify whether each combination of labels is unique (if not, return warning)
if (length(unique(combined_labels))!=length(combined_labels)) {
  return(df <- data.frame(x=c('Duplicate labels found in column numes, try increasing the number of rows')))
  }

#Remove first n rows
df_wide <- df_wide[-c(1:number_of_parameters),]



#Set new column names (still concatenated by underscore if n>1)
colnames(df_wide) <- combined_labels

#Convert to tidy format
df_tidy <- gather(df_wide, Condition, Measurement)

#Split column with condition into multiple columns. Splits defined by underscore
df_tidy <- df_tidy %>% separate(Condition, parameters, sep = '_')

# Change column with measurements to nummeric
df_tidy$Measurement <- as.numeric(df_tidy$Measurement)

# If labels are supplied, use these to change the column names
if (length(labels) != 0) {
  names(df_tidy)[1:length(labels)] <- labels
}

return(df_tidy)

}

# df_wide <- read.csv("Data-with-replicates.csv", header = FALSE, stringsAsFactors = FALSE)
# tidy_df(df_wide, 2, c('treatment', 'replicate'))

# library(readxl)
# df_wide <- as.data.frame(read_excel("Multi-Grouped-data.xlsx", col_names = FALSE))
# tidy_df(df_wide, 3, labels =  c('Treatment','Concentration', 'Replicate'))

