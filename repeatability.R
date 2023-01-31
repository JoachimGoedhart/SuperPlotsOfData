## Function to calculate measures of repeatability
## Code for testing the function at the bottom

library(tidyverse)

# Function to calculate the 'sum of squares'
sos <- function(x) {
  sum((mean(x)-x)^2)
}

# Function that takes a dataframe with at least two columns.
# One column which has (measurement) values and another column that indicates the repeats
# Note, these repeats or the 'observational units' or replicates (Not the subject or 'experimental unit')
# Optional: a third column when multiple conditions/groups are present.
# The result is a dataframe with the repeatability coefficient (RC) and IntraClass Correlation (ICC)
# When groups are defined, the result will be shown for each group

repeatability <- function(df, values, replicates, groups) {
  
  #This is necessary to deal with arguments in tidyverse functions
  replicate <- enquo(replicates)
  group <- enquo(groups)
  value <- enquo(values)
  
  if (rlang::quo_is_missing(replicate)) {
    print("Warning: Replicates not identifed")
  }
  
  #Add IDs for the subjects
  df_id <- df %>% group_by(!! group, !! replicate) %>% mutate(Subject=row_number()) %>% ungroup()
  
  #Calculate n and k
  # n is the number of Experimental Units
  # k is the number of repeated measurements, or Observational Units
  df_id <- df_id %>% group_by(!! replicate, !! group) %>% mutate(n=n()) %>% ungroup()
  df_id <- df_id %>% group_by(Subject, !! group) %>% mutate(k=n()) %>% ungroup()
  
  #Calculate 'total sum of squares' for each condition TSS
  df_id <- df_id %>% group_by(!! group) %>% mutate(TSS = sos(!! value)) %>% ungroup()
  
  #Simplify the dataframe, will depend on presence of 'groups' argument
  if (rlang::quo_is_missing(group)) {
    print("Single group")
    df_result <- df_id %>% distinct(n, k, .keep_all = TRUE)
  } else {
    print("Multiple groups")
    df_result <- df_id %>% distinct(!! group, .keep_all = TRUE)
  }
  

  #Calculate 'within groups sum of squares' SSw for each condition
  df_result$SSw <- df_id %>%
    group_by(!! group, Subject) %>%
    summarize(sos = sos(!! value), .groups = 'drop') %>%
    group_by(!! group) %>% summarize(SSw=sum(sos)) %>% pull(SSw)
  
  #Simplify the dataframe by removing irrelevant columns
  df_result <- df_result %>% select(-c(Subject, quo_name(value)))

  #Calculate the ICC and RC
  df_result <- df_result %>%
    mutate(SSb = TSS-SSw) %>%
    mutate(MSw = SSw / (n*(k-1))) %>%
    mutate(MSb = SSb / (n-1)) %>%
    mutate(ICC =  (MSb - MSw) / (MSb + ((k-1)*MSw))) %>%
    # mutate(VR =  (MSw * k) / (MSb + ((k-1)*MSw))) %>%
    mutate(`total SD` =  sqrt(MSb + ((k-1)*MSw)/k)) %>%
    #### Add Effective Sample Size = n*k/(1+(k-1)*ICC)
    #### DOI: 10.1093/cvr/cvx151
    mutate(`Effective N` = (n*k)/(1+(k-1)*ICC)) %>%
    mutate(RC = 1.96*sqrt(2) * sqrt(MSw)) %>%
    mutate(`RC (95%CI_lo)` = 1.96*sqrt(2)*sqrt(SSw/qchisq(0.975, (n*(k-1)))),
           `RC (95%CI_hi)` = 1.96*sqrt(2)*sqrt(SSw/qchisq(0.025, (n*(k-1))))
           )
  
  return(df_result)
}


##########################################
## For testing, load this dataframe:
## df <- read.csv("Table_5_physiotherapy_tidy.csv")
## Run test: df %>% repeatability(values=Value, replicates=Measurement)

## For testing with multiple groups/conditions, load this dataframe:
## df <- read.csv("SystBloodPressure_tidy.csv")
## Run the function like this:
## repeatability(df, values=BP, replicates=Replicate , groups=Method)
## or:
## df %>% repeatability(values=BP, replicates=n , groups=Condition)

## synthetic <- read.csv("synthetic.csv")
## synthetic %>% repeatability(values=Values, replicates=expUnit , groups=Condition)



