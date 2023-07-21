setwd('C:/Users/DH/Documents/Projects/Apps/Crosstab')

package.list <- c("readxl","tidyverse","tidyr")
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library('readxl')
library('tidyverse')
library('tidyr')

data <- read_xlsx('UnitC1.xlsx',sheet='data')
segments <- read_xlsx('UnitC1.xlsx',sheet='Segment membership')

crosstab <- function(data,segments){

  merged_table <- merge(data,segments,by='participant_id')

  merged_table <- select(merged_table,-c(status_included_in_current_report,
  segment_id))

  data_stacked <- pivot_longer(merged_table, cols = -c(name,participant_id), names_to = "variable", values_to = "value")

  final_data <- data_stacked%>%group_by(name,variable)%>%summarize(median(value))
  final_data <- t(final_data)

  overall <- data_stacked%>%group_by(variable)%>%summarize(median(value))
  overall <- rbind("Total",t(overall)) 

  final_data <<- cbind(final_data,overall)
  return(final_data)

}

data_test <- crosstab(data = data,segments = segments)
View(data_test)

write.csv(data_test,'data_test.csv')