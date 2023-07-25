setwd('C:/Users/DH/Documents/Projects/Apps/Crosstab')

package.list <- c("readxl","tidyverse","tidyr")
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library('readxl')
library('tidyverse')
library('tidyr')

data <- read_xlsx('UnitC1.xlsx',sheet='data')
segments <- read_xlsx('UnitC1.xlsx',sheet='Segment membership')

crosstab <- function(data,segments,metric){

  merged_table <- merge(data,segments,by='participant_id')

  merged_table <- select(merged_table,-c(status_included_in_current_report,
  segment_id))

  data_stacked <- pivot_longer(merged_table, cols = -c(name,participant_id), names_to = "variable", values_to = "value")

  if(metric=="median")
  {
    final_data <- data_stacked%>%group_by(name,variable)%>%summarize(value = median(value))

  } else {final_data <- data_stacked%>%group_by(name,variable)%>%summarize(value = mean(value))}
  
  final_data <- pivot_wider(final_data, id_cols = variable, names_from = name, values_from = value)

  if(metric=="median")
  {
    overall <- data_stacked%>%group_by(variable)%>%summarize(overall=median(value))

  } else {overall <- data_stacked%>%group_by(variable)%>%summarize(overall=mean(value))}

  final_data <- cbind(final_data,overall[,2])
  return(final_data)

}

data_test <- crosstab(data = data,segments = segments,metric = "median")
View(data_test)

write.csv(data_test,'data_test.csv')
