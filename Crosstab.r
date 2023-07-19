setwd('C:/Users/DH/Documents/Projects/Apps/Crosstab')

package.list <- c("readxl","tidyverse")
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library('readxl')
library('tidyverse')

data <- read_xlsx('UnitC1.xlsx',sheet='data')
segments <- read_xlsx('UnitC1.xlsx',sheet='Segment membership')
merged_table <- merge(data,segments,by='participant_id')

#View(merged_table)
#dim(data)
#dim(segments)
#dim(merged_table)

segments = unique(merged_table$name)

remove_special_characters <- function(text) {
  gsub("[^[:alnum:][:space:]]", "", text)
}

# Apply the function to the segments vector
cleaned_segments <- sapply(segments, remove_special_characters)

for (segment in cleaned_segments) {

   crostabb_seg <- merged_table%>%filter(name==segment)
   write.csv(crostabb_seg,paste0('Crosstab ',segment,'.csv'))
}
