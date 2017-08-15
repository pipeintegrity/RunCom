library(caret)
library(dplyr)
library(partykit)

###Need to import risk_fix.csv as "risk" this has the numeric fixes for dollars and stationing

#subset risk for just Pof fields
risksub <- risk[,9:18]
