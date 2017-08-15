library(caret)
library(dplyr)
library(partykit)

###Need to import risk_fix.csv as "risk" this has the numeric fixes for dollars and stationing
library(readr)
risk <- read_csv("~/RunCom/risk_fix.csv", 
                 col_types = cols(X1 = col_skip()))
#subset risk for just Pof fields
risksub <- risk[,9:18]
ctr_all <- ctree(PoF~., risksub, 
                 control = ctree_control(maxdepth = 4,minbucket = 1000))

#plot results
plot(ctr_all,inner_panel = node_inner(ctr_all, id=F, pval = F) )