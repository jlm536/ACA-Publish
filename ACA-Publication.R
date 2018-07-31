####################Reading in packages and functiosn########################
source("/Users/eringutbrod/Projects/Resources/Code/R/FunctionList.R")
library(foreign)
library(readstata13)

####################Reading in data##########################################
############Reading in State Response Data
response=read.csv(file="Data/MedicaidResponseData.csv")
head(response)

response.clean=response%>%
  dplyr::select(state.postal,state.fips,year,state.year,expansion,expansion.delay,expansion.early,research.funds,nfib.support,king.support,authorize,waiver.expansion,nfib.challenge,king.challenge,grant.returns,Active.Waiver,mandate.challenge,leg.challenge,compact,leg.lit,fund.return,repeal)

names(response.clean)

########adding in grants
grants=read.csv("data/grantdata.csv")
grants=grants%>%
  dplyr::filter(date>2010)%>%
  dplyr::select(state.year,planning,level.1,level.2,innovator,grant.total,grant.weighted)
response.working=full_join(response.clean,grants)


#########Positive##########
#Expansion - 1 = 20, 0 = 0 #weighted.grants - stays same, #nfib.support - +5 #king.support - +5, #+1 per authorize, stays same

####Negative#######
#nfib.challenge, #king.challenge - -5, #grant.returns, -grant totals*2
#delay, -3 per year
#-2 for delay legislation (funding.cuts, leg.challenge, compacts, fund.return)
#-5 for rejection legislation (mandate.challenge, leg.lit, repeal)
#waiver.expansion, -10
names(response.clean)
unique(response.clean$weighted.grants)
working=response.clean%>%
  mutate(response.weighted.non=(nfib.challenge*-5)+
           (king.challenge*-5)+
           (nfib.support*5)+
           (king.support*5)+
           (weighted.grants))
################Attaching Shor Data
shor.data=read.dta13("/Users/eringutbrod/Projects/Resources/Datahub/Shor_data/2018 Update/shor mccarty 1993-2016 state aggregate data May 2018 release (Updated July 2018).dta")
head(shor.data)
