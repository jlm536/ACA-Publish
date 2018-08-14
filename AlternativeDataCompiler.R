####################Reading in packages and functiosn########################
source("/Users/eringutbrod/Projects/Resources/Code/R/FunctionList.R")
library(foreign)
library(readstata13)
library(psych)
library(GPArotation)

response=read.csv(file="Data/MedicaidResponseData.csv")
summary(response)

response.clean=response%>%
  mutate(expansion=if_else(is.na(as.numeric(expansion)),0,as.numeric(expansion)))%>%
  dplyr::select(state.postal,state.fips,year,state.year,expansion,expansion.delay,expansion.early,nfib.support,king.support,authorize,waiver.expansion,nfib.challenge,king.challenge,grant.returns,fund.return,mandate.challenge,leg.challenge,compact,leg.lit,repeal,funding.cuts)

########adding in grants
grants=read.csv("data/grantdata.csv")
head(grants)
grants=grants%>%
  dplyr::filter(date>2010)%>%
  dplyr::select(state.year,planning,level.1,level.2,innovator,grant.total,grant.weighted)

response.working=full_join(response.clean,grants)
response.working=response.working%>%  
  mutate(planning=if_else(is.na(as.numeric(planning)),0,as.numeric(planning)))%>%
  mutate(level.1=if_else(is.na(as.numeric(level.1)),0,as.numeric(level.1)))%>%
  mutate(level.2=if_else(is.na(as.numeric(level.2)),0,as.numeric(level.2)))%>%
  mutate(innovator=if_else(is.na(as.numeric(innovator)),0,as.numeric(innovator)))%>%
  mutate(grant.total=if_else(is.na(as.numeric(grant.total)),0,as.numeric(grant.total)))%>%
  mutate(grant.weighted=if_else(is.na(as.numeric(grant.weighted)),0,as.numeric(grant.weighted)))%>%
  mutate(grant.weighted.alt=(planning+level.1+level.2)+(3*innovator))
head(response.working)

###########Building some new DV's
summary(response.working)
response.dvs=response.working%>%
  mutate(nonexpanse.weighted=grant.weighted.alt+
           (nfib.support*3)+
           (king.support*3)+
           (authorize)+
           (nfib.challenge*-3)+
           (king.challenge*-3)+
           (grant.returns*-3)+
           (funding.cuts*-1)+
           (leg.challenge*-1)+
           (compact*-1)+
           (fund.return*-1)+
           (mandate.challenge*-3)+
           (leg.lit*-3)+
           (repeal*-3))%>%
  mutate(response.weighted=grant.weighted.alt+
           (nfib.support*3)+
           (king.support*3)+
           (authorize)+
           (nfib.challenge*-3)+
           (king.challenge*-3)+
           (grant.returns*-3)+
           (funding.cuts*-1)+
           (leg.challenge*-1)+
           (compact*-1)+
           (fund.return*-1)+
           (mandate.challenge*-3)+
           (leg.lit*-3)+
           (repeal*-3)+
           (waiver.expansion*-5)+
           (expansion*10))%>%
  mutate(nonexpanse.unweighted=grant.total+
           (nfib.support)+
           (king.support)+
           (authorize)+
           (nfib.challenge*-1)+
           (king.challenge*-1)+
           (grant.returns*-3)+
           (funding.cuts*-1)+
           (leg.challenge*-1)+
           (compact*-1)+
           (fund.return*-1)+
           (mandate.challenge*-1)+
           (leg.lit*-1)+
           (repeal*-1))%>%
    mutate(response.unweighted=grant.total+
             (nfib.support)+
             (king.support)+
             (authorize)+
             (nfib.challenge*-1)+
             (king.challenge*-1)+
             (grant.returns*-3)+
             (funding.cuts*-1)+
             (leg.challenge*-1)+
             (compact*-1)+
             (fund.return*-1)+
             (mandate.challenge*-1)+
             (leg.lit*-1)+
             (repeal*-1)+
             (waiver.expansion*-2)+
             (expansion*5))

summary(response.dvs$nonexpanse.weighted)
summary(response.dvs$nonexpanse.weighted[which(response.dvs$expansion==1)])
summary(response.dvs$response.weighted)
hist(response.dvs$response.weighted)
response.dvs[which(response.dvs$expansion==1 & response.dvs$response.weighted < 0),]
response.dvs[which(response.dvs$expansion!=1 & response.dvs$response.weighted >9),]

summary(response.dvs$nonexpanse.unweighted)
summary(response.dvs$nonexpanse.weighted[which(response.dvs$expansion==1)])
summary(response.dvs$response.unweighted)
hist(response.dvs$response.unweighted)
response.dvs[which(response.dvs$expansion==1 & response.dvs$response.unweighted < 0),]

cor.test(response.dvs$response.unweighted,response.dvs$response.weighted)
cor.test(response.dvs$nonexpanse.weighted,response.dvs$nonexpanse.unweighted)
plot(response.dvs$response.unweighted,response.dvs$response.weighted)

####other DV's
dvs=response.dvs%>%
  mutate(litigation=nfib.support-nfib.challenge+king.support-king.challenge)%>%
  mutate(anti.legislation=funding.cuts+leg.challenge+compact+fund.return+mandate.challenge+leg.lit+repeal)%>%
  dplyr::select(state.postal,state.fips,year,state.year,response.unweighted,response.weighted,nonexpanse.unweighted,nonexpanse.weighted,expansion,litigation,anti.legislation,authorize,grant.weighted.alt,grant.returns,nfib.support,nfib.challenge,king.support,king.challenge,grant.returns,mandate.challenge,leg.challenge,compact,leg.lit,fund.return,repeal,funding.cuts,grant.total)

head(dvs)

################Attaching Shor Data
head(dvs)
unique(dvs$year)
shor.data=read.dta13("/Users/eringutbrod/Projects/Resources/Datahub/Shor_data/2018 Update/shor mccarty 1993-2016 state aggregate data May 2018 release (Updated July 2018).dta")
head(shor.data)
shor.useful=shor.data%>%
  filter(year>2010)%>%
  mutate(state.year=paste(st,year,sep=""))%>%
  dplyr::select(state.year,hou_chamber,hou_dem,hou_rep,hou_majority,sen_chamber,sen_dem,sen_rep,sen_majority,h_diffs,s_diffs)
head(shor.useful)
summary(shor.useful)
shor.useful[which(is.na(shor.useful$hou_rep)),]
shor.useful[which(is.na(shor.useful$sen_rep)),]
dim(dvs)
dim(shor.useful)
dv.shor=full_join(dvs,shor.useful)
head(dv.shor)
unique(dv.shor$authorize[which(dv.shor$year>2010)])
#########Attaching other IV's##############
iv=read.csv("data/ACAIVData.csv")
head(iv)
iv=iv%>%
  dplyr::select(state.year,capita.income,fpl,no.insurance,operations.capita,health.donor.capita)
dv.shor.iv=full_join(dv.shor,iv)
head(dv.shor.iv)

#########Attaching competition scores##########
competition=read.csv("Data/competitionvariables.csv")
head(competition)
competition=competition[,-1]
working.data=full_join(dv.shor.iv,competition)
summary(working.data)
write.csv(working.data,"Data/workingdata-alternate.csv")
