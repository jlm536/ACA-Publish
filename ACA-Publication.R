####################Reading in packages and functiosn########################
source("/Users/eringutbrod/Projects/Resources/Code/R/FunctionList.R")
library(foreign)
library(readstata13)
library(psych)
library(GPArotation)
####################Reading in data##########################################
############Reading in State Response Data
response=read.csv(file="Data/MedicaidResponseData.csv")
summary(response)

response.clean=response%>%
  mutate(expansion=if_else(is.na(as.numeric(expansion)),0,as.numeric(expansion)))%>%
  dplyr::select(state.postal,state.fips,year,state.year,expansion,expansion.delay,expansion.early,research.funds,nfib.support,king.support,authorize,waiver.expansion,nfib.challenge,king.challenge,grant.returns,Active.Waiver,mandate.challenge,leg.challenge,compact,leg.lit,fund.return,repeal,funding.cuts)

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
  mutate(grant.weighted=if_else(is.na(as.numeric(grant.weighted)),0,as.numeric(grant.weighted)))
head(response.working)


######################################Producing our DV's#########################
#########Positive##########
#Expansion - 1 = 20, 0 = 0 #grant.weighted - stays same, #nfib.support - +5 #king.support - +5, #+1 per authorize, stays same
####Negative#######
#nfib.challenge, #king.challenge - -5, #grant.returns, -grant totals*2
#delay, -3 per year
#-2 for delay legislation (funding.cuts, leg.challenge, compacts, fund.return)
#-5 for rejection legislation (mandate.challenge, leg.lit, repeal)
#waiver.expansion, -10
summary(response.working)
response.dvs=response.working%>%
  mutate(nonexpanse.weighted=grant.weighted+
           (nfib.support*5)+
           (king.support*5)+
           (authorize)+
           (nfib.challenge*-5)+
           (king.challenge*-5)+
           (grant.returns)+
           (expansion.delay)+
           (funding.cuts*-2)+
           (leg.challenge*-2)+
           (compact*-2)+
           (fund.return*-2)+
           (mandate.challenge*-5)+
           (leg.lit*-5)+
           (repeal*-5)+
           (waiver.expansion*-10))%>%
  mutate(response.weighted=grant.weighted+
           (nfib.support*5)+
           (king.support*5)+
           (authorize)+
           (nfib.challenge*-5)+
           (king.challenge*-5)+
           (grant.returns)+
           (expansion.delay)+
           (funding.cuts*-2)+
           (leg.challenge*-2)+
           (compact*-2)+
           (fund.return*-2)+
           (mandate.challenge*-5)+
           (leg.lit*-5)+
           (repeal*-5)+
           (waiver.expansion*-10)+
           (expansion*20))%>%
  mutate(nonexpanse.unweighted=grant.total+
           (nfib.support)+
           (king.support)+
           (authorize)+
           (nfib.challenge*-1)+
           (king.challenge*-1)+
           (if_else(grant.returns<0,-1,0))+
           (if_else(expansion.delay<0,-1,0))+
           (funding.cuts*-1)+
           (leg.challenge*-1)+
           (compact*-1)+
           (fund.return*-1)+
           (mandate.challenge*-1)+
           (leg.lit*-1)+
           (repeal*-1)+
           (waiver.expansion*-1))%>%
  mutate(response.unweighted=grant.weighted+
           (nfib.support*5)+
           (king.support*5)+
           (authorize)+
           (nfib.challenge*-5)+
           (king.challenge*-5)+
           (grant.returns)+
           (expansion.delay)+
           (funding.cuts*-2)+
           (leg.challenge*-2)+
           (compact*-2)+
           (fund.return*-2)+
           (mandate.challenge*-5)+
           (leg.lit*-5)+
           (repeal*-5)+
           (waiver.expansion*-10)+
           (expansion*5))
summary(response.dvs$nonexpanse.unweighted[which(response.dvs$expansion==1)])
response.dvs[which(response.dvs$expansion==1 & response.dvs$nonexpanse.unweighted < -1),]
response.dvs[which(response.dvs$expansion==1 & response.dvs$nonexpanse.weighted < -15),]

##############Plotting my DV's
cor.test(response.dvs$response.unweighted,response.dvs$response.weighted)
cor.test(response.dvs$nonexpanse.weighted,response.dvs$nonexpanse.unweighted)
plot(response.dvs$response.unweighted,response.dvs$response.weighted)

####other DV's
dvs=response.dvs%>%
  mutate(litigation=nfib.support-nfib.challenge+king.support-king.challenge)%>%
  mutate(anti.legislation=funding.cuts+leg.challenge+compact+fund.return+mandate.challenge+leg.lit+repeal)%>%
  select(state.postal,state.fips,year,state.year,response.unweighted,response.weighted,nonexpanse.unweighted,nonexpanse.weighted,expansion,litigation,anti.legislation,authorize,grant.weighted)

head(dvs)

####################Attempting a PCA
head(response.working)
pca.data=response.working%>%
  dplyr::select(state.year,grant.weighted,expansion,expansion.delay,nfib.support,king.support,authorize,waiver.expansion,nfib.challenge,king.challenge,grant.returns,mandate.challenge,leg.challenge,compact,leg.lit,fund.return,repeal,funding.cuts,grant.total)%>%
  mutate(litigation=nfib.support-nfib.challenge+king.support-king.challenge)%>%
  mutate(anti.legislation=-1*(funding.cuts+leg.challenge+compact+fund.return+mandate.challenge+leg.lit+repeal))%>%
  dplyr::select(expansion,authorize,litigation,anti.legislation,grant.weighted,grant.returns)
summary(pca.data)
fit <- princomp(expansion+authorize+litigation+anti.legislation+grant.total+grant.returns, data=pca.data, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
length(fit$scores) # the principal components
dev.new()
biplot(fit)
  
####################Attempting a PCA - 2-Use This One!
parallel <- fa.parallel(pca.data, fm = 'wls', fa = 'fa')
threefactor <- fa(pca.data,nfactors = 4,rotate = "oblimin",fm="wls")
print(threefactor)
print(threefactor$loadings,cutoff = 0.3)
plot(threefactor)
fa.diagram(threefactor)

fit <- principal(pca.data, nfactors=2, rotate="oblimin")
fit # print results
print(fit$loadings,cutoff = 0.5)
plot(fit)
fa.diagram(fit)


################Attaching Shor Data
head(dvs)
unique(dvs$year)
shor.data=read.dta13("/Users/eringutbrod/Projects/Resources/Datahub/Shor_data/2018 Update/shor mccarty 1993-2016 state aggregate data May 2018 release (Updated July 2018).dta")
head(shor.data)
shor.useful=shor.data%>%
  filter(year>2010)%>%
  mutate(state.year=paste(st,year,sep=""))%>%
  dplyr::select(state.year,hou_chamber,hou_dem,hou_rep,hou_majority,sen_chamber,sen_dem,sen_rep,sen_majority)
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
  select(state.year,capita.income,fpl,no.insurance,operations.capita,health.donor.capita)
dv.shor.iv=full_join(dv.shor,iv)
head(dv.shor.iv)

#########Attaching competition scores##########
