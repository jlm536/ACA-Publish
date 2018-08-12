####################Reading in packages and functiosn########################
source("/Users/eringutbrod/Projects/Resources/Code/R/FunctionList.R")
library(foreign)
library(readstata13)
library(psych)
library(GPArotation)

#############################################################################
##############This is the space for actual modeling##########################
#############################################################################
df=read.csv("Data/workingdata-alternate.csv")
head(df)
df=df[,-1]
summary(df)
####################Comparing DV's##########################
##############Plotting my DV's
cor.test(df$response.unweighted,df$response.weighted)
cor.test(df$nonexpanse.weighted,df$nonexpanse.unweighted)
plot(df$response.unweighted,df$response.weighted)
#The results show that the unweighted and weighted scores are pretty comparable

##############PCA analysis with unweighted groups
#df.pca=read.csv("Data/workingdata-.csv")
pca.data=df%>%
  dplyr::select(state.year,grant.weighted.alt,expansion,nfib.support,king.support,authorize,nfib.challenge,king.challenge,grant.returns,anti.legislation,grant.total)%>%
  mutate(litigation=nfib.support-nfib.challenge+king.support-king.challenge)%>%
  mutate(anti.legislation=-1*anti.legislation)%>%
  mutate(grant.returns=-1*grant.returns)%>%
  dplyr::select(expansion,authorize,litigation,anti.legislation,grant.total,grant.returns)

fit <- principal(pca.data, nfactors=2, rotate="oblimin")
fit # print results
print(fit$loadings,cutoff = 0.3)
plot(fit)
fa.diagram(fit)

#########################majority average model##################################
head(df)
df=df%>%
  mutate(chamber.majorities=(hou_majority+sen_majority)/2)%>%
  mutate(response.index=if_else(response.weighted<0,-1,if_else(response.weighted>9,1,0)))

####################Examining key variables#################
hist(df$capita.income)#this should be logged
hist(df$operations.capita)
hist(df$health.donor.capita)
df[which(df$operations.capita>8000),]#Just alaska

hist(df$chamber.majorities)
hist(df$chamber.changes)
hist(df$effective.party)
df[which(df$effective.party>2),]#Just alaska
hist(df$competition.lee)
hist(df$competition.lee.up)

hist(df$response.weighted)

########You need to center your competition variables to interpret chamber majorities
describe(df$chamber.changes)
describe(df$effective.party)
describe(df$competition.lee.up)

df.working=df%>%
  mutate(capita.log=log(capita.income))%>%
  mutate(chamber.majorities=-1*chamber.majorities)%>%
  mutate(competition.lee.up=50-competition.lee)%>%
  mutate(competition.chambers.up=50-competition.lee.chambers)

df.working$chamber.changes.cent=scale(df.working$chamber.changes, scale = FALSE)[,1]
df.working$effective.party.cent=scale(df.working$effective.party, scale=FALSE)[,1]
df.working$competition.lee.cent=scale(df.working$competition.lee.up, scale=FALSE)[,1]
df.working$competition.chambers.cent=scale(df.working$competition.chambers.up, scale=FALSE)[,1]

summary(df.working)
summary(df.working$chamber.changes.cent)
###################Running basic models for weighted scores
maj.chamber.shifts=lm(response.weighted~chamber.changes.cent+chamber.majorities+chamber.majorities*chamber.changes.cent+gov_control+capita.log+operations.capita+health.donor.capita+as.factor(year),data=df.working)
analytics(maj.chamber.shifts)
coeftest(maj.chamber.shifts, vcov = vcovHC(maj.chamber.shifts,type="HC1"))
coeftest(maj.chamber.shifts, vcov = vcovHAC(maj.chamber.shifts))

maj.effective=lm(response.weighted~effective.party.cent+chamber.majorities+chamber.majorities*effective.party.cent+gov_control+capita.log+operations.capita+health.donor.capita+as.factor(year),data=df.working)
analytics(maj.effective)
coeftest(maj.effective, vcov = vcovHC(maj.effective,type="HC1"))
coeftest(maj.effective, vcov = vcovHAC(maj.effective))

maj.competitionlee=lm(response.weighted~competition.lee.cent+chamber.majorities+chamber.majorities*competition.lee.cent+gov_control+capita.log+operations.capita+health.donor.capita+as.factor(year),data=df.working)
analytics(maj.competitionlee)
coeftest(maj.competitionlee, vcov = vcovHC(maj.competitionlee,type="HC1"))
coeftest(maj.competitionlee, vcov = vcovHAC(maj.competitionlee))

maj.competitionchambers=lm(response.weighted~competition.chambers.cent+chamber.majorities+chamber.majorities*competition.chambers.cent+gov_control+capita.log+operations.capita+health.donor.capita+as.factor(year),data=df.working)
analytics(maj.competitionchambers)
coeftest(maj.competitionchambers, vcov = vcovHC(maj.competitionchambers,type="HC1"))
coeftest(maj.competitionchambers, vcov = vcovHAC(maj.competitionchambers))
###########################Lets make some plots###############################
####Implementation Index Plot########
panel.data=df ###This section adds a line to the dataset to better sort the data
summary(panel.data)
dim(panel.data)
panel.data$final.index=array(NA,300)
for (i in c(1:length(panel.data$state.postal))){
  panel.data$final.index[i]=panel.data$response.index[which(panel.data$state.postal==panel.data$state.postal[i] & panel.data$year==2016)]
}

panel.data=panel.data%>%
  mutate(coop.bin=ifelse(year==2014,20,20))%>%
  mutate(resist.bin=ifelse(year==2014,0,0))

summary(as.factor(panel.data$response.index))
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
dev.new()
#color plot
ggplot(panel.data,aes(x= reorder(state.postal,final.index),y=response.weighted,group=as.factor(response.index))) +
  geom_point(aes(shape=as.factor(response.index), color=as.factor(response.index))) +
  scale_shape_manual(name="Response Type",values=c(3, 16, 17),labels = c("Resist", "Negotiate","Cooperate"))+
  scale_color_manual(name="Response Type",values=c("#D55E00", "#0072B2", "#009E73"),labels = c("Resist", "Negotiate","Cooperate")) +
  facet_wrap( ~ year, ncol=6, scales="free") +
  #geom_hline(aes(yintercept=coop.bin),color="Blue")+
  #geom_hline(aes(yintercept=resist.bin),color="Red")+
  coord_flip() +
  ggtitle(expression("Figure 1: Annual Medicaid Implementation Index")) +
  labs(y="Medicaid Implementation Index",x="State")+
  theme(
    plot.title = element_text(hjust = 0.5,size = 11),
    axis.text.x=element_text(size=8),
    plot.caption=element_text(hjust = 0,size=10,face="italic",lineheight=1),
    axis.text.y=element_text(size=6),
    panel.spacing.x=unit(0.1, "lines"),
    legend.position="bottom",
    legend.direction="horizontal")+
  ggsave(filename="MedicaidIndex.png",device="png",width = 11, height = 8, units = "in", path="Figure PNGs",dpi=300)

####Predicted Chamber Shift Plot########
###Chamber Changes Plot
summary(df.working)
plottable.data = df.working %>%####Generate a mean variable list of the data table
  dplyr::select(response.weighted,chamber.changes.cent,effective.party.cent,competition.lee.cent,competition.chambers.cent,chamber.majorities,gov_control,capita.log,operations.capita,health.donor.capita,year,chamber.changes,effective.party,competition.lee.up,competition.chambers.up)%>%
  mutate(capita.log=mean(capita.log,na.rm=TRUE))%>%
  mutate(operations.capita=mean(operations.capita,na.rm=TRUE))%>%
  mutate(health.donor.capita=mean(health.donor.capita,na.rm=TRUE))%>%
  mutate(gov_control=0)%>%
  mutate(year=2014)

summary(df.working$chamber.changes)#min 0,mean=1.578,max=7
summary(df.working$chamber.changes.cent)#min -1.578 mean=0,max=5.42
dim(plottable.data)

chamberplot.min=plottable.data%>%
  mutate(chamber.changes.cent=-1.5782)
chamberpredict.min=predict.lm(maj.chamber.shifts,newdata= chamberplot.min,interval='predict')
chamberplot.min=cbind(chamberplot.min,chamberpredict.min)
chamberplot.min$class=array("0 Shifts",300)
head(chamberplot.min)

chamberplot.two=plottable.data%>%
  mutate(chamber.changes.cent=0.4218)
chamberpredict.two=predict.lm(maj.chamber.shifts,newdata= chamberplot.two,interval='predict')
chamberplot.two=cbind(chamberplot.two,chamberpredict.two)
chamberplot.two$class=array("2 Shifts",300)
head(chamberplot.two)

chamberplot.four=plottable.data%>%
  mutate(chamber.changes.cent=2.4218)
chamberpredict.four=predict.lm(maj.chamber.shifts,newdata= chamberplot.four,interval='predict')
chamberplot.four=cbind(chamberplot.four,chamberpredict.four)
chamberplot.four$class=array("4 Shifts",300)
head(chamberplot.four)

chamberplot.six=plottable.data%>%
  mutate(chamber.changes.cent=4.4218)
chamberpredict.six=predict.lm(maj.chamber.shifts,newdata= chamberplot.six,interval='predict')
chamberplot.six=cbind(chamberplot.six,chamberpredict.six)
chamberplot.six$class=array("6 Shifts",300)
head(chamberplot.six)


chamberplot.data=rbind(chamberplot.min, chamberplot.two, chamberplot.four, chamberplot.six)
dim(chamberplot.data)
head(chamberplot.data)

dev.new()
ggplot(chamberplot.data,aes(x= chamber.majorities, y=fit,group=class,color=class)) +
  geom_smooth() +
  scale_color_brewer(palette="GnBu") +
  ggtitle("Figure 2: Predicted Implementation Index vs. Mean Chamber Majority Ideal Point\nUnder Different Levels of Chamber Turnover") +
  labs(x="Lower Chamber Majority Ideal Point",y="Predicted Implementation Index\n(Other variables held at mean)",color="# of Chamber Changes\nin the Past 10 Years") +
  scale_y_continuous(breaks=seq(-4,14,2),labels=seq(-4,14,2))+
  theme(
    plot.title = element_text(hjust = 0.5,size = 11),
    axis.text.x=element_text(size=8),
    plot.caption=element_text(hjust = 0,size=10,face="italic",lineheight=1),
    axis.text.y=element_text(size=8),
    panel.background = element_rect(fill = "white",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'dashed',
                                    colour = "black"), 
    panel.grid.minor = element_blank()
  )+
ggsave(filename="ChamberChangesPredicted.png",device="png", width = 8, height = 5, units = "in", path="Figure PNGs",dpi=300)


##########Effective Parties Plot data
summary(df.working$effective.party)#min 1.23, 1st=1.733, mean=1.82, third=1.972,max=2.15
summary(df.working$effective.party.cent)#min -0.58, 1st=-0.088 mean=0,3rd= 0.15140, max=0.32778
dim(plottable.data)
hist(df.working$effective.party)

effectiveplot.min=plottable.data%>%
  mutate(effective.party.cent=-0.697)
effectivepredict.min=predict.lm(maj.effective,newdata= effectiveplot.min,interval='predict')
effectiveplot.min=cbind(effectiveplot.min,effectivepredict.min)
effectiveplot.min$class=array("1.2",300)
head(effectiveplot.min)

effectiveplot.first=plottable.data%>%
  mutate(effective.party.cent=-0.497)
effectivepredict.first=predict.lm(maj.effective,newdata= effectiveplot.first,interval='predict')
effectiveplot.first=cbind(effectiveplot.first,effectivepredict.first)
effectiveplot.first$class=array("1.4",300)
head(effectiveplot.first)

effectiveplot.mean=plottable.data%>%
  mutate(effective.party.cent=-0.297)
effectivepredict.mean=predict.lm(maj.effective,newdata= effectiveplot.mean,interval='predict')
effectiveplot.mean=cbind(effectiveplot.mean,effectivepredict.mean)
effectiveplot.mean$class=array("1.6",300)
head(effectiveplot.mean)

effectiveplot.third=plottable.data%>%
  mutate(effective.party.cent=-0.097)
effectivepredict.third=predict.lm(maj.effective,newdata= effectiveplot.third,interval='predict')
effectiveplot.third=cbind(effectiveplot.third,effectivepredict.third)
effectiveplot.third$class=array("1.8 (Mean)",300)
head(effectiveplot.third)

effectiveplot.max=plottable.data%>%
  mutate(effective.party.cent=0.179)
effectivepredict.max=predict.lm(maj.effective,newdata= effectiveplot.max,interval='predict')
effectiveplot.max=cbind(effectiveplot.max,effectivepredict.max)
effectiveplot.max$class=array("2",300)
head(effectiveplot.max)

effectiveplot.data=rbind(effectiveplot.first, effectiveplot.mean, effectiveplot.third, effectiveplot.max)
dim(effectiveplot.data)
head(effectiveplot.data)

dev.new()
ggplot(effectiveplot.data,aes(x= chamber.majorities, y=fit,group=class,color=class)) +
  geom_smooth() +
  scale_color_brewer(palette="GnBu") +
  ggtitle("Figure 3: Predicted Implementation Index vs. Mean Chamber Majority Ideal Point\nUnder Different Effective Party Scores") +
  labs(x="Lower Chamber Majority Ideal Point",y="Predicted Implementation Index\n(Other variables held at mean)",color="# of Effective Parties") +
  scale_y_continuous(breaks=seq(-4,14,2),labels=seq(-4,14,2))+
  theme(
    plot.title = element_text(hjust = 0.5,size = 11),
    axis.text.x=element_text(size=8),
    plot.caption=element_text(hjust = 0,size=10,face="italic",lineheight=1),
    axis.text.y=element_text(size=8),
    panel.background = element_rect(fill = "white",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'dashed',
                                    colour = "black"), 
    panel.grid.minor = element_blank()
  )+
  ggsave(filename="EffectivePredicted.png",device="png", width = 8, height = 5, units = "in", path="Figure PNGs",dpi=300)


##########Competition Chamber Plot Data
summary(df.working$competition.chambers.up)#min 1.23, 1st=1.733, mean=1.82, third=1.972,max=2.15
summary(df.working$competition.chambers.cent)#min -0.58, 1st=-0.088 mean=0,3rd= 0.15140, max=0.32778
dim(plottable.data)
hist(df.working$competition.chambers.up)

competitionplot.min=plottable.data%>%
  mutate(competition.chambers.cent=-14.739)
competitionpredict.min=predict.lm(maj.competitionchambers,newdata= competitionplot.min,interval='predict')
competitionplot.min=cbind(competitionplot.min,competitionpredict.min)
competitionplot.min$class=array("20",300)
head(competitionplot.min)

competitionplot.first=plottable.data%>%
  mutate(competition.chambers.cent=-4.739)
competitionpredict.first=predict.lm(maj.competitionchambers,newdata= competitionplot.first,interval='predict')
competitionplot.first=cbind(competitionplot.first,competitionpredict.first)
competitionplot.first$class=array("30",300)
head(competitionplot.first)

competitionplot.second=plottable.data%>%
  mutate(competition.chambers.cent=5.261)
competitionpredict.second=predict.lm(maj.competitionchambers,newdata= competitionplot.second,interval='predict')
competitionplot.second=cbind(competitionplot.second,competitionpredict.second)
competitionplot.second$class=array("40",300)
head(competitionplot.second)

competitionplot.third=plottable.data%>%
  mutate(competition.chambers.cent=15.261)
competitionpredict.third=predict.lm(maj.competitionchambers,newdata= competitionplot.third,interval='predict')
competitionplot.third=cbind(competitionplot.third,competitionpredict.third)
competitionplot.third$class=array("50",300)
head(competitionplot.third)

chamberplot.data=rbind(competitionplot.min, competitionplot.first, competitionplot.second, competitionplot.third)
dim(chamberplot.data)
head(chamberplot.data)

dev.new()
ggplot(chamberplot.data,aes(x= chamber.majorities, y=fit,group=class,color=class)) +
  geom_smooth() +
  scale_color_brewer(palette="GnBu") +
  ggtitle("Figure 4: Predicted Implementation Index vs. Mean Chamber Majority Ideal Point\nUnder Different Legislative Competition Scores") +
  labs(x="Lower Chamber Majority Ideal Point",y="Predicted Implementation Index\n(Other variables held at mean)",color="Legislative Competition Score") +
  scale_y_continuous(breaks=seq(-4,14,2),labels=seq(-4,14,2))+
  theme(
    plot.title = element_text(hjust = 0.5,size = 11),
    axis.text.x=element_text(size=8),
    plot.caption=element_text(hjust = 0,size=10,face="italic",lineheight=1),
    axis.text.y=element_text(size=8),
    panel.background = element_rect(fill = "white",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'dashed',
                                    colour = "black"), 
    panel.grid.minor = element_blank()
  )+
  ggsave(filename="ChamberPredicted.png",device="png", width = 8, height = 5, units = "in", path="Figure PNGs",dpi=300)


##############Analysis for the appendix################
maj.chamber.shifts.un=lm(response.unweighted~chamber.changes.cent+chamber.majorities+chamber.majorities*chamber.changes.cent+gov_control+capita.log+operations.capita+health.donor.capita+as.factor(year),data=df.working)
analytics(maj.chamber.shifts.un)
  
maj.effective.un=lm(response.unweighted~effective.party.cent+chamber.majorities+chamber.majorities*effective.party.cent+gov_control+capita.log+operations.capita+health.donor.capita+as.factor(year),data=df.working)
analytics(maj.effective.un)
  
maj.competitionlee.un=lm(response.unweighted~competition.lee.cent+chamber.majorities+chamber.majorities*competition.lee.cent+gov_control+capita.log+operations.capita+health.donor.capita+as.factor(year),data=df.working)
analytics(maj.competitionlee.un)

maj.competitionchambers.un=lm(response.unweighted~competition.chambers.cent+chamber.majorities+chamber.majorities*competition.chambers.cent+gov_control+capita.log+operations.capita+health.donor.capita+as.factor(year),data=df.working)
analytics(maj.competitionchambers.un)

#############################################################################
##############This spac examines different models for the main model##########################
#############################################################################
###########################Split Chamber Models######################
chamber.shifts=lm(response.weighted~chamber.changes+hou_majority+hou_majority*chamber.changes+sen_majority+sen_majority*chamber.changes+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(chamber.shifts)###Clears NCV but only barely, high VIF but it still works
##chamber shifts with weighted response is robus
chamber.shifts.unweighted=lm(response.unweighted~chamber.changes+hou_majority+hou_majority*chamber.changes+sen_majority+sen_majority*chamber.changes+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(chamber.shifts.unweighted)###Clears NCV but only barely, high VIF but it still works
##chamber shifts is not predictive

effective.lm=lm(response.weighted~effective.party+hou_majority+hou_majority*effective.party+sen_majority+sen_majority*effective.party+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(effective.lm)###Clears NCV but only barely, high VIF but it still works
##chamber shifts with weighted response is robus
effective.lm.unweighted=lm(response.unweighted~effective.party+hou_majority+hou_majority*effective.party+sen_majority+sen_majority*effective.party+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(effective.lm.unweighted)###Clears NCV but only barely, high VIF but it still works
##chamber shifts is not predictive

pres.margin.lm=lm(response.weighted~pres.margin+hou_majority+hou_majority*pres.margin+sen_majority+sen_majority*pres.margin+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(pres.margin.lm)###Clears NCV but only barely, high VIF but it still works
##chamber shifts with weighted response is robus
pres.margin.lm.unweighted=lm(response.unweighted~pres.margin+hou_majority+hou_majority*pres.margin+sen_majority+sen_majority*pres.margin+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(pres.margin.lm.unweighted)###Clears NCV but only barely, high VIF but it still works
##chamber shifts is not predictive

competitionlee.lm=lm(response.weighted~competition.lee+hou_majority+hou_majority*competition.lee+sen_majority+sen_majority*competition.lee+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(competitionlee.lm)###Clears NCV but only barely, high VIF but it still works
##chamber shifts with weighted response is robus
competitionlee.lm.unweighted=lm(response.unweighted~competition.lee+hou_majority+hou_majority*competition.lee+sen_majority+sen_majority*competition.lee+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(competitionlee.lm.unweighted)###Clears NCV but only barely, high VIF but it still works
##chamber shifts is not predictive

###########################Majority Average Models######################
df=df%>%
  mutate(chamber.majorities=(hou_majority+sen_majority)/2)

maj.chamber.shifts=lm(response.weighted~chamber.changes+chamber.majorities+chamber.majorities*chamber.changes+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(maj.chamber.shifts)
maj.chamber.shifts.robust =coeftest(maj.chamber.shifts, vcov = NeweyWest(maj.chamber.shifts,lag=1))
print(maj.chamber.shifts.robust)

maj.chamber.shifts.unweighted=lm(response.unweighted~chamber.changes+chamber.majorities+chamber.majorities*chamber.changes+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(maj.chamber.shifts.unweighted)

maj.effective=lm(response.weighted~effective.party+chamber.majorities+chamber.majorities*effective.party+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(maj.effective)

maj.effective.unweighted=lm(response.unweighted~effective.party+chamber.majorities+chamber.majorities*effective.party+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(maj.effective.unweighted)

maj.pres.margin=lm(response.weighted~pres.margin+chamber.majorities+chamber.majorities*pres.margin+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(maj.pres.margin)

maj.pres.margin.unweighted=lm(response.unweighted~pres.margin+chamber.majorities+chamber.majorities*pres.margin+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(maj.pres.margin.unweighted)

maj.competitionlee=lm(response.weighted~competition.lee+chamber.majorities+chamber.majorities*competition.lee+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(maj.competitionlee)

maj.competitionlee.unweighted=lm(response.unweighted~competition.lee+chamber.majorities+chamber.majorities*competition.lee+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(maj.competitionlee.unweighted)



###########################chamber Average Models######################
df=df%>%
  mutate(chamber.ideals=(hou_chamber+sen_chamber)/2)

ideals.chamber.shifts=lm(response.weighted~chamber.changes+chamber.ideals+chamber.ideals*chamber.changes+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(ideals.chamber.shifts)

ideals.effective=lm(response.weighted~effective.party+chamber.ideals+chamber.ideals*effective.party+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(ideals.effective)

ideals.competitionlee=lm(response.weighted~competition.lee+chamber.ideals+chamber.ideals*competition.lee+gov_control+capita.income+operations.capita+health.donor.capita+as.factor(year),data=df)
analytics(ideals.competitionlee)




#############################################################################
##############This is the space for actual modeling##########################
#############################################################################
