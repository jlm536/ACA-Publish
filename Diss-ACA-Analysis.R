###Packages and Functions####
library(car)
library(stringr)
library(lattice)
library(grid)
library(latticeExtra)
#library(plyr)
library(nnet)
library(lmtest)
#library(dplyr)
library(mlogit)
library(MASS)
library(clusterSEs)
#library(ggplot2)
library(tidyverse)
library(dotwhisker)
###Writing Functions to analyze models
norm.analytics.plot=function(lmmodel){
sresid <- studres(lmmodel) 
hist(sresid, freq=FALSE, 
   main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
}
analytics=function(lmmodel){
print(summary(lmmodel))
print(ncvTest(lmmodel)) #Null hypothesis is homoskedastic, p<0.05 reject the null hypo, ie you have a problem
print(vif(lmmodel))#Collinearity, nothing above 4
print(durbinWatsonTest(lmmodel, max.lag=5)) #p<0.05 == Problem of autocorrelation
dev.new()
par(mfrow=c(3,1))
qqPlot(lmmodel, main="QQ Plot") 
norm.analytics.plot(lmmodel)
acf.ind=acf(residuals(lmmodel))
}


describe = function(x)
{
    m=mean(x,na.rm=T)
    m=round(m,2)
    s=sd(x,na.rm=T)
    s=round(s,3)
    N=sum(is.na(x))
    n=length(x)-N
    n=round(n,0)
    N=round(N,0)
    se=s/sqrt(n)
    se=round(se,2)
    ci.top=m+(1.96*se)
    ci.bot=m-(1.96*se)
    out=c(ci.top,m,ci.bot,s,se,n,N)
    names(out)=c("top-ci","mean","bot-ci","sd","sem","n","NAs")
   out
}

####Reading in data
working=read.csv(file="/Users/eringutbrod/Research/1 Dissertation Active/Chapter 6 - ACA/ActiveWork/JOP Submission/AnnualACAData13.csv") #reads in annual metricss
head(working)
working.bi=read.csv(file="/Users/eringutbrod/Research/1 Dissertation Active/Chapter 6 - ACA/ActiveWork/JOP Submission/BiACAData.csv") #reads in bi-annual metricss
head(working.bi)
head(working)

working$gov_control[which(working$gov_control==0.5)]=1###Fixing Lincoln Chaffee

working[which(is.na(working$hou_chamber)),]

working=working%>%
  mutate(index.binary.unified=ifelse(index.unified>19,1,ifelse(index.unified<0,-1,0)))
head(working)
working.fixes=working%>%
	mutate(gov_control =1-gov_control)%>% #Makes Gov=1 Republican
	mutate(sen_control=ifelse(sen_control==0.5, 1-hs_control,sen_control))%>%#Converts split chambers to reverse of opposite chamber
	mutate(hs_control =ifelse(hs_control ==0.5, 1-sen_control,hs_control))%>%#Converts split chambers to reverse of opposite chamber
	mutate(sen_control=1-sen_control)%>%#Makes 1=Republican
	mutate(hs_control =1-hs_control)%>%#Makes 1=Republican	
	mutate(leg_control=(sen_control+ hs_control))%>%#Establish republican leg control
	mutate(dom.mean=((hou_majority+sen_majority)/2))%>%#establish a mean ideology
	mutate(index.binary.mod=ifelse(index.binary==1,2,abs(index.binary)))%>%###Establishes a binary
	mutate(rep.mean=((hou_rep+ sen_rep)/2))%>%
	mutate(dem.mean=((hou_dem+ sen_dem)/2))%>%
	mutate(state_risk_factor=((house_risk_factor+ senate_risk_factor)/2))%>%
	mutate(rep_control=sen_control+hs_control+gov_control)%>%
	mutate(divided_leg=ifelse(leg_control==1,1,0))%>%
	na.omit()
head(working.fixes)
summary(working.fixes$hs_risk_redis)
working.fixes$hs_risk_redis_center=as.vector(scale(working.fixes$hs_risk_redis,scale=FALSE))
working.fixes$sen_risk_redis_center=as.vector(scale(working.fixes$sen_risk_redis,scale=FALSE))
working.fixes[which(is.na(working.fixes$h_diffs)),]

head(working.fixes)

is.numeric(working.fixes$hs_risk_redis_center)
#######################################################################
#                                 PLOTTING THE INDEX
#######################################################################
head(working)
index10=dotplot(reorder(state[which(year==10)],index.mean[which(year==14)])~index.mean[which(year==10)],data=working.bi,col="black",xlab="Implementation Index\n2011",scales=list(y=list(alternating=1),x=list(limits=c(-20,30))),panel =function(x,y,...){panel.dotplot(x,y,...);
                      panel.abline(v=10, col="black",lty="dotted");
                      panel.abline(v=0, col="black",lty="dotted")})
index12=dotplot(reorder(state[which(year==12)],index.mean[which(year==14)])~index.mean[which(year==12)],data=working.bi,col="black",xlab="Implementation Index\n2012-2013",scales=list(y=list(alternating=1),x=list(limits=c(-20,30))),panel =function(x,y,...){panel.dotplot(x,y,...);
                      panel.abline(v=10, col="black",lty="dotted");
                      panel.abline(v=0, col="black",lty="dotted")})
index14=dotplot(reorder(state[which(year==14)],index.mean[which(year==14)])~index.mean[which(year==14)],data=working.bi,col="black",xlab="Implementation Index\n2014-2015",panel =function(x,y,...){panel.dotplot(x,y,...);
                      panel.abline(v=20, col="black",lty="dotted");
                      panel.abline(v=-20, col="black",lty="dotted")})

print(index10, position=c(0, 0, 0.37, 1), more=TRUE)
print(index12, position=c(0.31, 0, 0.66, 1), more=TRUE)
print(index14, position=c(0.60, 0, 1, 1), more=FALSE)
grid.text("Figure 1: Medicaid Implementation Index", x = 0.5, y = 0.99, just = c("centre", "top"))


####This is a final plot I will be using
panel.data=working ###This section adds a line to the dataset to better sort the data
panel.data$final.index=array(NA,200)
for (i in c(1:length(panel.data$state))){
  panel.data$final.index[i]=panel.data$index.unified[which(panel.data$state==panel.data$state[i] & panel.data$year==2014)]
}
panel.data=panel.data%>%
  mutate(coop.bin=ifelse(year==2014,20,20))%>%
  mutate(resist.bin=ifelse(year==2014,0,0))

annualindex11=dotplot(reorder(state[which(year==2011)],index[which(year==2014)])~index[which(year==2011)],data=working,col="black",xlab="Implementation Index\n2011",scales=list(y=list(alternating=1),x=list(limits=c(-20,30))),panel =function(x,y,...){panel.dotplot(x,y,...);
  panel.abline(v=10, col="black",lty="dotted");
  panel.abline(v=0, col="black",lty="dotted")})

head(panel.data)
summary(as.factor(panel.data$index.binary))
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
dev.new()
ggplot(panel.data,aes(x= reorder(state,final.index),y=index.unified,color=as.factor(index.binary.unified))) +
  scale_color_manual(labels = c("Resist", "Negotiate","Cooperate"),values=c("#D55E00", "#0072B2", "#009E73")) +
  geom_point() +
  facet_wrap( ~ year, ncol=4, scales="free") +
  #geom_hline(aes(yintercept=coop.bin),color="Blue")+
  #geom_hline(aes(yintercept=resist.bin),color="Red")+
  coord_flip() +
  ggtitle(expression("Annual Medicaid Implementation Index")) +
  labs(y="Medicaid Implementation Index",x="State",color="Implementation Rank:")+
  theme(
    plot.title = element_text(hjust = 0.5,size = 11),
    axis.text.x=element_text(size=8),
    plot.caption=element_text(hjust = 0,size=10,face="italic",lineheight=1),
    axis.text.y=element_text(size=6),
    panel.spacing.x=unit(0.1, "lines"),
    legend.position="bottom",
    legend.direction="horizontal")+
  ggsave(filename="MedicaidIndex.png",device="png",width = 9, height = 6, units = "in", path="/Users/eringutbrod/Research/1 Dissertation Active/JobTalk/Images/Figure PNGS",dpi=300)


#,caption="Source: Author's Calculations"

####Use the following lines to help build your new text table
head(working.fixes)
working[which(working$state=="MN"),]
working$state[which(working$year==2014 & working$index.binary.unified==-1)]

working$state[which(working$year==2012 & working$index.binary.unified==0)]%in%working$state[which(working$year==2013 & working$index.binary.unified==0)]
#######################################################################
#                                 Univariate Analysis
#######################################################################
cor.test(~hou_majority+index.unified,data=working.fixes[which(working.fixes$year==2011),])
cor.test(~hou_majority+index.unified,data=working.fixes[which(working.fixes$year==2012),])
cor.test(~hou_majority+index.unified,data=working.fixes[which(working.fixes$year==2013),])
cor.test(~hou_majority+index.unified,data=working.fixes[which(working.fixes$year==2014),])

cor.test(~sen_majority+index.unified,data=working.fixes[which(working.fixes$year==2011),])
cor.test(~sen_majority+index.unified,data=working.fixes[which(working.fixes$year==2012),])
cor.test(~sen_majority+index.unified,data=working.fixes[which(working.fixes$year==2013),])
cor.test(~sen_majority+index.unified,data=working.fixes[which(working.fixes$year==2014),])


cor.test(~hou_majority+index.unified,data=working.fixes[which(working.fixes$hs_control==1),])
cor.test(~hou_majority+index.unified,data=working.fixes[which(working.fixes$hs_control==0),])

cor.test(~sen_majority+index.unified,data=working.fixes[which(working.fixes$sen_control==1),])
cor.test(~sen_majority+index.unified,data=working.fixes[which(working.fixes$sen_control==0),])



cor.test(~h_diffs+index.unified,data=working.fixes[which(working.fixes$year==2011),])
cor.test(~h_diffs+index.unified,data=working.fixes[which(working.fixes$year==2012),])
cor.test(~h_diffs+index.unified,data=working.fixes[which(working.fixes$year==2013),])
cor.test(~h_diffs+index.unified,data=working.fixes[which(working.fixes$year==2014),])

cor.test(~s_diffs+index.unified,data=working.fixes[which(working.fixes$year==2011),])
cor.test(~s_diffs+index.unified,data=working.fixes[which(working.fixes$year==2012),])
cor.test(~s_diffs+index.unified,data=working.fixes[which(working.fixes$year==2013),])
cor.test(~s_diffs+index.unified,data=working.fixes[which(working.fixes$year==2014),])

cor.test(~hs_risk_redis+index.unified,data=working.fixes[which(working.fixes$year==2011),])
cor.test(~hs_risk_redis+index.unified,data=working.fixes[which(working.fixes$year==2012),])
cor.test(~hs_risk_redis+index.unified,data=working.fixes[which(working.fixes$year==2013),])
cor.test(~hs_risk_redis+index.unified,data=working.fixes[which(working.fixes$year==2014),])

cor.test(~sen_risk_redis+index.unified,data=working.fixes[which(working.fixes$year==2011),])
cor.test(~sen_risk_redis+index.unified,data=working.fixes[which(working.fixes$year==2012),])
cor.test(~sen_risk_redis+index.unified,data=working.fixes[which(working.fixes$year==2013),])
cor.test(~sen_risk_redis+index.unified,data=working.fixes[which(working.fixes$year==2014),])


dim(working.fixes[which(working.fixes$year==2011),])
head(working.fixes)


dev.new()
chamber_plot=ggplot(working.fixes, aes(x = hou_majority,y=index.unified,color=hs_risk_redis)) + 
  geom_point(size=0.1,stroke=0,shape=16)+
  geom_point(aes(x = sen_majority,y=index.unified,color=hs_risk_redis))+
  facet_wrap( ~ year, ncol=4, scales="free") +
  scale_color_distiller(direction=1,palette="GnBu")+
  geom_smooth(method=lm,color="black",lwd=0.5)+
  ggtitle("Figure 6.2: Implementation Score vs Chamber Majority Ideal Point")+
  labs(y="Medicaid Implementation Index",x="Chamber Majority Ideal Point\n(- Corresponds to Liberal and + Corresponds to Conservative)",color="Partisan Competitive Index:",
       caption="Source: Medicaid Implementation Index: Author's Calculations\n             Chamber Ideal Point: Shor, Boris, and Nolan McCarty. 2011.\n                       “The Ideological Mapping of American Legislatures.”\n                       American Political Science Review 105(03): 530–551.")+
  geom_hline(yintercept=0,color="red",lty=2,lwd=0.25)+
  geom_hline(yintercept=20,color="blue",lty=2,lwd=0.25)+
  theme(
    plot.title = element_text(hjust = 0.5,size = 11),
    axis.text.x=element_text(size=8),
    plot.caption=element_text(hjust = 0,size=10,face="italic",lineheight=1),
    axis.text.y=element_text(size=6),
    panel.spacing.x=unit(0.1, "lines"),
    legend.position="top",
    legend.direction="horizontal"
  )
chamber_plot

polar_plot=ggplot(working.fixes, aes(x = h_diffs,y=index.unified,color=hs_risk_redis)) + 
  geom_point()+
  geom_point(aes(x = s_diffs,y=index.unified,color=hs_risk_redis))+
  facet_wrap( ~ year, ncol=4, scales="free") +
  scale_color_distiller(direction=1,palette="GnBu")+
  geom_smooth(method=lm)+
  ggtitle("Figure 2: Implementation Score vs Chamber Polarization Score") +
  labs(y="Medicaid Implementation Index",x="Polarization Score",color="Partisan Competitive Index:")+
  geom_hline(yintercept=0,color="red",lty=2,lwd=0.25)+
  geom_hline(yintercept=20,color="blue",lty=2,lwd=0.25)+
  theme(
    plot.title = element_text(hjust = 0.5,size = 11),
    axis.text.x=element_text(size=8),
    axis.text.y=element_text(size=6),
    panel.spacing.x=unit(0.1, "lines"),
    legend.position="top",
    legend.direction="horizontal"
  )
polar_plot

#######################################################################
#                                 OLS Analysis
#######################################################################
head(working.fixes)
dim(working.fixes)
cor(working.fixes[,c(6:51)])

cor.test(~working.fixes$sen_dem[which(working.fixes$divided_leg==0)]+working.fixes$hou_dem[which(working.fixes$divided_leg==0)])
cor.test(~working.fixes$sen_rep[which(working.fixes$divided_leg==0)]+working.fixes$hou_rep[which(working.fixes$divided_leg==0)])

########Finally a fucking model that works!!!!
###THIS MODEL INCORPORATE BOTH MEASURES
ols.Chamber.House.Full=lm(index.unified ~ capita.income+operations.capita+health.donor.capita+as.factor(gov_control)+as.factor(divided_leg)+ hou_majority + hs_risk_redis_center + h_diffs + hou_majority* hs_risk_redis_center + h_diffs*hs_risk_redis_center  + as.factor(year),data= working.fixes)
analytics(ols.Chamber.House.Full)####This model fail homoskedasticity and 1st degree autocorrelation
length(ols.Chamber.House.Full$residuals)
ols.Chamber.House.Full.robust =coeftest(ols.Chamber.House.Full, vcov = NeweyWest(ols.Chamber.House.Full,lag=1))
print(ols.Chamber.House.Full.robust)

describe(working.fixes$hou_majority)
summary(working.fixes$hou_majority)
describe(working.fixes$hou_rep)
summary(working.fixes$hou_rep)
describe(working.fixes$hou_dem)
summary(working.fixes$hou_dem)
describe(working.fixes$hs_risk_redis)
summary(working.fixes$hs_risk_redis)
describe(working.fixes$hs_risk_redis_center)
summary(working.fixes$hs_risk_redis_center)

#House
-7.04*0.38
#Senate
-6.26*0.35

-7.04-(-7.04 + (0.11*13.23))



##### Are extremes dominating the slope of your model...can you really say anything about partisan variation within a single party..
hist(working.fixes$hou_majority)

###Generating a Risk * Ideal figure across various risk assessments for House
summary(working.fixes$hs_risk_redis)#min 0.54, 1st 0.71, mean=0.77,3rd=0.87,max=1.04
summary(working.fixes$hs_risk_redis_center)#min -0.24, 1st -0.07, mean=0,3rd=0.09,max=0.26
working.risk = working.fixes %>%####Generate a mean variable list of the data table
	dplyr::select(index.unified,capita.income,operations.capita,health.donor.capita,gov_control,divided_leg,hou_majority,hs_risk_redis_center, h_diffs,year)%>%
	mutate(capita.income=mean(capita.income))%>%
	mutate(operations.capita=mean(operations.capita))%>%
	mutate(health.donor.capita=mean(health.donor.capita))%>%
	mutate(h_diffs =mean(h_diffs,na.omit=TRUE))%>%
	mutate(gov_control=1)%>%
	mutate(year=2014)%>%
	mutate(divided_leg=0)

working.risk.min=working.risk%>%
	mutate(hs_risk_redis_center=-0.24)
predict.min=predict.lm(ols.Chamber.House.Full,newdata= working.risk.min,interval='predict')
working.risk.min=cbind(working.risk.min,predict.min)
working.risk.min$class=array("0.54 (Minimum Score)",188)
head(working.risk.min)

working.risk.first=working.risk%>%
	mutate(hs_risk_redis_center=-0.07)
predict.first=predict.lm(ols.Chamber.House.Full,newdata= working.risk.first,interval='predict')
working.risk.first=cbind(working.risk.first, predict.first)
working.risk.first$class=array("0.71 (First Quartile)",188)
head(working.risk.first)
	
working.risk.mean=working.risk%>%
	mutate(hs_risk_redis_center=0)
predict.mean=predict.lm(ols.Chamber.House.Full,newdata= working.risk.mean,interval='predict')
working.risk.mean=cbind(working.risk.mean, predict.mean)
working.risk.mean$class=array("0.77 (Mean)",188)
head(working.risk.mean)
summary(working.fixes$index.unified[which(working.fixes$year<2014)])
working.risk.third=working.risk%>%
	mutate(hs_risk_redis_center=0.09)	
predict.third=predict.lm(ols.Chamber.House.Full,newdata= working.risk.third,interval='predict')
working.risk.third =cbind(working.risk.third, predict.third)
working.risk.third$class=array("0.87 (Third Quartile)",188)
head(working.risk.third)
	
working.risk.max=working.risk%>%
	mutate(hs_risk_redis_center=0.26)	
predict.max=predict.lm(ols.Chamber.House.Full,newdata= working.risk.max,interval='predict')
working.risk.max =cbind(working.risk.max, predict.max)
working.risk.max$class=array("1.04 (Max)",188)
head(working.risk.max)

plottable.data=rbind(working.risk.min, working.risk.first, working.risk.mean, working.risk.third, working.risk.max)
dim(plottable.data)
head(plottable.data)

dev.new()
ggplot(plottable.data,aes(x= hou_majority, y=fit,group=class,color=class)) +
	geom_smooth() +
	scale_color_brewer(palette="GnBu") +
	ggtitle("Predicted Implementation Index vs. Lower Chamber Majority Ideal Point\n(Model Results for the Lower Chamber Model)") +
	labs(x="Lower Chamber Majority Ideal Point",y="Predicted Implementation Index\n(Other variables held at mean)",color="Competition Index Score") +
  geom_hline(yintercept=0,color="red",lty=1,lwd=0.7)+
  geom_hline(yintercept=20,color="blue",lty=1,lwd=0.7)+
  scale_y_continuous(breaks=seq(-10,30,5),labels=seq(-10,30,5))+
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
		)#+
  #ggsave(filename="MedicaidPredicted.png",device="png", width = 8, height = 5, units = "in",path="/Users/eringutbrod/Research/1 Dissertation Active/JobTalk/Images/Figure PNGS",dpi=300)



#caption="Source: Author's Calculations produced from Model 1 in Table 6.2"


#############Senate Plots
ols.Chamber.Senate.Full=lm(index.unified ~ capita.income+operations.capita+health.donor.capita+as.factor(gov_control)+as.factor(divided_leg)+ sen_majority + sen_risk_redis_center + s_diffs+ sen_majority* sen_risk_redis_center + s_diffs* sen_risk_redis_center  +as.factor(year),data= working.fixes)
analytics(ols.Chamber.Senate.Full)
length(ols.Chamber.Senate.Full$residuals)
ols.Chamber.Senate.Full.robust =coeftest(ols.Chamber.Senate.Full, vcov = NeweyWest(ols.Chamber.Senate.Full,lag=1))
print(ols.Chamber.Senate.Full.robust)

describe(working.fixes$sen_majority)
summary(working.fixes$sen_majority)
describe(working.fixes$sen_rep)
summary(working.fixes$sen_rep)

0.38*-7.6228354
dev.new()
dwplot(list(ols.Chamber.House.Full)) %>% 
  relabel_predictors(c("capita.income" = "Per Capita Income",                       
                       "operations.capita" = "State Operations Budget", 
                       "health.donor.capita" = "Health Donations per Capita", 
                       "as.factor(gov_control)1" = "Republican Governor", 
                       "as.factor(divided_leg)1"= "Divided Legislature", 
                       "hou_majority" = "Majority Idealogy",
                       "hs_risk_redis_center" = "Chamber Election Risk",                       
                       "h_diffs" = "Chamber Polarization", 
                       'as.factor(year)2012' = "2012",
                       'as.factor(year)2013' = "2013",
                       'as.factor(year)2014' = "2014",
                       'hou_majority:hs_risk_redis_center' = "Ideology*Election Risk",
                       'hs_risk_redis_center:h_diffs' = "Polarization*Election Risk")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Lower Chamber Model Results") +
  theme(plot.title = element_text(face="bold"),
        legend.position = 'none')+
  ggsave(filename="ACAModelResults.png",device="png", width = 5, height = 4, units = "in", path="/Users/eringutbrod/Research/1 Dissertation Active/JobTalk/Images/Figure PNGS",dpi=300)

dwplot(list(ols.Chamber.Senate.Full)) %>% 
  relabel_predictors(c("capita.income" = "Per Capita Income",                       
                       "operations.capita" = "State Operations Budget", 
                       "health.donor.capita" = "Health Donations per Capita", 
                       "as.factor(gov_control)1" = "Republican Governor", 
                       "as.factor(divided_leg)1"= "Divided Legislature", 
                       "sen_majority" = "Majority Idealogy",
                       "sen_risk_redis_center" = "Chamber Election Risk",                       
                       "s_diffs" = "Chamber Polarization", 
                       'as.factor(year)2012' = "2012",
                       'as.factor(year)2013' = "2013",
                       'as.factor(year)2014' = "2014",
                       'sen_majority:sen_risk_redis_center' = "Ideology*Election Risk",
                       'sen_risk_redis_center:s_diffs' = "Polarization*Election Risk")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Upper Chamber Model Results") +
  theme(plot.title = element_text(face="bold"),
        legend.position = 'none')+
  ggsave(filename="ACAModelResultsSenate.png",device="png", width = 5, height = 4, units = "in", path="/Users/eringutbrod/Research/1 Dissertation Active/JobTalk/Images/Figure PNGS",dpi=300)



  

##########################################################################################
############       Plotting Competitiveness                        #######################
##########################################################################################
head(working.fixes)
risk.plot.data=working.fixes%>%
  select(state,year,state.year,house_risk_factor,hs_risk_redis,senate_risk_factor,sen_risk_redis)
head(risk.plot.data)
hist(risk.plot.data$house_risk_factor[which(risk.plot.data$year==2014)])
risk_hist=ggplot(risk.plot.data)


risk_hist=ggplot(data=risk.plot.data,aes(hs_risk_redis))+
  geom_histogram(binwidth=.1,colour="black", fill="white")+
  facet_wrap( ~ year, ncol=4, scales="free")+
  ggtitle("Annual Chamber Electoral Risk Score Frequencies, (House)") +
  labs(y="Frequency",x="Risk Score") +
  scale_x_continuous(limits=c(0.5,1.2),breaks=seq(0.6,1.1,0.1),labels=seq(0.6,1.1,0.1))+
  theme(plot.title = element_text(hjust = 0.5,size = 14),
        axis.text.x=element_text(size=12,angle=90),
        axis.text.y=element_text(size=12),
        plot.caption=element_text(hjust = 0,size=8,lineheight=1),
        legend.position="top",
        legend.direction="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.35, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_blank())+
  ggsave(filename="HouseRisk.png",device="png",width = 8, height = 5, units = "in", path="/Users/eringutbrod/Research/1 Dissertation Active/JobTalk/Images/Figure PNGS",dpi=300)

risk_avg_hist=ggplot(data=risk.plot.data,aes(house_risk_factor))+
  geom_histogram(binwidth=.1,colour="black", fill="white")+
  facet_wrap( ~ year, ncol=4, scales="free")+
  ggtitle("Annual Average Chamber Electoral Risk Score Frequencies, (House)") +
  labs(y="Frequency",x="Risk Score") +
  scale_x_continuous(limits=c(0.5,1.2),breaks=seq(0.6,1.1,0.1),labels=seq(0.6,1.1,0.1))+
  theme(plot.title = element_text(hjust = 0.5,size = 14),
        axis.text.x=element_text(size=12,angle=90),
        axis.text.y=element_text(size=12),
        plot.caption=element_text(hjust = 0,size=8,lineheight=1),
        legend.position="top",
        legend.direction="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.35, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_blank())+
  ggsave(filename="HouseRiskAverage.png",device="png",width = 8, height = 5, units = "in", path="/Users/eringutbrod/Research/1 Dissertation Active/JobTalk/Images/Figure PNGS",dpi=300)


dev.new()
risk_avg_hist

############Plotting Shor Data
head(working.fixes)
shor.plot=working.fixes%>%
  dplyr::select(state,year,hou_dem,hou_rep,hou_majority)
head(shor.plot)

ggplot(data=shor.plot,aes(hou_dem))+
  geom_histogram(binwidth=.1,colour="black", fill="blue")+
  geom_histogram(data=shor.plot,aes(hou_rep),binwidth=.1,colour="black", fill="red",alpha = 0.4 )+
  ggtitle("State Party Ideology Score, Lower Chamber") +
  labs(y="Frequency",x="Ideology Score") +
  scale_x_continuous(limits=c(-1.8,1.8),breaks=seq(-1.8,1.8,0.2),labels=seq(-1.8,1.8,0.2))+
  theme(plot.title = element_text(hjust = 0.5,size = 14),
        axis.text.x=element_text(size=12,angle=90),
        axis.text.y=element_text(size=12),
        plot.caption=element_text(hjust = 0,size=8,lineheight=1),
        legend.position="top",
        legend.direction="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.35, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_blank())+
  ggsave(filename="LowerChamberPartyIdeology.png",device="png",width = 6, height = 4, units = "in", path="/Users/eringutbrod/Research/1 Dissertation Active/JobTalk/Images/Figure PNGS",dpi=300)


ggplot(data=shor.plot,aes(hou_majority))+
  geom_histogram(binwidth=.1,colour="black", fill="white")+
  #geom_histogram(data=shor.plot,aes(hou_rep),binwidth=.1,colour="black", fill="red",alpha = 0.4 )+
  ggtitle("Majority Party Ideology Score, Lower Chamber") +
  labs(y="Frequency",x="Ideology Score") +
  scale_x_continuous(limits=c(-1.8,1.4),breaks=seq(-1.8,1.4,0.2),labels=seq(-1.8,1.4,0.2))+
  theme(plot.title = element_text(hjust = 0.5,size = 14),
        axis.text.x=element_text(size=12,angle=90),
        axis.text.y=element_text(size=12),
        plot.caption=element_text(hjust = 0,size=8,lineheight=1),
        legend.position="top",
        legend.direction="horizontal",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.35, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_blank())+
  ggsave(filename="LowerChamberMajorityIdeology.png",device="png",width = 6, height = 4, units = "in", path="/Users/eringutbrod/Research/1 Dissertation Active/JobTalk/Images/Figure PNGS",dpi=300)

