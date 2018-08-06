library(tidyverse)
competition.raw=read.csv("Data/SideData/competitionrawdata.csv")
head(competition.raw)
competition.formatted=competition.raw%>%
  mutate(pres.margin=abs(pres.rep.perc-pres.dem.perc))%>%
  mutate(hs_dem_perc=100*(hs_dem_in_sess/hs_tot_in_sess))%>%
  mutate(sen_dem_perc=100*(sen_dem_in_sess/sen_tot_in_sess))%>%
  mutate(competition.lee=abs(((sen_dem_perc+hs_dem_perc+gov.dem.percent)/3)-50))%>%
  mutate(dem.seats=(sen_dem_in_sess+hs_dem_in_sess)/(sen_tot_in_sess+hs_tot_in_sess))%>%
  mutate(rep.seats=(sen_rep_in_sess+hs_rep_in_sess)/(sen_tot_in_sess+hs_tot_in_sess))%>%
  mutate(effective.party=1/((dem.seats)^2+(rep.seats)^2))%>%
  arrange(state.year)%>%
  mutate(hs_switch=if_else(hs_control==lag(hs_control,n=1L),0,1))%>%
  mutate(sen_switch=if_else(sen_control==lag(sen_control,n=1L),0,1))%>%
  mutate(hs_changes=hs_switch+
           lag(hs_switch,1)+
           lag(hs_switch,2)+
           lag(hs_switch,3)+
           lag(hs_switch,4)+
           lag(hs_switch,5)+
           lag(hs_switch,6)+
           lag(hs_switch,7)+
           lag(hs_switch,8)+
           lag(hs_switch,9))%>%
  mutate(sen_changes=sen_switch+
           lag(sen_switch,1)+
           lag(sen_switch,2)+
           lag(sen_switch,3)+
           lag(sen_switch,4)+
           lag(sen_switch,5)+
           lag(sen_switch,6)+
           lag(sen_switch,7)+
           lag(sen_switch,8)+
           lag(sen_switch,9))%>%
  mutate(chamber.changes=hs_changes+sen_changes)%>%
  filter(year>2010)%>%
  dplyr::select(state.year,chamber.changes,effective.party,pres.margin,competition.lee)

competition.formatted[c(1:20),]
write.csv(competition.formatted,"Data/competitionvariables.csv")

############################THIS SECTION COMPILES THE COMPETITION RAW DATA AND NEED NOT BE RUN###########################
#############Reading in Klarner Data 2000-2011
klarner.balance=read.csv('/Users/eringutbrod/Projects/Resources/Datahub/PartisanBalance/Partisan_Balance_For_Use2011_06_09b.csv')
head(klarner.balance)
klarner.useful=klarner.balance%>%	
  select(year,state,sen_cont_alt,hs_cont_alt,govparty_c,sen_elections_this_year,hs_elections_this_year,gub_election,sen_dem_in_sess,sen_rep_in_sess,hs_dem_in_sess,hs_rep_in_sess, sen_tot_in_sess, hs_tot_in_sess)%>%#Cont_alt is binary with 1=Dems,0-Rep,0.5=Split
  setNames(c("year","state.name","sen_control","hs_control","gov_control","senate_election_nxt","house_election_nxt",'gov_election_nxt',"sen_dem_in_sess","sen_rep_in_sess","hs_dem_in_sess","hs_rep_in_sess", "sen_tot_in_sess", "hs_tot_in_sess"))%>%#Set the names
  filter(year>2000)%>%
  filter(year<2012)%>%
  arrange(year, state.name)%>%
  mutate(dem_control= sen_control+ hs_control+ gov_control)%>% #3=Dem
  mutate(rep_control=3-dem_control)

head(klarner.useful)

###############Adding a state variable to Klarner Data for joining
###Adding a state variable
head(klarner.useful)
klarner.useful$state=array(NA,550)
state.fips=read.csv("/Users/eringutbrod/Projects/Resources/Datahub/FipsConverter.csv")
state.fips$name =as.character(state.fips$name)
state.fips$postal=as.character(state.fips$postal)
klarner.useful$state.name=as.character(klarner.useful$state.name)
for(i in state.fips$name){
  klarner.useful$state[which(klarner.useful$state.name==i)]=state.fips$postal[which(state.fips$name==i)]
}
head(klarner.useful)

############Grabbing data from 2011-2016
partisan_2016=read.csv('/Users/eringutbrod/Projects/Resources/Datahub/PartisanBalance/partisancomposition2010-2016.csv')
head(partisan_2016)####1=Rep,-1==Dem--Gov,1=Rep,0=mixed,-1=Dem
partisan_useful=partisan_2016%>%
  filter(year>2011)%>%
  mutate(sen_control=ifelse(senate.dem>senate.rep,1,ifelse(senate.dem==senate.rep,0.5,0)))%>%
  mutate(hs_control=ifelse(house.dem>house.rep,1,ifelse(house.dem==house.rep,0.5,0)))%>%
  mutate(gov_control=ifelse(gov.control==1,0,1))%>%
  select(state,year,sen_control,hs_control,gov_control,senate_election_nxt,house_election_nxt,gov_election_nxt,senate.dem,senate.rep,house.dem,house.rep,senate,house)%>%
  setNames(c('state','year','sen_control','hs_control','gov_control','senate_election_nxt','house_election_nxt','gov_election_nxt',"sen_dem_in_sess","sen_rep_in_sess","hs_dem_in_sess","hs_rep_in_sess","sen_tot_in_sess", "hs_tot_in_sess"))%>%
  mutate(dem_control= sen_control+ hs_control+ gov_control)%>% #3=Dem
  mutate(rep_control=3-dem_control)%>%
  arrange(year,state)
head(partisan_useful)


##########Combining that information
klarner.useful=klarner.useful[,-2]
partisan_data=rbind(klarner.useful,partisan_useful)
dim(partisan_data)
unique(partisan_data$year)

#########################ADDING IN GUB DATA
summary(partisan_data)
gov_votes=read.csv('Data/stategovvote.csv')
summary(gov_votes)
gov_votes=gov_votes%>%
  filter(year>2007)
summary(gov_votes)
gov_votes=gov_votes[-which(is.na(gov_votes$state.name)),]
gov_votes=gov_votes%>%
  dplyr::select(state.year,gov.rep.votes,gov.dem.votes,gov.rep.percent,gov.dem.percent,election.year)
partisan_data=partisan_data%>%
  mutate(state.year=paste(state,year,sep=""))
partisan_plusgov=full_join(partisan_data,gov_votes)

head(partisan_plusgov)

#############Adding in presidential data###########

pres.vote=read.csv("Data/SideData/presvotesbystateannual.csv")
head(pres.vote)
dim(pres.vote)
pres.vote$state=array(NA,650)
pres.vote$state.name=as.character(pres.vote$state.name)
for(i in state.fips$name){
  pres.vote$state[which(pres.vote$state.name==i)]=state.fips$postal[which(state.fips$name==i)]
}
dim(pres.vote)
pres.vote=pres.vote%>%
  mutate(state.year=paste(state,year,sep=""))%>%
  mutate(pres.electionyear=election.year)%>%
  dplyr::select(state.year,pres.votes,pres.rep,pres.dem,pres.rep.perc,pres.dem.perc,pres.electionyear)
head(pres.vote)  
partisan_plus=full_join(partisan_plusgov,pres.vote)

head(partisan_plus)

#####################################
write.csv(partisan_plus,"Data/SideData/competitionrawdata.csv")



############This section processes, cleans and attaches gov data, it does not need run!!!!
gov_votes=read.csv('Data/SideData/gov_vote_data.csv')

head(gov_votes)
dim(gov_votes)
gov_votes$state=array(NA,155)
gov_votes$state.name=as.character(gov_votes$state.name)
for(i in state.fips$name){
  gov_votes$state[which(gov_votes$state.name==i)]=state.fips$postal[which(state.fips$name==i)]
}
dim(gov_votes)
gov_votes=gov_votes%>%
  mutate(state.year=paste(state,year,sep=""))%>%
  mutate(election.year=1)
head(partisan_data)
testing=partisan_data%>%
  dplyr::select(state,year)%>%
  filter(year>2006)%>%
  mutate(state.year=paste(state,year,sep=""))
head(testing)
testing.2=full_join(testing,gov_votes)
dim(testing.2)
50*6
unique(testing.2$state)
write.csv(testing.2,"data/stategovevotestest.csv")





















###################THIS CLEANS PRES VOTE DATA AND DOES NOT NEED RUN##############
pres=read.csv("Data/SideData/presvotesbystate.csv")
head(pres)
pres.2004plus=pres%>%
  filter(election.year==2004)
pres.2004=pres.2004plus%>%
  mutate(year=2004)
pres.2005=pres.2004plus%>%
  mutate(year=2005)
pres.2006=pres.2004plus%>%
  mutate(year=2006)
pres.2007=pres.2004plus%>%
  mutate(year=2007)
pres.out=rbind(pres.2004,pres.2005,pres.2006,pres.2007)

pres.2008plus=pres%>%
  filter(election.year==2008)
pres.2008=pres.2008plus%>%
  mutate(year=2008)
pres.2009=pres.2008plus%>%
  mutate(year=2009)
pres.2010=pres.2008plus%>%
  mutate(year=2010)
pres.2011=pres.2008plus%>%
  mutate(year=2011)
pres.out=rbind(pres.out,pres.2008,pres.2009,pres.2010,pres.2011)

pres.2012plus=pres%>%
  filter(election.year==2012)
pres.2012=pres.2012plus%>%
  mutate(year=2012)
pres.2013=pres.2012plus%>%
  mutate(year=2013)
pres.2014=pres.2012plus%>%
  mutate(year=2014)
pres.2015=pres.2012plus%>%
  mutate(year=2015)
pres.out=rbind(pres.out,pres.2012,pres.2013,pres.2014,pres.2015)

pres.2016plus=pres%>%
  filter(election.year==2016)
pres.2016=pres.2016plus%>%
  mutate(year=2016)
pres.out=rbind(pres.out,pres.2016)

summary(pres.out)
dim(pres.out)
unique(pres.out$year)
write.csv(pres.out,"Data/SideData/presvotesbystateannual.csv")
