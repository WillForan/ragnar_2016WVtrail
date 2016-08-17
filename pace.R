library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

# from google
racestartdate <- ymd_hm('2016-08-11 15:30 EDT')
sunset    <- ymd_hm('2016-08-11 19:39 EDT')
sunrise   <- ymd_hm('2016-08-12 06:07 EDT')

# leglengths
leglengths <- list(Green=3.5,Yellow=4.6,Red=6.5)

sec2pace <- function(t){
  # N.B. weird stuff if using an origin like year 0
  format(as.POSIXlt(seconds(t*60),origin="2000-01-01 00:00:00 EDT"), format="%M:%S")
}


# http://www.racetecresults.com/MyResults.aspx?uid=16432-2119-1-53149
teamtxt <- Sys.glob('txt/*.txt')

legs.list<- lapply(teamtxt,function(x){cbind(gsub('.txt','',basename(x)),read.table(x)[,-1])})
legs <-
  bind_rows(legs.list) %>%
  setNames(c('team','loop','difficulty','totaltime','legtime')) %>%
  mutate(
         loopno= as.numeric(gsub('-','',legs$loop)),

         runner=loopno%%8,
         runner=ifelse(runner==0,8,runner),

         runnumber=as.factor(rep(rep(c(1,2,3),each=8),length(teamtxt))),

         totaltime=hms(totaltime), 
         totaltime.m=period_to_seconds(totaltime)/60, 

         legtime=hms(legtime),
         legtime.m=period_to_seconds(legtime)/60,

         distance = unname(unlist(leglengths[as.character(difficulty)])),
         pace     = legtime.m/distance
         
         ) %>%
  select(-loop) 
levels(legs$difficulty) <- list("Green"="Green", "Yellow"="Yellow", "Red"="Red")
  

p.teambyrunner <- 
 ggplot(legs) +
  aes(x=difficulty,y=legtime.m,fill=team) +
  geom_bar(stat='identity',position='dodge') +
  geom_text(aes(label=sec2pace(pace),angle=90,y=15),position=position_dodge(.9)) +
  facet_wrap(~runner) + theme_bw()
print(p.teambyrunner )

ggsave(p.teambyrunner,file='teambyrunner.png')

ddrunners <- c('Nicholas','Kent','Jillian','Tracker','Will','Sarah','Brit','Caroline')
sexrunners<- c('M'       ,'M'   ,'F'      ,'M'      ,'M'   ,'F'    ,'F'   ,'F'       )



dd <- 
  legs %>% 
  filter(team=='DisciplesOfDistance') %>%
  mutate(runner.named = paste(sep='.',runner,ddrunners[runner] ),
         sex = sexrunners[runner] )
# not sure why we need this -- otherwise date column is 96 long
dd<-dd[1:24,]


dd$finishdate =  dd$totaltime  + racestartdate
dd$startdate  = c(racestartdate,dd$finishdate[-nrow(dd)])

####
dd$indark = 0

# if sunset is between begin and end
dd$indark<-
 ifelse( dd$startdate < sunset & sunset < dd$finishdate,
       dd$finishdate-dd$startdate - (sunset-dd$startdate),
       dd$indark)
# entirely in the dar
dd$indark<-
 ifelse( dd$startdate > sunset & sunrise > dd$finishdate,
       dd$finishdate-dd$startdate,
       dd$indark)

# if sunrise is between begin and end
dd$indark<-
  ifelse( dd$startdate < sunrise & sunrise < dd$finishdate,
       dd$finishdate-dd$startdate - (dd$finishdate-sunrise),
       dd$indark)

dd$indark <- round(dd$indark/dd$legtime.m,2)


 
p.byleg <- 
 ggplot(dd) +
  aes(x=runnumber,y=pace,fill=runner.named,shape=sex) +
  geom_bar(stat='identity',position='dodge') +
  geom_point(position=position_dodge(.9),aes(y=pace-1)) +
  geom_point(position=position_dodge(.9),aes(y=-1,shape=NULL,color=indark)) +
  facet_wrap(~difficulty) + theme_bw() +
  scale_fill_brewer(type='qual') +
  scale_color_gradient(low="lightblue",high="darkblue")
print(p.byleg)

ggsave(p.byleg,file="byleg.png")



p.byrunner <- 
 ggplot(dd) +
  aes(x=runnumber,y=pace,fill=difficulty) +
  geom_bar(stat='identity',position='dodge') +
  geom_point(position=position_dodge(.9),aes(y=-1,color=indark)) +
  geom_smooth(aes(group=runner.named)) +
  geom_text(aes(label=sec2pace(pace),y=pace-1)) +
  facet_wrap(~runner.named) + theme_bw() +
  scale_color_gradient(low="lightblue",high="darkblue")+
  scale_fill_manual(values=c('lightgreen','yellow','pink'))
print(p.byrunner)

ggsave(file='byrunner.png',p.byrunner)
