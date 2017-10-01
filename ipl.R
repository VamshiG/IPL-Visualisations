matches <- read.csv("D:/kaggle/ipl/matches.csv",header =FALSE)
   View(matches)
deliveries <- read.csv("D:/kaggle/ipl/deliveries.csv")
   View(deliveries)
 matches = matches[-1,]
colnames(matches) <- c("id","season","city","date","team1","team2","toss_winner","toss_decision","result","dl_applied","winner","win_by_runs","win_by_wickets
","player_of_match","venue","umpire1","umpire2","umpire3")
teams <- deliveries %>% select(batting_team)%>%distinct()
teams <- rename(teams, team = batting_team)  
short_team <- c("KKR","RCB","CSK","KXIP","RR","DD","MI","DC","KTK","PWI","SRH","RPS","GL")
teams <- cbind(teams, short_team)
ipl=merge(deliveries,matches,by.x = "match_id", by.y = "id")
library(ggplot2)
library(dplyr)

season1 = subset(matches ,season == 2008)
season2 = subset(matches , season == 2009)
 season3 = subset(matches , season == 2010)
 season4 = subset(matches , season == 2011)
 season5 = subset(matches , season == 2012)
 season6 = subset(matches , season == 2013)
 season7 = subset(matches , season == 2014)
 season8 = subset(matches , season == 2015)
 season9 = subset(matches , season == 2016)
 #number of matches played in different cities
 ggplot(matches[which(!is.na(matches$city)),],aes(city,fill= city,rm.na=T)) +geom_bar() +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of Matches Played") +guides(fill=FALSE)
 
 #winning percentage across seasons
 
 matches_won1<-as.data.frame(table(season1$winner))
 colnames(matches_won1)[2]<-"Won"
 matches_played1<-as.data.frame(table(season1$team2) + table(season1$team1))
 colnames(matches_played1)[2]<-"Played"
 ggplot(left_join(matches_played1,matches_won1 ),aes(reorder(Var1,-Won/Played),Won*100/Played,fill = Var1)) +geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+ylab("Win Percentage") +  guides(fill=FALSE)+coord_cartesian(ylim = c(0, 100))
 tail(season1,1)$winner
 matches_won2<-as.data.frame(table(season2$winner))
 colnames(matches_won2)[2]<-"Won"
 matches_played2<-as.data.frame(table(season2$team2) + table(season2$team1))
 colnames(matches_played2)[2]<-"Played"
 ggplot(left_join(matches_played2,matches_won2 ),aes(reorder(Var1,-Won/Played),Won*100/Played,fill = Var1)) +geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+ylab("Win Percentage") +  guides(fill=FALSE)+coord_cartesian(ylim = c(0, 100))
 tail(season2,1)$winner
 matches_won3<-as.data.frame(table(season3$winner))
 colnames(matches_won3)[2]<-"Won"
 matches_played3<-as.data.frame(table(season3$team2) + table(season3$team1))
 colnames(matches_played3)[2]<-"Played"
 ggplot(left_join(matches_played3,matches_won3 ),aes(reorder(Var1,-Won/Played),Won*100/Played,fill = Var1)) +geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+ylab("Win Percentage") +  guides(fill=FALSE)+coord_cartesian(ylim = c(0, 100))
 tail(season3,1)$winner
 matches_won7<-as.data.frame(table(season7$winner))
 colnames(matches_won7)[2]<-"Won"
 matches_played7<-as.data.frame(table(season7$team2) + table(season7$team1))
 colnames(matches_played7)[2]<-"Played"
 ggplot(left_join(matches_played7,matches_won7 ),aes(reorder(Var1,-Won/Played),Won*100/Played,fill = Var1)) +geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+ylab("Win Percentage") +  guides(fill=FALSE)+coord_cartesian(ylim = c(0, 100))
 tail(season7,1)$winner
 matches_won9<-as.data.frame(table(season9$winner))
 colnames(matches_won9)[2]<-"Won"
 matches_played9<-as.data.frame(table(season9$team2) + table(season9$team1))
 colnames(matches_played9)[2]<-"Played"
 ggplot(left_join(matches_played9,matches_won9 ),aes(reorder(Var1,-Won/Played),Won*100/Played,fill = Var1)) +geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+ylab("Win Percentage") +  guides(fill=FALSE)+coord_cartesian(ylim = c(0, 100))
 tail(season9,1)$winner
 
 
 #toss decision across seasons
 ggplot(matches,aes(season,fill = toss_decision))+geom_bar()+ ggtitle("Toss Decisions across Seasons")+xlab("season")+ylab("count")
 
 #Toss decisions of toss winners
 toss_dec <- matches%>%
   left_join(teams,by=c("toss_winner"="team") )%>%
   select(short_team,toss_winner,toss_decision)%>%
   group_by(short_team,toss_decision)%>%
   summarize(wins=n())
 
 ggplot(toss_dec,aes(x=short_team,y=wins,colour=toss_decision,fill=toss_decision))+
   geom_bar(position = "dodge",stat = "identity")+
   theme(legend.position="right")+
   scale_y_continuous(name="Toss decision")+
   scale_x_discrete(name="Toss winners and toss decisions")+
   ggtitle("Toss decisions by each Team")
 
 #Top 10 Batsman
   deliveries %>% 
   group_by(batsman) %>% 
     summarise(total_runs = sum(batsman_runs)) %>% 
   arrange(desc(total_runs)) %>% 
     top_n(n = 10, wt = total_runs) %>% 
   ggplot(aes(x = reorder(batsman, -total_runs), y = total_runs))+
   geom_bar(aes(fill = batsman),stat = "identity")+
   labs(list(title = "Top 10 Batsman", x = "Batsman", y = "Total Runs"))+
   theme(axis.text.x=element_text(angle=90, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=10))
 
 #Top 10 bowlers
 deliveries %>% 
   group_by(bowler) %>% 
   summarise(total_wickets = length(dismissal_kind[dismissal_kind%in% c("caught","bowled","lbw","stumped","caught and bowled","hit wicket")])) %>% 
   arrange(desc(total_wickets)) %>% top_n(n= 10, wt = total_wickets) %>% 
   ggplot(aes(x = reorder(bowler, -total_wickets), y= total_wickets))+
   geom_bar(aes(fill= bowler), stat = "identity")+
   labs(list(title = "Top 10 Bowlers", x = "Bowler", y = "Total Wickets"))+
   theme(axis.text.x=element_text(angle=90, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=10))
 
 #top 10 bowlers in last overs
 
   deliveries %>% 
   group_by(bowler) %>%filter(over >= 15) %>%
   summarise(total_wickets = length(dismissal_kind[dismissal_kind %in% c("caught","bowled","lbw","stumped","caught and bowled","hit wicket")])) %>% 
   arrange(desc(total_wickets)) %>% top_n(n = 10, wt = total_wickets) %>% 
   ggplot(aes(x = reorder(bowler, -total_wickets), y = total_wickets))+
   geom_bar(aes(fill = bowler),stat = "identity")+
   ggtitle("Top Bowlers\n(Death Overs)")+
   labs(list(x = "Bowler", y = "Total Wickets"))+
   theme(plot.title = element_text(size = 14, face = "bold",hjust = 0.5),axis.text.x=element_text(angle=60, hjust=1, size = 14),axis.text.y=element_text(size = 14))+
   guides(fill = FALSE)
 
 #top 10 batsmen in last overs
 
   deliveries %>% 
   group_by(batsman) %>%filter(over >= 15,sum(batsman_runs) > 1000) %>%
   summarise(total_runs = 100*sum(batsman_runs)/n()) %>% 
   arrange(desc(total_runs)) %>% top_n(n = 10, wt = total_runs) %>% 
   ggplot(aes(x = reorder(batsman, -total_runs), y = total_runs))+
   geom_bar(aes(fill = batsman),stat = "identity")+
   ggtitle("Top Batsmen\n(Death Overs)")+
   labs(list(x = "Batsman", y = "Strike Rate"))+
   theme(plot.title = element_text(size = 14, face = "bold",hjust = 0.5),axis.text.x=element_text(angle=60, hjust=1, size = 14),axis.text.y=element_text(size = 14))+
   guides(fill = FALSE)
 
 #Number of times Man of Matches Awards
   matches %>% 
   group_by(player_of_match) %>% summarise(No_times = n()) %>% arrange(desc(No_times)) %>% top_n(n=10, wt = No_times) %>% 
   ggplot(aes(x = reorder(player_of_match, -No_times), y = No_times))+geom_bar(aes(fill = player_of_match),stat = "identity")+
   labs(list(title = "Man of the Match", x = "Batsman", y = "Number of times"))+
   theme(axis.text.x=element_text(angle=75, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=8))
 
 #Total number of runs scored in each over of the innings
  runscored <- deliveries %>% group_by(over) %>% filter(is_super_over==0) %>% summarise(Runs= sum(total_runs))
  print(runscored)
  runscored %>% ggplot(aes(over,Runs,fill=over))+geom_bar(stat = "identity",color="green")+scale_x_continuous(breaks = 1:20)+guides(fill=F) +xlab("Over") + ylab("Total runs scored") + ggtitle("Total number of runs scored in each over of the innings")
  #Average number of runs scored in each over of the innings
  averunscored <- deliveries %>% group_by(over) %>% filter(is_super_over==0) %>% summarise(Runs= mean(total_runs)*6)
  print(averunscored)
  averunscored %>% ggplot(aes(over,Runs,fill=over))+geom_bar(stat = "identity",color="black")+scale_x_continuous(breaks = 1:20)+ guides(fill=F) +xlab("Over") + ylab("Total runs scored") + ggtitle("Average number of runs scored in each over of the innings")
  
  #key players for RCB
  batsmen<-ipl%>% 
    filter(team1 == "Royal Challengers Bangalore" | team2 == "Royal Challengers Bangalore") %>% 
    group_by(batsman) %>% 
    summarise(runs_scored = sum(batsman_runs)) %>% 
    arrange(desc(runs_scored)) %>% 
    top_n(n= 5, wt = runs_scored)
  
  top_bowls<-ipl%>% 
    filter(team1 == "Royal Challengers Bangalore" | team2 == "Royal Challengers Bangalore") %>% 
    group_by(bowler) %>% 
    summarise(total_wickets = length(dismissal_kind[dismissal_kind%in% c("caught","bowled","lbw","stumped","caught and bowled","hit wicket")])) %>% 
    arrange(desc(total_wickets)) %>% 
    top_n(n= 5, wt = total_wickets)
  
  ggplot(aes(x = reorder(batsman, -runs_scored),y = runs_scored), data =  batsmen)+
    geom_bar(stat  = "identity", aes(fill = batsman))+
    labs(list(title ="Royal Challengers Bangalore" , x = "Batsman", y = "Runs Scored"))+
    theme(axis.text.x=element_text(angle=75, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=8))
  
  ggplot(aes(x = reorder(bowler, -total_wickets),y = total_wickets), data = top_bowls)+
    geom_bar(stat = "identity", aes(fill = bowler))+
    labs(list(title = "Royal Challengers Bangalore", x = "Bowler", y = "Wickets"))+
    theme(axis.text.x=element_text(angle=75, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=8))