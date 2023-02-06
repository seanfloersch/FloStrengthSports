getDate <- function(x){
  x <- Sys.Date()
  x <- str_remove_all(x, "-")
  return(x)
}
getyestDate <-function(x){
  x <- Sys.Date() -1
  x <- str_remove_all(x, "-")
  return(x)
}
getNHLPlayer <- function(x){
  h <- read_html("https://www.hockey-reference.com/leagues/NHL_2023_skaters.html") 
  stats <- html_nodes(h, "td") %>% html_text
  len <- length(stats) / 27
  df<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:27)){
      marker = j + (i -1)* 27
      df[i,j]<- stats[marker]
    }
  }
  colnames(df)<- html_nodes(h, ".right+ .poptip , .poptip.right , .center+ .poptip , .left+ .poptip , .poptip.left") %>% html_text
  df <- df[,c(1:15,19:27)]
  df <- df %>% select(-PS, -EV,-PP,-SH,-GW, -`S%`, -ATOI)
  h <- read_html("https://www.hockey-reference.com/leagues/NHL_2023_skaters-advanced.html") 
  stats <- html_nodes(h, "td") %>% html_text
  len <- length(stats) / 25
  dfadv<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:25)){
      marker = j + (i -1)* 25
      dfadv[i,j]<- stats[marker]
    }
  }
  colnames(dfadv)<- html_nodes(h, ".right+ .poptip , .center+ .poptip , .left+ .poptip , .poptip.left") %>% html_text
  dfadv <- dfadv %>% select(Player,Tm,`CF%`, "OISH"=`oiSH%`, "OISV"=`oiSV%`, `PDO`, `dZS%`, TK,GV,`SAtt.`)
  dfplay <- left_join(df, dfadv, by = c("Player", "Tm"))
  for (i in c(2,5:25)) {
    dfplay[i] <- as.numeric(unlist(dfplay[i]))
  }
  ind <- which(dfplay$Pos == "LW" | (dfplay$Pos == "RW" | dfplay$Pos == "F"))
  dfplay$Pos[ind] = "W"
  dfplay <- dfplay %>%
    select(-`FO%`) %>%
    mutate(FOScore = FOW*FOW / (FOL + 1)) %>%
    mutate(PTSpm =(.0001 + ( G + .5 * A)/TOI)) %>%
    na.omit() %>%
    mutate(TORatio = (TK - GV)/ TOI) %>%
    mutate(Shooting = (((S*S/SAtt.) + 0.00001))) %>%
    mutate(TOIpg = TOI / GP) %>%
    mutate(Def = ((1.5 * BLK + HIT) / TOI)) %>%
    mutate(Def1 = OISV) %>%
    filter(GP > 4 & TOIpg>5) %>%
    mutate(yearID = 2022) %>%
    group_by(Pos) %>%
    mutate(PtScore = (PTSpm - mean(PTSpm)) / sd(PTSpm))%>%
    mutate(ShotScore = (Shooting - mean(Shooting)) / sd(Shooting))%>%
    mutate(TOScore = (TORatio - mean(TORatio)) / sd(TORatio))%>%
    mutate(DEFScore = (Def - mean(Def)) / sd(Def)) %>%
    mutate(DEFScore1 = (Def1 - mean(Def1)) / sd(Def1)) %>%
    mutate(FOScore = (FOScore - mean(FOScore)) / sd(FOScore)) %>%
    ungroup %>%
    mutate(FloStrength = NA)
  dfC <- dfplay %>%
    filter(Pos == "C") %>%
    mutate(FloStrength = (2*PtScore +.7 * ShotScore + .4 * TOScore+ .4*DEFScore1+ .5 * FOScore)/4)%>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (TOI/340)) %>%
    select(Player, Age, Tm, Pos, GP,TOI, G, A, `+/-`, FOScore, ShotScore,PtScore, TOScore, DEFScore,DEFScore1, FloStrength, Value)
  dfD <- dfplay %>%
    filter(Pos == "D") %>%
    mutate(FloStrength = (.6* PtScore + .4* TOScore+3 *DEFScore1+ 2*DEFScore)/6)%>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (TOI/1060))%>%
    select(Player, Age, Tm, Pos, GP,TOI, G, A, `+/-`, FOScore, ShotScore,PtScore, TOScore, DEFScore,DEFScore1, FloStrength, Value)
  
  dfW <- dfplay %>%
    filter(Pos == "W") %>%
    mutate(FloStrength = (3* PtScore + .6* TOScore+.4 *DEFScore1+ 2*ShotScore)/6)%>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (TOI/304))%>%
    select(Player, Age, Tm, Pos, GP,TOI, G, A, `+/-`, FOScore, ShotScore,PtScore, TOScore, DEFScore,DEFScore1, FloStrength, Value)
  dfplayer <- rbind(dfC, dfD, dfW)
  
  h <- read_html("https://www.hockey-reference.com/leagues/NHL_2023_goalies.html") 
  stats <- html_nodes(h, "td") %>% html_text
  len <- length(stats) / 25
  dfgoal<- data.frame()
  for (i in c(1:len)) {
    for (j in c(1:25)){
      marker = j + (i -1)* 25
      dfgoal[i,j]<- stats[marker]
    }
  }
  colnames(dfgoal) <- (html_nodes(h, ".left+ .poptip , .sort_default_asc.left , .center+ .poptip") %>% html_text)[1:25]
  for (i in c(2,4:25)) {
    dfgoal[i] <- as.numeric(unlist(dfgoal[i]))
  }
  dfgoal1<-dfgoal %>%
    na.omit %>%
    mutate(Pos = "G") %>%
    mutate(TOI = MIN) %>%
    mutate(FloStrength = GSAA+ 30*(SO/GP) - 30*(RBS/GP)- 200 * (GA/TOI)) %>%
    select(Player, Age, Tm, Pos, GP, TOI,FloStrength) %>%
    group_by(Pos) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * (GP/ 22)) %>%
    mutate(G = 0)%>%
    mutate(A = 0)%>%
    mutate(`+/-` = 0)%>%
    mutate(FOScore = 0)%>%
    mutate(ShotScore = 0)%>%
    mutate(PtScore = 0)%>%
    mutate(TOScore = 0)%>%
    mutate(DEFScore = 0)%>%
    mutate(DEFScore1 = 0)
  dfplayer <- rbind(dfplayer, dfgoal1)
  return(dfplayer)
}
getNHLMaster <- function(NHLGames, FloPlayer){
  NHLGames <- NHLGames
  FloPlayer1 <- FloPlayer
  #################################################
  # Team Stats
  #################################################
  
  h <- read_html("https://www.espn.com/nhl/stats/team/_/season/2023/seasontype/2/table/offensive/sort/avgGoals/dir/desc") 
  Team <- html_nodes(h, ".TeamLink__Logo+ .AnchorLink") %>% html_text
  GP <- html_nodes(h, ".Table__TD:nth-child(1) div") %>% html_text%>% as.numeric
  GF <- html_nodes(h, ".Table__Scroller .Table__TD:nth-child(2) div") %>% html_text %>% as.numeric
  Assists <- html_nodes(h, ".Table__TD:nth-child(3) div") %>% html_text %>% str_remove(",")%>% as.numeric
  PPGoals <- html_nodes(h, ".Table__TD:nth-child(5) div") %>% html_text %>% str_remove(",")%>% as.numeric
  PPSucRate <- html_nodes(h, ".Table__TD:nth-child(6) div") %>% html_text %>% str_remove(",")%>% as.numeric
  SHG <- html_nodes(h, ".Table__TD:nth-child(7) div") %>% html_text %>% str_remove(",")%>% as.numeric
  Shots <- html_nodes(h, ".Table__TD:nth-child(8) div") %>% html_text %>% str_remove(",")%>% as.numeric
  GoalPer <- html_nodes(h, ".Table__TD:nth-child(9) div") %>% html_text %>% str_remove(",")%>% as.numeric
  PenMin <- html_nodes(h, ".Table__TD:nth-child(10) div") %>% html_text %>% str_remove(",")%>% as.numeric
  PenKill <- html_nodes(h, ".Table__TD:nth-child(11) div") %>% html_text %>% str_remove(",")%>% as.numeric
  SOA <- html_nodes(h, ".Table__TD:nth-child(12) div") %>% html_text %>% str_remove(",")%>% as.numeric
  SOG <- html_nodes(h, ".Table__TD:nth-child(13) div") %>% html_text %>% str_remove(",")%>% as.numeric
  SOP <- html_nodes(h, ".Table__TD:nth-child(14) div") %>% html_text %>% str_remove(",")%>% as.numeric
  
  dft <- data.frame(Team, GP, GF, Assists, PPGoals, PPSucRate, SHG, Shots, GoalPer, PenMin, PenKill, SOA, SOG, SOP)
  
  h <- read_html("https://www.espn.com/nhl/stats/team/_/view/goaltending/season/2023/seasontype/2/table/defensive/sort/avgGoalsAgainst/dir/asc") 
  Team <- html_nodes(h, ".TeamLink__Logo+ .AnchorLink") %>% html_text
  GAPG <- html_nodes(h, ".Table__Scroller .Table__TD:nth-child(2)") %>% html_text %>% as.numeric
  Wins <- html_nodes(h, ".Table__TD:nth-child(3)") %>% html_text %>% as.numeric
  Losses <- html_nodes(h, ".Table__TD:nth-child(4)") %>% html_text %>% as.numeric
  OTLoss <- html_nodes(h, ".Table__TD:nth-child(5)") %>% html_text %>% as.numeric
  SA <- html_nodes(h, ".Table__TD:nth-child(6)") %>% html_text %>% str_remove(",")%>%as.numeric
  GA <- html_nodes(h, ".Table__TD:nth-child(7)") %>% html_text %>% as.numeric
  Saves <- html_nodes(h, ".Table__TD:nth-child(8)") %>% html_text %>% str_remove(",")%>%as.numeric
  SavePer <- html_nodes(h, ".Table__TD:nth-child(9)") %>% html_text %>% as.numeric
  ShutOuts <- html_nodes(h, ".Table__TD:nth-child(10)") %>% html_text %>% as.numeric
  SOSA <- html_nodes(h, ".Table__TD:nth-child(11)") %>% html_text %>% as.numeric
  SOS <- html_nodes(h, ".Table__TD:nth-child(12)") %>% html_text %>% as.numeric
  SOSPer <- html_nodes(h, ".Table__TD:nth-child(13)") %>% html_text %>% as.numeric
  dfd <- data.frame(Team, GAPG, Wins, Losses, OTLoss, SA, GA, Saves, SavePer, ShutOuts, SOSA, SOS, SOSPer)
  
  
  TeamStats <- merge(dft, dfd, by = "Team")
  
  TeamStats <- TeamStats %>%
    mutate(wpct = (2* Wins + 1 * OTLoss) / (2*GP))
  
  NHLMaster <- TeamStats %>%
    mutate(GS = round(GF * GP)) %>%
    mutate(GoalRatio = GS / GA) %>%
    mutate(SPG = SA /GP) %>%
    mutate(WinPct = (Wins + .5 * OTLoss) / GP)%>%
    mutate(ShotRatio = Shots / SA) %>%
    mutate(Defense = GAPG + 6 * SavePer) %>%
    mutate(Discapline = (1-PPSucRate) - PenKill) %>%
    mutate(Offense = (.25 * Assists + GS + .25 * Shots + PPSucRate) / GP) %>%
    select(Team,WinPct,Offense, Defense, Discapline) %>%
    mutate(Defense = -1 *((Defense - mean(Defense)) / sd(Defense))) %>%
    mutate(Offense = (Offense - mean(Offense)) / sd(Offense)) %>%
    mutate(Discapline = -1*((Discapline - mean(Discapline)) / sd(Discapline))) %>%
    mutate(TeamScore = .5 + 0.06256* Offense + 0.05065* Defense + 0.01751 *Discapline)
  
  Teams <- c("Anaheim Ducks" = "ANA","Arizona Coyotes" = "ARI","Boston Bruins" = "BOS","Buffalo Sabres" = "BUF","Calgary Flames" = "CGY","Carolina Hurricanes" = "CAR","Chicago Blackhawks" = "CHI","Colorado Avalanche" = "COL","Columbus Blue Jackets" = "CBJ","Dallas Stars" = "DAL","Detroit Red Wings" = "DET","Edmonton Oilers" = "EDM","Florida Panthers" = "FLA","Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN","Montreal Canadiens" = "MTL","Nashville Predators" = "NSH","New Jersey Devils" = "NJD","New York Islanders" = "NYI","New York Rangers" = "NYR","Ottawa Senators" = "OTT","Philadelphia Flyers" = "PHI","Pittsburgh Penguins" = "PIT","San Jose Sharks" = "SJS","Seattle Kraken" = "SEA","St. Louis Blues" = "STL","Tampa Bay Lightning" = "TBL","Toronto Maple Leafs" = "TOR","Vancouver Canucks" = "VAN","Vegas Golden Knights"="VEG","Washington Capitals" = "WSH", "Winnipeg Jets"="WPG")
  NHLMaster$Team <-as.character(Teams[NHLMaster$Team])
  h <- read_html("https://www.espn.com/nhl/injuries") 
  Injured <- html_nodes(h, ".Table__TD .AnchorLink") %>% html_text 
  x <- setdiff(FloPlayer1$Player, Injured)
  FloPlayer <- FloPlayer1 %>%
    filter(Player %in% x)
  
  FPC <- FloPlayer %>%
    filter(Pos == "C" & Tm != "TOT") %>%
    group_by(Tm) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(CenScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, CenScore)
  FPD <- FloPlayer %>%
    filter(Pos == "D" & Tm != "TOT") %>%
    group_by(Tm) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(DefScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, DefScore)
  FPW <- FloPlayer %>%
    filter(Pos == "W" & Tm != "TOT") %>%
    group_by(Tm) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(WingScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, WingScore)
  FPG <- FloPlayer %>%
    filter(Pos == "G" & Tm != "TOT") %>%
    group_by(Tm) %>%
    mutate(WFS = FloStrength * (TOI / sum(TOI)))%>%
    mutate(GScore = sum(WFS)) %>%
    slice(1) %>%
    ungroup %>%
    select(Tm, GScore)
  NHLTP <- left_join(FPC, FPD, by = "Tm")
  NHLTP <- left_join(NHLTP, FPG, by = "Tm")
  NHLTP <- left_join(NHLTP, FPW, by = "Tm") %>%
    mutate(GScore= ifelse(is.na(GScore)==TRUE,-1.5,GScore)) %>%
    mutate(PlayerScore = WingScore + GScore+CenScore+DefScore) %>%
    mutate(yearID = 2022) %>%
    group_by(yearID) %>%
    mutate(PlayerScore = (3 +((PlayerScore - mean(PlayerScore))/ sd(PlayerScore)))/6) %>%
    mutate(Team = Tm) %>%
    ungroup %>%
    select(-Tm)
  
  NHLMaster <- left_join(NHLMaster, NHLTP, by = "Team")
  
  #################################################
  # Massey Method
  #################################################
  
  AFS <- NHLMaster %>%
    select("ATeam"=Team, "ATS"=TeamScore)
  HFS <- NHLMaster %>%
    select("HTeam"=Team,"HTS"= TeamScore)
  NHLGames1 <- left_join(NHLGames, AFS, by = "ATeam")
  NHLGames1 <- left_join(NHLGames1, HFS, by = "HTeam") %>%
    mutate(AFS = NA) %>%
    mutate(HFS = NA)
  ind <- which(NHLGames1$AScore > NHLGames1$HScore )
  NHLGames1$AFS[ind] = NHLGames1$HTS[ind] * NHLGames1$PD[ind]
  NHLGames1$HFS[ind] = (1- NHLGames1$ATS[ind]) * NHLGames1$PD[ind] * -1 
  ind <- which(NHLGames1$AScore < NHLGames1$HScore)
  NHLGames1$HFS[ind] = NHLGames1$ATS[ind] * NHLGames1$PD[ind]
  NHLGames1$AFS[ind] = (1- NHLGames1$HTS[ind]) * NHLGames1$PD[ind] * -1 
  AS <- NHLGames1 %>%
    group_by(ATeam) %>%
    mutate(ASc = sum(AFS)) %>%
    mutate(AGP = length(AFS)) %>%
    slice(1) %>%
    select("Team" = ATeam, ASc, AGP)
  HS <- NHLGames1 %>%
    group_by(HTeam) %>%
    mutate(HSc = sum(HFS)) %>%
    mutate(HGP = length(HFS)) %>%
    slice(1) %>%
    select("Team" = HTeam, HSc, HGP)
  SS <- left_join(AS, HS, by = "Team") %>%
    mutate(SchedScore = (ASc + HSc) / (AGP+HGP)) %>%
    mutate(yearID = 2022) %>%
    group_by(yearID) %>%
    mutate(SchedScore = (3 +((SchedScore - mean(SchedScore))/ sd(SchedScore)))/6) %>%
    ungroup %>%
    select(Team, SchedScore)
  NHLMaster<- merge(NHLMaster, SS, by = "Team") %>%
    mutate(FloStrength = (TeamScore + PlayerScore + SchedScore)/ 3)
  NHLGames1 <-NHLGames %>%
    mutate(daynum = NA)
  gamedays <- unique(NHLGames1$date)
  for (i in c(1:length(gamedays))) {
    ind <- which(NHLGames1$date == gamedays[i])
    NHLGames1$daynum[ind] = i
  }
  AFS <- NHLMaster %>%
    select("ATeam"=Team, "ATS"=TeamScore)
  HFS <- NHLMaster %>%
    select("HTeam"=Team,"HTS"= TeamScore)
  NHLGames1 <- left_join(NHLGames1, AFS, by = "ATeam")
  NHLGames1 <- left_join(NHLGames1, HFS, by = "HTeam") %>%
    mutate(AFS = NA) %>%
    mutate(HFS = NA)
  ind <- which(NHLGames1$AScore > NHLGames1$HScore )
  NHLGames1$AFS[ind] = NHLGames1$HTS[ind] * NHLGames1$PD[ind]
  NHLGames1$HFS[ind] = (1- NHLGames1$ATS[ind]) * NHLGames1$PD[ind] * -1 
  ind <- which(NHLGames1$AScore < NHLGames1$HScore)
  NHLGames1$HFS[ind] = NHLGames1$ATS[ind] * NHLGames1$PD[ind]
  NHLGames1$AFS[ind] = (1- NHLGames1$HTS[ind]) * NHLGames1$PD[ind] * -1 
  away <- NHLGames1 %>%
    select("Team" = ATeam,"Sched"= AFS, daynum)
  home <- NHLGames1 %>%
    select("Team" = HTeam, "Sched" =HFS,daynum)
  sched3 <- rbind(away,home) %>%
    arrange(-daynum) %>%
    group_by(Team) %>%
    slice(1:3) %>%
    mutate(last3games = sum(Sched)/3) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last3games = ((last3games-mean(last3games))/ sd(last3games))/6) %>%
    mutate(last3games = last3games + .5) %>%
    ungroup()%>%
    select(Team, last3games)
  sched5 <- rbind(away,home) %>%
    arrange(daynum) %>%
    group_by(Team) %>%
    slice(1:5) %>%
    mutate(last5games = sum(Sched)/5) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last5games = ((last5games-mean(last5games))/ sd(last5games))/6) %>%
    mutate(last5games = last5games + .5) %>%
    ungroup()%>%
    select(Team, last5games)
  sched7 <- rbind(away,home) %>%
    arrange(daynum) %>%
    group_by(Team) %>%
    slice(1:7) %>%
    mutate(last7games = sum(Sched)/7) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last7games = ((last7games-mean(last7games))/ sd(last7games))/6) %>%
    mutate(last7games = last7games + .5) %>%
    ungroup()%>%
    select(Team, last7games)
  sched10 <- rbind(away,home) %>%
    arrange(daynum) %>%
    group_by(Team) %>%
    slice(1:10) %>%
    mutate(last10games = sum(Sched)/10) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(yr = 1) %>%
    group_by(yr) %>%
    mutate(last10games = ((last10games-mean(last10games))/ sd(last10games))/6) %>%
    mutate(last10games = last10games + .5) %>%
    ungroup()%>%
    select(Team, last10games)
  lastx <- left_join(sched3,sched5, by = "Team")
  lastx <- left_join(lastx,sched7, by = "Team")
  lastx <- left_join(lastx,sched10, by = "Team")
  yesterday <- max(NHLGames1$daynum)
  yesteam<- rbind(away,home) %>%
    mutate(wasyest = ifelse(daynum == yesterday, 1,0)) %>%
    filter(wasyest == 1) %>%
    select(Team) %>%
    mutate(PlayedYest = 1)
  lastx <- left_join(lastx,yesteam, by = "Team")
  ind <- which(is.na(lastx$PlayedYest) == TRUE)
  lastx$PlayedYest[ind] = 0
  NHLMaster <- left_join(NHLMaster, lastx, by = "Team")
  return(NHLMaster)
}  
getNHLResults <- function(yestdate){
  weekschedfun <- function(url)  {
    h <- read_html(url) 
    date <- url %>% str_remove("https://www.espn.com/nhl/scoreboard/_/date/")
    ATeam <- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text
    HTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text
    Scores <- html_nodes(h, ".ScoreCell_Score--scoreboard") %>% html_text
    AScore <- c()
    for (i in c(1:(length(Scores)/2))){
      y <- 2 *i -1
      AScore[i] <- Scores[y]
    }
    HScore <- c()
    for (i in c(1:(length(Scores)/2))){
      y <- 2 *i
      HScore[i] <- Scores[y]
    }
    if (length(HScore) != length(HTeam)){
      x <- length(HScore)
      HTeam <- HTeam[1:x]
      ATeam <- ATeam[1:x]
    }
    df <- data.frame(ATeam, HTeam, AScore, HScore) %>%
      mutate(AScore = as.numeric(AScore))%>%
      mutate(HScore = as.numeric(HScore)) %>%
      mutate(PD = abs(AScore - HScore)) %>%
      mutate(gameID = str_c(date, c(1:length(AScore)), by = "")) %>%
      mutate(date = date)
    Teams <- c("Ducks" = "ANA","Coyotes" = "ARI","Bruins" = "BOS","Sabres" = "BUF","Flames" = "CGY","Hurricanes" = "CAR","Blackhawks" = "CHI","Avalanche" = "COL","Blue Jackets" = "CBJ","Stars" = "DAL","Red Wings" = "DET","Oilers" = "EDM","Panthers" = "FLA","Kings" = "LAK","Wild" = "MIN","Canadiens" = "MTL","Predators" = "NSH","Devils" = "NJD","Islanders" = "NYI","Rangers" = "NYR","Senators" = "OTT","Flyers" = "PHI","Penguins" = "PIT","Sharks" = "SJS","Kraken" = "SEA","Blues" = "STL","Lightning" = "TBL","Maple Leafs" = "TOR","Canucks" = "VAN","Golden Knights"="VEG","Capitals" = "WSH", "Jets"="WPG")
    df$ATeam <-as.character(Teams[df$ATeam])
    df$HTeam <-as.character(Teams[df$HTeam])
    comp <- read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLGames2223")
    df <- rbind(comp, df)
    return(df)
  }
  df <- weekschedfun(str_c("https://www.espn.com/nhl/scoreboard/_/date/", yestdate, sep = "") )
  
  df <- unique(df)
  
  return(df)  
}  
getNHLPredictions <- function(toddate, NHLMaster){
  h <- read_html(str_c("https://www.espn.com/nhl/scoreboard/_/date/", toddate,sep = ""))
  AwayTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--away .db") %>% html_text
  HomeTeam <- html_nodes(h, ".ScoreboardScoreCell__Item--home .db") %>% html_text
  toddf <- data.frame(AwayTeam, HomeTeam)
  Teams <- c("Ducks" = "ANA","Coyotes" = "ARI","Bruins" = "BOS","Sabres" = "BUF","Flames" = "CGY","Hurricanes" = "CAR","Blackhawks" = "CHI","Avalanche" = "COL","Blue Jackets" = "CBJ","Stars" = "DAL","Red Wings" = "DET","Oilers" = "EDM","Panthers" = "FLA","Kings" = "LAK","Wild" = "MIN","Canadiens" = "MTL","Predators" = "NSH","Devils" = "NJD","Islanders" = "NYI","Rangers" = "NYR","Senators" = "OTT","Flyers" = "PHI","Penguins" = "PIT","Sharks" = "SJS","Kraken" = "SEA","Blues" = "STL","Lightning" = "TBL","Maple Leafs" = "TOR","Canucks" = "VAN","Golden Knights"="VEG","Capitals" = "WSH", "Jets"="WPG")
  toddf$AwayTeam <-as.character(Teams[toddf$AwayTeam])
  toddf$HomeTeam <-as.character(Teams[toddf$HomeTeam])
  teamdf <- NHLMaster %>%
    rename("AwayTeam"="Team")
  toddf <- left_join(toddf, teamdf, by = "AwayTeam")
  for (i in c(3:20)) {
    colnames(toddf)[i]<- str_c("Away",colnames(toddf)[i], sep = "")
  }
  teamdf <- NHLMaster %>%
    rename("HomeTeam"="Team")
  toddf <- left_join(toddf, teamdf, by = "HomeTeam")
  for (i in c(21:38)) {
    colnames(toddf)[i]<- str_c("Home",colnames(toddf)[i], sep = "")
  }
  DailyOdds<- function(x) {
    x <- date() %>% str_extract("\\d{1,2}") %>% as.numeric
    h <- read_html("https://www.sportsline.com/nhl/odds/money-line/") 
    BovOddAway <- html_nodes(h, ".away-team td:nth-child(4) .primary") %>% html_text
    BovOddAway <- str_replace_all(BovOddAway, "\\+", "") %>% as.numeric
    BovOddHome <- html_nodes(h, ".home-team td:nth-child(4) .primary") %>% html_text
    BovOddHome <- str_replace_all(BovOddHome, "\\+", "") %>% as.numeric
    Team <- html_nodes(h, ".away-team .cfYQTQ") %>% html_text
    Opponent <- html_nodes(h, ".home-team .cfYQTQ") %>% html_text
    det <- html_nodes(h, ".game-details") %>% html_text
    day <- str_extract(det, "\\d{1,2}") %>% as.numeric
    
    h <- read_html("https://www.sportsline.com/nhl/odds/picks-against-the-spread/") 
    SpreadAway <- html_nodes(h, ".away-team td:nth-child(4) .primary") %>% html_text
    SpreadAway <- str_replace_all(SpreadAway, "\\+", "") %>% as.numeric
    SpreadHome <- html_nodes(h, ".home-team td:nth-child(4) .primary") %>% html_text
    SpreadHome <- str_replace_all(SpreadHome, "\\+", "") %>% as.numeric
    SpreadAwayOdd<- html_nodes(h, ".away-team td:nth-child(4) .secondary") %>% html_text
    SpreadAwayOdd <- str_replace_all(SpreadAwayOdd, "\\+", "") %>% as.numeric
    SpreadHomeOdd<- html_nodes(h, ".home-team td:nth-child(4) .secondary") %>% html_text
    SpreadHomeOdd <- str_replace_all(SpreadHomeOdd, "\\+", "") %>% as.numeric
    
    BookOdds <- data.frame(Team[1:length(SpreadHome)],BovOddAway[1:length(SpreadHome)], Opponent[1:length(SpreadHome)],BovOddHome[1:length(SpreadHome)] ,SpreadAway, SpreadAwayOdd, SpreadHome, SpreadHomeOdd)
    colnames(BookOdds) <- c("ATeam", "Aodd", "HTeam", "Hodd","ASpread", "SpreadAwayOdd", "HSpread", "SpreadHomeOdd")
    BookOdds <-BookOdds %>%
      mutate(BookWin = NA) %>%
      mutate(BookSpread = NA)
    Teams <- c("Ducks" = "ANA","Coyotes" = "ARI","Bruins" = "BOS","Sabres" = "BUF","Flames" = "CGY","Hurricanes" = "CAR","Blackhawks" = "CHI","Avalanche" = "COL","Blue Jackets" = "CBJ","Stars" = "DAL","Red Wings" = "DET","Oilers" = "EDM","Panthers" = "FLA","Kings" = "LAK","Wild" = "MIN","Canadiens" = "MTL","Predators" = "NSH","Devils" = "NJD","Islanders" = "NYI","Rangers" = "NYR","Senators" = "OTT","Flyers" = "PHI","Penguins" = "PIT","Sharks" = "SJS","Kraken" = "SEA","Blues" = "STL","Lightning" = "TBL","Maple Leafs" = "TOR","Canucks" = "VAN","Golden Knights"="VEG","Capitals" = "WSH", "Jets"="WPG")
    BookOdds$ATeam <-as.character(Teams[BookOdds$ATeam])
    BookOdds$HTeam <-as.character(Teams[BookOdds$HTeam])
    ind <- which(BookOdds$ASpread < 0)
    BookOdds$BookWin[ind] <- BookOdds$ATeam[ind]
    ind <- which(BookOdds$ASpread > 0)
    BookOdds$BookWin[ind] <- BookOdds$HTeam[ind]
    ind <- which(BookOdds$SpreadAwayOdd < 0)
    BookOdds$BookSpread[ind] <- BookOdds$ASpread[ind]
    ind <- which(BookOdds$SpreadAwayOdd > 0)
    BookOdds$BookSpread[ind] <- -1 * BookOdds$ASpread[ind]
    BookOdds <- BookOdds 
    return(BookOdds)
  }
  BookOdds <- DailyOdds(x) %>%
    mutate(gameID = str_c(ATeam, HTeam, toddate,sep = "")) %>%
    select(-ATeam, -HTeam)
  BookOdds <- BookOdds[1:length(toddf$AwayTeam),]
  toddf <- toddf %>%
    mutate(gameID = str_c(AwayTeam, HomeTeam, toddate,sep = ""))
  toddf1 <- left_join(toddf, BookOdds, by = "gameID") %>%
    mutate(predPD = -.4 + 10*AwayFloStrength -10 *HomeFloStrength) %>%
    mutate(FloWin = ifelse(predPD >0, AwayTeam, HomeTeam))
  return(toddf1)
}
updYestNHL <- function(NHLGames, yestdate){
  NHLGames1 <- NHLGames %>%
    filter(date == yestdate) %>%
    mutate(gameID = str_c(ATeam, HTeam, date)) %>%
    mutate(WTeam = ifelse(AScore > HScore, ATeam, HTeam))
  yestdf <- read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLPredyest")
  yestcom <- left_join(yestdf, NHLGames1, by = "gameID") %>%
    mutate(FloCorr = ifelse(WTeam == FloWin, 1,0))%>%
    mutate(BookCorr = ifelse(WTeam == BookWin, 1,0))
  commod <- read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLModelComp")
  commod <- rbind(commod, yestcom)
  return(commod)
}
predictNHLModel<- function(preddf, toddate){
  modeldf1<- read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLModelComp")%>%
    mutate(AWin =ifelse(ATeam==WTeam, 1, 0))%>%
    mutate(PD =ifelse(ATeam==WTeam, PD, -1*PD)) %>%
    na.omit()%>%
    mutate(TeamDiff=AwayTeamScore-HomeTeamScore)%>%
    mutate(PlayDiff=AwayPlayerScore-HomePlayerScore)%>%
    mutate(SchedDiff=AwaySchedScore-HomeSchedScore)%>%
    mutate(last3Diff=Awaylast3games-Homelast3games)%>%
    mutate(last5Diff=Awaylast5games-Homelast5games)%>%
    mutate(last7Diff=Awaylast7games-Homelast7games)%>%
    mutate(last10Diff=Awaylast10games-Homelast10games)%>%
    mutate(playyestDiff=AwayPlayedYest-HomePlayedYest)
  preddf<- preddf %>%
    na.omit()
  modeldf<-  modeldf1[,c(4:12,14:20,22:30,32:38,59)]
  modlindf<-  modeldf1[,c(4:12,14:20,22:30,32:38,54)]
  
  set.seed(1212)
  logmod <- glm(AWin~., data = modeldf,family = binomial)
  logpred <- predict(logmod, newdata = preddf, type = "response")
  logwin <- ifelse(logpred>.5,1,0)
  
  set.seed(1212)
  svmmod <- svm(AWin~., data = modeldf,probability = TRUE)
  svmpred <- predict(svmmod, newdata = preddf, probability = TRUE)
  svmwin <- as.vector(ifelse(svmpred>.5,1,0))
  
  set.seed(1212)
  rfmod <- randomForest(AWin~., data = modeldf)
  rfpred<-as.vector(predict(rfmod, newdata = preddf))
  rfwin=ifelse(rfpred>0.5,1,0)
  
  set.seed(1212)
  nnmod  <- neuralnet(AWin~., data = modeldf, hidden=1,act.fct = "logistic",linear.output = FALSE)
  nnpred<-as.vector((compute(nnmod,preddf))$net.result)
  nnwin <- as.vector(ifelse(nnpred>0.5, 1, 0))
  
  set.seed(1212)
  gbmmod <- gbm(AWin~., data = modeldf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 100) 
  gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, preddf, type = "response")
  gbmwin=ifelse(gbmpred>0.5,1,0)
  
  linmod <- lm(PD~., data = modlindf)
  linpred1 <- predict(linmod, newdata = preddf)
  
  predact <- preddf %>%
    mutate(predPD = linpred1) %>%
    mutate(log = logwin) %>%
    mutate(logprob = logpred) %>%
    mutate(svm = svmwin) %>%
    mutate(svmprob = svmpred) %>%
    mutate(gbm = gbmwin) %>%
    mutate(gbmprob = gbmpred) %>%
    mutate(nn = nnwin) %>%
    mutate(nnprob = nnpred) %>%
    mutate(rf = rfwin) %>%
    mutate(rfprob = rfpred) %>%
    mutate(ensprob = (gbm+nn+rf)/3)%>%
    mutate(ens = ifelse(ensprob>.5, 1, 0)) %>%
    mutate(book = ifelse(BookWin==AwayTeam,1,0)) %>%
    mutate(diff = ifelse(ens == book, "no", "yes"))
  strwins <- predact %>%
    filter(diff == "yes") %>%
    mutate(Bet = ifelse(ens == 1, AwayTeam, HomeTeam)) %>%
    mutate(Game = str_c(AwayTeam, HomeTeam, sep = " @ ")) %>%
    mutate(Date = str_remove(gameID, "[A-Z]{1,6}"))%>%
    mutate(x = ifelse(ASpread==-1.5,SpreadAwayOdd,SpreadHomeOdd)) %>%
    mutate(exp = ifelse(x<0, (-1*x/(-1*x+100)),(100/(x+100))))%>%
    mutate(exp = 1-(exp+.225)) %>%
    mutate(exp1 = (1-exp)/(exp)) %>%
    mutate(Earnings = .91*.91*exp1) %>%
    select(Date,Game,predPD,BookSpread, BookWin, ensprob,Bet,Earnings) %>%
    mutate(ensprob = ifelse(ensprob<.5, 1-ensprob, ensprob)) %>%
    arrange(-ensprob)
  sprwins <-predact %>%
    filter(diff == "no") %>%
    mutate(Spread = ifelse(abs(predPD)>1.5,-1.5,1.5)) %>%
    filter(Spread != BookSpread) %>%
    mutate(Bet = Spread) %>%
    mutate(Game = str_c(AwayTeam, HomeTeam, sep = " @ ")) %>%
    mutate(Date = str_remove(gameID, "[A-Z]{1,6}")) %>%
    mutate(Earnings = NA)
  
  ind <- which(sprwins$Spread ==sprwins$ASpread & sprwins$SpreadAwayOdd>0)
  sprwins$Earnings[ind]=sprwins$SpreadAwayOdd[ind]/100
  ind <- which(sprwins$Spread ==sprwins$ASpread & sprwins$SpreadAwayOdd<0)
  sprwins$Earnings[ind]=-100/sprwins$SpreadAwayOdd[ind]
  ind <- which(sprwins$Spread ==sprwins$HSpread & sprwins$SpreadHomeOdd>0)
  sprwins$Earnings[ind]=sprwins$SpreadHomeOdd[ind]/100
  ind <- which(sprwins$Spread ==sprwins$HSpread & sprwins$SpreadHomeOdd<0)
  sprwins$Earnings[ind]=-100/sprwins$SpreadHomeOdd[ind]
  sprwins <-sprwins %>%
    select(Date,Game, predPD,BookSpread, BookWin, ensprob, Bet,Earnings) %>%
    mutate(ensprob= abs(ensprob))%>%
    arrange(-ensprob)
  betsdf <- rbind(strwins, sprwins)    
  return(betsdf)
}
updNHLBets <- function(NHLModdf,yestdate, yestbets){
  yestres <- NHLModdf %>%
    select("Date"=date,WTeam,BookWin, PD) %>%
    mutate(actsp = ifelse(WTeam==BookWin & PD>1.5,-1.5,1.5)) %>%
    filter(Date == yestdate) %>%
    mutate(Date=as.numeric(Date))
  betstr <- left_join(yestbets, yestres, by = c("BookWin", "Date")) %>%
    filter(is.na(as.numeric(Bet)) == TRUE) %>%
    mutate(Earnings = ifelse(WTeam == Bet,Earnings, -1)) %>%
    select(Bet,Date, Earnings, Game,ensprob)
  betsp <- left_join(yestbets, yestres, by = c("BookWin", "Date")) %>%
    filter(is.na(as.numeric(Bet)) == FALSE) %>%
    mutate(Bet =as.numeric(Bet)) %>%
    mutate(Earnings = ifelse(actsp == Bet,Earnings, -1)) %>%
    select(Bet,Date, Earnings, Game,ensprob)
  betdf <-rbind(betstr, betsp)
  combetsNHL <- read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLBetsDF")
  combet <- rbind(betdf, combetsNHL)
  return(combet)
}
getNHLModSuc <- function(x){
  NHLModdf <- read.csv("/Users/seanfloersch/FloStrength/NHLFloStrength/NHLModelComp") %>%
    mutate(AWin = ifelse(WTeam == AwayTeam,1,0)) %>%
    mutate(BookWin = ifelse(BookWin == AwayTeam,1,0)) %>%
    mutate(daynum = NA) %>%
    arrange(-date)%>%
    mutate(TeamDiff=AwayTeamScore-HomeTeamScore)%>%
    mutate(PlayDiff=AwayPlayerScore-HomePlayerScore)%>%
    mutate(SchedDiff=AwaySchedScore-HomeSchedScore)%>%
    mutate(last3Diff=Awaylast3games-Homelast3games)%>%
    mutate(last5Diff=Awaylast5games-Homelast5games)%>%
    mutate(last7Diff=Awaylast7games-Homelast7games)%>%
    mutate(last10Diff=Awaylast10games-Homelast10games)%>%
    mutate(playyestDiff=AwayPlayedYest-HomePlayedYest)
  for (i in c(1:(length(unique(NHLModdf$date))+1))) {
    i = i
    x <- unique(NHLModdf$date)[i]
    ind <- which(NHLModdf$date == x)
    NHLModdf$daynum[ind] <- length(unique(NHLModdf$date)) -i
  }
  NHLModdf <- NHLModdf %>% na.omit()
  traindf <- NHLModdf %>%
    filter(daynum < 16)
  traindf<- traindf[,c(4:12,14:20,22:30,32:38,59)]
  testdf <- NHLModdf%>%
    filter(daynum == 16)
  bookpred <- testdf$BookWin 
  actwin <- testdf$AWin
  daynum <- testdf$daynum
  
  logmod <- glm(AWin~., data = traindf, family = binomial)
  svmmod = svm(AWin~., data = traindf, probability = TRUE)
  svmpred <- as.vector(predict(svmmod, newdata = testdf, probability = TRUE))
  set.seed(1212)
  
  rfmod <- randomForest(AWin~., data = traindf)
  set.seed(1212)
  
  nnmod  <- neuralnet(AWin~., data = traindf, hidden=1,act.fct = "logistic",linear.output = FALSE)
  logpred <- predict(logmod, newdata = testdf, type = "response")
  rfpred<-as.vector(predict(rfmod, newdata = testdf))
  nnpred<-(compute(nnmod,testdf))$net.result
  set.seed(1212)
  
  gbmmod <- gbm(AWin~., data = traindf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 5000) 
  gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, testdf, type = "response")
  df <- data.frame(logpred, svmpred, rfpred, nnpred, gbmpred,bookpred, actwin,daynum)  %>%
    mutate(svmpred = as.numeric(svmpred)) %>%
    mutate(svm = ifelse(svmpred>.5,1,0))%>%
    mutate(log = ifelse(logpred>.5,1,0))%>%
    mutate(rf = ifelse(rfpred>.5,1,0))%>%
    mutate(nn = ifelse(nnpred>.5,1,0))%>%
    mutate(gbm = ifelse(gbmpred>.5,1,0))%>%
    mutate(Ensemble = svm+rf+nn+log+gbm) %>%
    mutate(E2 = rf+nn +gbm) %>%
    mutate(E2 = ifelse(E2 > 1.5, 1,0)) %>%
    mutate(Ensemble = ifelse(Ensemble > 2.5, 1,0)) 
  compdf <- df
  for (i in c(16:(max(NHLModdf$daynum)))){
    traindf <- NHLModdf %>%
      filter(daynum < i)
    traindf<- traindf[,c(4:12,14:20,22:30,32:38,59)]
    
    testdf <- NHLModdf%>%
      filter(daynum == i)
    bookpred <- testdf$BookWin 
    actwin <- testdf$AWin
    daynum <- testdf$daynum
    set.seed(1212)
    logmod <- glm(AWin~., data = traindf, family = binomial)
    svmmod = svm(AWin~., data = traindf, probability = TRUE)
    svmpred <- as.vector(predict(svmmod, newdata = testdf, probability = TRUE))
    set.seed(1212)
    rfmod <- randomForest(AWin~., data = traindf)
    set.seed(1212)
    nnmod  <- neuralnet(AWin~., data = traindf, hidden=1,act.fct = "logistic",linear.output = FALSE)
    logpred <- predict(logmod, newdata = testdf, type = "response")
    rfpred<-as.vector(predict(rfmod, newdata = testdf))
    nnpred<-(compute(nnmod,testdf))$net.result
    set.seed(1212)
    gbmmod <- gbm(AWin~., data = traindf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 5000) 
    gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, testdf, type = "response")
    df <- data.frame(logpred, svmpred, rfpred, nnpred, gbmpred,bookpred, actwin,daynum)  %>%
      mutate(svmpred = as.numeric(svmpred)) %>%
      mutate(svm = ifelse(svmpred>.5,1,0))%>%
      mutate(log = ifelse(logpred>.5,1,0))%>%
      mutate(rf = ifelse(rfpred>.5,1,0))%>%
      mutate(nn = ifelse(nnpred>.5,1,0))%>%
      mutate(gbm = ifelse(gbmpred>.5,1,0))%>%
      mutate(Ensemble = svm+rf+nn+log+gbm) %>%
      mutate(E2 = rf+nn +gbm) %>%
      mutate(E2 = ifelse(E2 > 1.5, 1,0)) %>%
      mutate(Ensemble = ifelse(Ensemble > 2.5, 1,0))
    compdf <- rbind(compdf,df)
    
  }
  compdf <- compdf %>%
    mutate(LogCorr = ifelse(log == actwin,1,0)) %>%
    mutate(SVMCorr = ifelse(svm == actwin,1,0)) %>%
    mutate(RFCorr = ifelse(rf == actwin,1,0)) %>%
    mutate(NNCorr = ifelse(nn == actwin,1,0)) %>%
    mutate(GBMCorr = ifelse(gbm == actwin,1,0)) %>%
    mutate(EnsCorr = ifelse(Ensemble == actwin,1,0))%>%
    mutate(E2Corr = ifelse(E2 == actwin,1,0))%>%
    mutate(BookCorr = ifelse(bookpred == actwin,1,0))
  ovrcorr <- compdf %>%
    filter(daynum == 16) %>%
    group_by(daynum) %>%
    mutate(LogCorr = mean(LogCorr))%>%
    mutate(SVMCorr = mean(SVMCorr))%>%
    mutate(RFCorr = mean(RFCorr))%>%
    mutate(NNCorr = mean(NNCorr))%>%
    mutate(GBMCorr = mean(GBMCorr))%>%
    mutate(EnsCorr = mean(EnsCorr))%>%
    mutate(E2Corr = mean(E2Corr)) %>%
    mutate(BookCorrect = mean(BookCorr)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(Day = daynum - 15) %>%
    select(Day, LogCorr,SVMCorr,RFCorr,NNCorr,GBMCorr, EnsCorr,E2Corr,BookCorrect)
  last <- max(compdf$daynum)
  for (i in c(17:last)){
    ovrcorr1 <- compdf %>%
      filter(daynum <= i)  %>%
      mutate(LogCorr = mean(LogCorr))%>%
      mutate(SVMCorr = mean(SVMCorr))%>%
      mutate(RFCorr = mean(RFCorr))%>%
      mutate(NNCorr = mean(NNCorr))%>%
      mutate(GBMCorr = mean(GBMCorr))%>%
      mutate(EnsCorr = mean(EnsCorr))%>%
      mutate(E2Corr = mean(E2Corr)) %>%
      mutate(BookCorrect = mean(BookCorr)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(Day = i - 15) %>%
      select(Day, LogCorr,SVMCorr,RFCorr,NNCorr,GBMCorr,EnsCorr,E2Corr, BookCorrect)
    
    ovrcorr <- rbind(ovrcorr, ovrcorr1)
  }
  ovr1 <- ovrcorr %>%
    select(Day,"Corr"=LogCorr) %>%
    mutate(Model = "Log")
  ovr2 <- ovrcorr %>%
    select(Day,"Corr"=RFCorr) %>%
    mutate(Model = "RF")
  ovr3 <- ovrcorr %>%
    select(Day,"Corr"=NNCorr) %>%
    mutate(Model = "NN")
  ovr4 <- ovrcorr %>%
    select(Day,"Corr"=GBMCorr) %>%
    mutate(Model = "GBM")
  ovr5 <- ovrcorr %>%
    select(Day,"Corr"=EnsCorr) %>%
    mutate(Model = "Ens")
  ovr6 <- ovrcorr %>%
    select(Day,"Corr"=SVMCorr) %>%
    mutate(Model = "SVM")
  ovr7 <- ovrcorr %>%
    select(Day,"Corr"=E2Corr) %>%
    mutate(Model = "E3")
  ovr8 <- ovrcorr %>%
    select(Day,"Corr"=BookCorrect) %>%
    mutate(Model = "Book")
  ovrcorrplot<- rbind(ovr1,ovr2,ovr3,ovr4,ovr5,ovr6,ovr7,ovr8)
  write_csv(ovrcorrplot,"/Users/seanfloersch/FloStrength/FloStrengthApp/NHLModOvrSuc")
  dfcomp<- compdf %>%
    select(bookpred,svm, log,rf,nn,gbm,actwin,E2,Ensemble)
  svm<- dfcomp %>%
    mutate(Specificity = length(which(svm==actwin&svm==0))/length(which(svm==0)))%>%
    mutate(Sensitivity = length(which(svm==actwin&svm==1))/length(which(svm==1))) %>%
    mutate(Accuracy = length(which(svm==actwin))/length(svm)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(svm)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "SVM")
  gbm<- dfcomp %>%
    mutate(Specificity = length(which(gbm==actwin&gbm==0))/length(which(gbm==0)))%>%
    mutate(Sensitivity = length(which(gbm==actwin&gbm==1))/length(which(gbm==1))) %>%
    mutate(Accuracy = length(which(gbm==actwin))/length(gbm)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(gbm)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "GBM")
  nn<- dfcomp %>%
    mutate(Specificity = length(which(nn==actwin&nn==0))/length(which(nn==0)))%>%
    mutate(Sensitivity = length(which(nn==actwin&nn==1))/length(which(nn==1))) %>%
    mutate(Accuracy = length(which(nn==actwin))/length(nn)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(nn)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "NN")
  log<- dfcomp %>%
    mutate(Specificity = length(which(log==actwin&nn==0))/length(which(log==0)))%>%
    mutate(Sensitivity = length(which(log==actwin&log==1))/length(which(log==1))) %>%
    mutate(Accuracy = length(which(log==actwin))/length(log)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(log)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "Log")
  rf<- dfcomp %>%
    mutate(Specificity = length(which(rf==actwin&rf==0))/length(which(rf==0)))%>%
    mutate(Sensitivity = length(which(rf==actwin&rf==1))/length(which(rf==1))) %>%
    mutate(Accuracy = length(which(rf==actwin))/length(rf)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(rf)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "RF")
  e3<- dfcomp %>%
    mutate(Specificity = length(which(E2==actwin&E2==0))/length(which(E2==0)))%>%
    mutate(Sensitivity = length(which(E2==actwin&E2==1))/length(which(E2==1))) %>%
    mutate(Accuracy = length(which(E2==actwin))/length(E2)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(E2)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "E3")
  ens<- dfcomp %>%
    mutate(Specificity = length(which(Ensemble==actwin&Ensemble==0))/length(which(Ensemble==0)))%>%
    mutate(Sensitivity = length(which(Ensemble==actwin&Ensemble==1))/length(which(Ensemble==1))) %>%
    mutate(Accuracy = length(which(Ensemble==actwin))/length(Ensemble)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(Ensemble)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "Ensemble")
  book<- dfcomp %>%
    mutate(Specificity = length(which(bookpred==actwin&bookpred==0))/length(which(bookpred==0)))%>%
    mutate(Sensitivity = length(which(bookpred==actwin&bookpred==1))/length(which(bookpred==1))) %>%
    mutate(Accuracy = length(which(bookpred==actwin))/length(bookpred)) %>%
    mutate(F1Score = (Specificity+Sensitivity)/2)%>%
    mutate(SampSize = length(bookpred)) %>%
    slice(1)%>%
    select(Accuracy,F1Score,Specificity,Sensitivity,SampSize)%>%
    mutate(Model = "Book")
  dfcomp<- rbind(log,svm,rf,nn,gbm,e3,ens,book)%>%
    mutate(Specificity = round(Specificity,3))%>%
    mutate(Sensitivity = round(Sensitivity,3))%>%
    mutate(Accuracy = round(Accuracy,3))%>%
    mutate(F1Score = round(F1Score,3))
  names<-dfcomp$Model
  dfcomp<-data.frame(t(dfcomp))
  colnames(dfcomp)<- names
  dfcomp <-dfcomp[1:5,]
  write_csv(dfcomp,"/Users/seanfloersch/FloStrength/FloStrengthApp/NHLModSucDF")
}
getNHLModSuc()

