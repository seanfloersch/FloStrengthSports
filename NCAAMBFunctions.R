getDate <- function(x){
  x <- Sys.Date()
  x <- str_remove_all(x, "-")
  return(x)
}
getyestDate <- function(x){
  x <- Sys.Date() -1
  x <- str_remove_all(x, "-")
  return(x)
}
getNCAAMBTeam <- function(ResultsDf){
  getFG <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/148"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:6)) {
        df[i,j]<- stats[j + (i -1)* 6]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/148/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:6)) {
          df1[j,k]<- stats[k + (j -1)* 6]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "FGM", "FGA", "FGP")
    df <- df %>%
      select(-Rank)
    return(df)
  }
  dfFG <- getFG(x)
  #########
  getOppFG <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/149"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:6)) {
        df[i,j]<- stats[j + (i -1)* 6]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/149/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:6)) {
          df1[j,k]<- stats[k + (j -1)* 6]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "oppFGM", "oppFGA", "oppFGP")
    df <- df %>%
      select(-Rank,-Gm)
    return(df)
  }
  dfoppFG <- getOppFG(x)
  ######
  get3FG <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/152"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:6)) {
        df[i,j]<- stats[j + (i -1)* 6]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/152/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:6)) {
          df1[j,k]<- stats[k + (j -1)* 6]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "TPM", "TPPA", "TPP")
    df <- df %>%
      select(-Rank, -Gm)
    return(df)
  }
  df3P <- get3FG(x)
  ######
  getOpp3FG <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/518"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:6)) {
        df[i,j]<- stats[j + (i -1)* 6]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/518/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:6)) {
          df1[j,k]<- stats[k + (j -1)* 6]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "OTPA", "OTPM", "OTPP")
    df <- df %>%
      select(-Rank, -Gm)
    return(df)
  }
  dfopp3P <- getOpp3FG(x)
  dfshooting <- left_join(dfFG, dfoppFG, by = "Team")
  dfshooting <- left_join(dfshooting, df3P, by = "Team")
  dfshooting <- left_join(dfshooting, dfopp3P, by = "Team")
  ######
  get3FG <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/152"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:6)) {
        df[i,j]<- stats[j + (i -1)* 6]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/152/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:6)) {
          df1[j,k]<- stats[k + (j -1)* 6]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "TPM", "TPPA", "TPP")
    df <- df %>%
      select(-Rank, -Gm)
    return(df)
  }
  df3P <- get3FG(x)
  ######
  getRB <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/151"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:8)) {
        df[i,j]<- stats[j + (i -1)* 8]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/151/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:8)) {
          df1[j,k]<- stats[k + (j -1)* 8]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "Reb", "RPG", "OppReb", "OppRPG", "RebDiff")
    df <- df %>%
      select(-Rank, -Gm)
    return(df)
  }
  dfReb <- getRB(x)
  ######
  getATO <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/474"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:6)) {
        df[i,j]<- stats[j + (i -1)* 6]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/474/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:6)) {
          df1[j,k]<- stats[k + (j -1)* 6]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "AST", "TO", "ATOR")
    df <- df %>%
      select(-Rank, -Gm)
    return(df)
  }
  dfATOR <- getATO(x)
  ######
  getFoul <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/286"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:6)) {
        df[i,j]<- stats[j + (i -1)* 6]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/286/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:6)) {
          df1[j,k]<- stats[k + (j -1)* 6]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "Foul", "PFPG", "DQ")
    df <- df %>%
      select(-Rank, -Gm)
    return(df)
  }
  dfFoul <- getFoul(x)
  ######
  getoppTO <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/931"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:5)) {
        df[i,j]<- stats[j + (i -1)* 5]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/931/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:5)) {
          df1[j,k]<- stats[k + (j -1)* 5]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "oppTO", "oppTOPG")
    df <- df %>%
      select(-Rank, -Gm)
    return(df)
  }
  dfOTO <- getoppTO(x)
  ######
  getDef <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/146"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:5)) {
        df[i,j]<- stats[j + (i -1)* 5]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/146/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:5)) {
          df1[j,k]<- stats[k + (j -1)* 5]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "oppPt", "oppPPG")
    df <- df %>%
      select(-Rank, -Gm)
    return(df)
  }
  dfdef <- getDef(x)
  ######
  getBK <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/214"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:5)) {
        df[i,j]<- stats[j + (i -1)* 5]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/214/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:5)) {
          df1[j,k]<- stats[k + (j -1)* 5]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "Gm", "Blk", "BlkPG")
    df <- df %>%
      select(-Rank, -Gm)
    return(df)
  }
  dfblk <- getBK(x)
  ######
  getFT <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/150"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:6)) {
        df[i,j]<- stats[j + (i -1)* 6]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/150/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:6)) {
          df1[j,k]<- stats[k + (j -1)* 6]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank","Team", "Gm", "FT", "FTA", "FTP")
    df <- df %>%
      select(-Rank, -Gm)
    return(df)
  }
  dfft <- getFT(x)
  ######
  getWPCT <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/168"))
    stats <- html_nodes(h, "td") %>% html_text
    len <- length(html_nodes(h, ".school") %>% html_text)
    df <- data.frame()
    for (i in c(1:len)){
      for (j in c(1:5)) {
        df[i,j]<- stats[j + (i -1)* 5]
      }
    }
    for (i in c(2:8)){
      h <- read_html(str_c("https://www.ncaa.com/stats/basketball-men/d1/current/team/168/p",i))
      stats <- html_nodes(h, "td") %>% html_text
      len <- length(html_nodes(h, ".school") %>% html_text)
      df1 <- data.frame()
      for (j in c(1:len)){
        for (k in c(1:5)) {
          df1[j,k]<- stats[k + (j -1)* 5]
        }
      }
      df <- rbind(df, df1)
    }
    colnames(df)<- c("Rank", "Team", "W", "L", "WPCT")
    df <- df %>%
      select(-Rank)
    return(df)
  }
  dfwpc <- getWPCT(x)
  ########################
  dftotal <- left_join(dfshooting, dfReb, by = "Team")
  dftotal <- left_join(dftotal, dfATOR, by = "Team")
  dftotal <- left_join(dftotal, dfFoul, by = "Team")
  dftotal <- left_join(dftotal, dfOTO, by = "Team")
  dftotal <- left_join(dftotal, dfblk, by = "Team")
  dftotal <- left_join(dftotal, dfdef, by = "Team")
  dftotal <- left_join(dftotal, dfwpc, by = "Team")
  dftotal <- left_join(dftotal, dfft, by = "Team")
  
  for (i in c(2:37)){
    x <- as.numeric(unname(unlist(dftotal[i])))
    dftotal[i] <- as.numeric(x)
  } 
  dfteam <- dftotal %>% 
    na.omit() %>%
    mutate(TwoFG = FGM - TPM)%>% 
    mutate(TwoFGA = FGA - TPPA)%>% 
    mutate(oppTwoFG = oppFGM - OTPM)%>% 
    mutate(oppTwoFGA = oppFGA - OTPA) %>%
    mutate(Shooting = ((TwoFG*TwoFG/TwoFGA) + 1.5 * (TPM*TPM/TPPA) + .5 *(FT * FT / FTA))/ Gm) %>%
    mutate(Defense = ((oppTwoFG*oppTwoFG/oppTwoFGA) + 1.5 * (OTPM*OTPM/OTPA) + .3 *(Foul))/ Gm) %>%
    mutate(Rebounding = (Reb-OppReb)/Gm) %>%
    mutate(Playmaking = (AST - 2*TO + .6* Blk + 2*oppTO)/Gm) %>%
    select(Team, W, L, WPCT, Shooting, Defense, Rebounding, Playmaking) %>%
    mutate(yearID = 2022) %>%
    group_by(yearID) %>%
    mutate(Shooting = (3+((Shooting - mean(Shooting))/sd(Shooting)))/6) %>%
    mutate(Defense = (3+(-1*(Defense - mean(Defense))/sd(Defense)))/6) %>%
    mutate(Rebounding = (3+((Rebounding - mean(Rebounding))/sd(Rebounding)))/6) %>%
    mutate(Playmaking = (3+((Playmaking - mean(Playmaking))/sd(Playmaking)))/6) %>%
    mutate(FloStrength = (Shooting +Defense +.4*Rebounding + .6 *Playmaking)/3)
  getConf <- function(x){
    h <- read_html(str_c("https://www.ncaa.com/standings/basketball-men/d1"))
    conferences <- html_nodes(h, ".standings-conference") %>% html_text
    Team<- html_nodes(h, ".standings-team") %>% html_text
    df<- data.frame(Team) %>%
      mutate(Conference = NA) 
    df$Conference[1:11]<- "AAC"
    df$Conference[12:26]<- "ACC"
    df$Conference[26:35]<- "AmEast"
    df$Conference[36:49]<- "ASun"
    df$Conference[50:64]<- "A10"
    df$Conference[65:74]<- "B12"
    df$Conference[75:85]<- "BEast"
    df$Conference[86:95]<- "BSky"
    df$Conference[96:105]<- "BSouth"
    df$Conference[106:119]<- "B10"
    df$Conference[120:130]<- "BWest"
    df$Conference[131:141]<- "CUSA"
    df$Conference[142:154]<- "Col"
    df$Conference[155:156]<- "AmEast"
    df$Conference[157:167]<- "Horiz"
    df$Conference[168:175]<- "Ivy"
    df$Conference[176:186]<- "Metro"
    df$Conference[187:198]<- "MAC"
    df$Conference[199:206]<- "MEAC"
    df$Conference[207:217]<- "MWC"
    df$Conference[218:229]<- "MoVal"
    df$Conference[230:238]<- "NE"
    df$Conference[239:248]<- "OhVal"
    df$Conference[249:260]<- "Pac12"
    df$Conference[261:270]<- "Pat"
    df$Conference[271:284]<- "SEC"
    df$Conference[285:294]<- "Sthrn"
    df$Conference[295:304]<- "Sthlnd"
    df$Conference[305:314]<- "Sum"
    df$Conference[315:328]<- "SunB"
    df$Conference[329:340]<- "SWAC"
    df$Conference[341:353]<- "WAC"
    df$Conference[354:363]<- "WCst"
    
    h <- read_html(str_c("https://www.colleyrankings.com/hcurconf.html"))
    Conference <- html_nodes(h, "th a") %>% html_text
    ConfRating<- html_nodes(h, "th~ td:nth-child(3)") %>% html_text %>% as.numeric()
    confdf<- data.frame(Conference,ConfRating)
    df <- left_join(df, confdf, by = "Conference")
    return(df) 
  }
  dfconf<- getConf(x)
  dfteam <- left_join(dfteam, dfconf, by = "Team") %>%
    mutate(Rating = FloStrength) %>%
    group_by(yearID) %>%
    mutate(Rating = (3+((Rating - mean(Rating))/sd(Rating)))/6) %>%
    ungroup() %>%
    arrange(-Rating)
  awaydf <- dfteam %>%
    select("away" = "Team", "awayFS" = "Rating")
  homedf <- dfteam %>%
    select("home" = "Team", "homeFS" = "Rating")
  ResultsDf <- left_join(ResultsDf, awaydf, by = "away")
  ResultsDf <- left_join(ResultsDf, homedf, by = "home") %>%
    mutate(awaysched = NA) %>%
    mutate(homesched = NA)
  ind <- which(is.na(ResultsDf$awayFS)==TRUE)
  ResultsDf$awayFS[ind] = .1
  ind <- which(is.na(ResultsDf$homeFS)==TRUE)
  ResultsDf$homeFS[ind] = .1
  ind <- which(ResultsDf$awayscore < ResultsDf$homescore)
  ResultsDf$awaysched[ind] = -1 * ResultsDf$PD[ind]*(1-ResultsDf$homeFS[ind])
  ResultsDf$homesched[ind] = ResultsDf$PD[ind]*(ResultsDf$awayFS[ind])
  ind <- which(ResultsDf$awayscore > ResultsDf$homescore)
  ResultsDf$homesched[ind] = -1 * ResultsDf$PD[ind]*(1-ResultsDf$awayFS[ind])
  ResultsDf$awaysched[ind] = ResultsDf$PD[ind]*(ResultsDf$homeFS[ind])
  ResultsDf <- ResultsDf %>%
    na.omit()
  awayscheddf <- ResultsDf %>%
    group_by(away) %>%
    mutate(AFS = sum(awaysched))%>%
    mutate(Agp = length(awayFS)) %>%
    slice(1) %>%
    ungroup %>%
    select("Team" =away, AFS, Agp)
  homescheddf <- ResultsDf %>%
    group_by(home) %>%
    mutate(HFS = sum(homesched))%>%
    mutate(Hgp = length(homeFS)) %>%
    slice(1) %>%
    ungroup %>%
    select("Team" =home, HFS, Hgp)
  scheddf <- right_join(awayscheddf, homescheddf, by = "Team") %>%
    mutate(AFS = ifelse(is.na(AFS)==TRUE, 0, AFS))%>%
    mutate(HFS = ifelse(is.na(HFS)==TRUE, 0, HFS))%>%
    mutate(Agp = ifelse(is.na(Agp)==TRUE, 0, Agp))%>%
    mutate(Hgp = ifelse(is.na(Hgp)==TRUE, 0, Hgp)) %>%
    na.omit %>%
    mutate(SchedRating = (AFS + HFS)/ (Agp+Hgp)) %>%
    select(Team, SchedRating) %>%
    mutate(yearID = 2022) %>%
    group_by(yearID) %>%
    mutate(SchedRating = (3+((SchedRating - mean(SchedRating))/sd(SchedRating)))/6) %>%
    ungroup
  dfteam <- left_join(dfteam, scheddf, by = "Team") %>%
    mutate(TmRating = (Rating + SchedRating)/ 2)
  dfteam <- unique(dfteam)
  dfteam <- dfteam %>% group_by(Team) %>% slice(1) %>% ungroup
  getStreaks <- function(dfteam,ResultsDf){
    sched <- ResultsDf%>%
      mutate(WTeam = ifelse(awayscore>homescore,away,home)) %>%
      select(away,home, PD, WTeam,"date"="Date") %>%
      ungroup() %>% 
      mutate(Date = NA)
    for (i in c(1:length(unique(sched$date)))) {
      i = i
      x <- unique(sched$date)[i]
      ind <- which(sched$date == x)
      sched$Date[ind] <- i
    }
    sched <- sched %>%
      select(-date)
    Ateam <- dfteam %>%
      select("away"=Team, "AwayFS" =TmRating)
    Hteam <- dfteam %>%
      select("home"=Team,"HomeFS"= TmRating)
    sched <- left_join(sched, Ateam, by = "away")
    sched<- left_join(sched, Hteam, by = "home") %>%
      mutate(ASched = 0) %>%
      mutate(HSched = 0) %>%
      mutate(AwayFS = ifelse(is.na(AwayFS)==TRUE, 0.1, AwayFS)) %>%
      mutate(HomeFS = ifelse(is.na(HomeFS)==TRUE, 0.1, HomeFS))
    ind <- which(sched$WTeam == sched$away)
    sched$ASched[ind] = sched$PD[ind] * sched$HomeFS[ind]
    sched$HSched[ind] = -1 * sched$PD[ind] * (1-sched$AwayFS[ind])
    ind <- which(sched$WTeam == sched$home)
    sched$HSched[ind] = sched$PD[ind] * sched$AwayFS[ind]
    sched$ASched[ind] =  -1 * sched$PD[ind] * (1-sched$HomeFS[ind])
    away <- sched %>%
      select("Team" = away,"Sched"= ASched, Date)
    home <- sched %>%
      select("Team" = home, "Sched" =HSched,Date)
    sched3 <- rbind(away,home) %>%
      arrange(-Date) %>%
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
      arrange(-Date) %>%
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
      arrange(-Date) %>%
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
      arrange(-Date) %>%
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
    return(lastx)
  }
  streak <- getStreaks(dfteam, ResultsDf)
  dfteam<- left_join(dfteam, streak, by = "Team")
  return(dfteam)
}
getNCAAMBModSuc <- function(x){
  ncaamb<- read.csv("/Users/seanfloersch/FloStrength/NCAAMBFloStrength/NCAAMBModDF") 
  ncaaplot <- ncaamb %>%
    mutate(day = Date) %>%
    mutate(daynum = NA) %>% 
    mutate(weeknum = NA) %>%
    mutate(AWin = ifelse(WTeam == away,1,0))
  
  ncaaplot$BookWin <- ifelse(ncaaplot$BookWin == ncaaplot$away, 1, 0)
  for (i in c(1:(length(unique(ncaaplot$day))+1))) {
    i = i
    x <- unique(ncaaplot$day)[i]
    ind <- which(ncaaplot$day == x)
    ncaaplot$daynum[ind] <- length(unique(ncaaplot$day)) -i
  }
  traindf <- ncaaplot %>% 
    filter(daynum < 3) 
  traindf<-traindf[,c(3,5:16,18:29,41)]
  testdf <- ncaaplot %>% 
    filter(daynum == 3)
  bookpred <- testdf$BookWin  
  actwin <- testdf$AWin
  games <- testdf$away
  daynum <- testdf$daynum
  testdf <- ncaaplot %>% 
    filter(daynum == 3)
  testdf<-testdf[,c(3,5:16,18:29,41)]
  
  
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
  gbmmod <- gbm(AWin~., data = traindf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 1000) 
  gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, testdf, type = "response")
  df <- data.frame(games,logpred, svmpred, rfpred, nnpred, gbmpred,bookpred, actwin,daynum) 
  compdf <- df
  
  for (i in c(4:(length(unique(ncaaplot$daynum))-1))){
    traindf <- ncaaplot %>% 
      filter(daynum < i) 
    traindf<-traindf[,c(3,5:16,18:29,41)]
    testdf <- ncaaplot %>% 
      filter(daynum == i)
    bookpred <- testdf$BookWin  
    actwin <- testdf$AWin
    games <- testdf$away
    daynum <- testdf$daynum
    testdf <- ncaaplot %>% 
      filter(daynum == i)
    testdf<-testdf[,c(3,5:16,18:29,41)]
    
    
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
    gbmmod <- gbm(AWin~., data = traindf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 100) 
    gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, testdf, type = "response")
    df <- data.frame(games,logpred, svmpred, rfpred, nnpred, gbmpred,bookpred, actwin,daynum) 
    compdf <- rbind(compdf, df) 
  }
  compdf <- compdf %>%
    mutate(svmpred = as.numeric(svmpred)) %>%
    mutate(svm = ifelse(svmpred>.5,1,0)) %>%
    mutate(log = ifelse(logpred>.5,1,0)) %>%
    mutate(nn = ifelse(nnpred>.5,1,0)) %>%
    mutate(rf = ifelse(rfpred>.5,1,0)) %>%
    mutate(gbm = ifelse(gbmpred>.5,1,0)) %>%
    mutate(Ensemble = (svm+nn+gbm+log+rf))%>%
    mutate(Ensemble = ifelse(Ensemble>2.5,1,0)) %>%
    mutate(EnsCorr = ifelse(Ensemble==actwin,1,0)) %>%
    mutate(E2 = (nn+gbm+rf))%>%
    mutate(E2 = ifelse(E2>1.5,1,0)) %>%
    mutate(E2Corr = ifelse(E2==actwin,1,0)) %>%
    mutate(LogCorr = ifelse(log==actwin,1,0)) %>%
    mutate(NNCorr = ifelse(nn==actwin,1,0)) %>%
    mutate(RFCorr = ifelse(rf==actwin,1,0)) %>%
    mutate(SVMCorr = ifelse(svm==actwin,1,0)) %>%
    mutate(GBMCorr = ifelse(gbm==actwin,1,0)) %>%
    mutate(BookCorr = ifelse(bookpred==actwin,1,0)) 
  ovrcorr <- compdf %>%
    filter(daynum ==3) %>%
    group_by(daynum) %>%
    mutate(LogCorr = mean(LogCorr)) %>%
    mutate(SVMCorr = mean(SVMCorr)) %>%
    mutate(GBMCorr = mean(GBMCorr)) %>%
    mutate(NNCorr = mean(NNCorr)) %>%
    mutate(RFCorr = mean(RFCorr)) %>%
    mutate(EnsCorr = mean(EnsCorr))%>%
    mutate(E2Corr = mean(E2Corr))%>%
    mutate(BookCorrect = mean(BookCorr)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(Day = daynum - 2) %>%
    select(Day,LogCorr,SVMCorr,GBMCorr,RFCorr,NNCorr,EnsCorr,E2Corr, BookCorrect)
  last <- max(compdf$daynum)
  for (i in c(4:last)){
    ovrcorr1 <- compdf %>%
      filter(daynum <= i& daynum>2) %>%
      mutate(LogCorr = mean(LogCorr)) %>%
      mutate(SVMCorr = mean(SVMCorr)) %>%
      mutate(GBMCorr = mean(GBMCorr)) %>%
      mutate(NNCorr = mean(NNCorr)) %>%
      mutate(RFCorr = mean(RFCorr)) %>%
      mutate(EnsCorr = mean(EnsCorr))%>%
      mutate(E2Corr = mean(E2Corr))%>%
      mutate(BookCorrect = mean(BookCorr)) %>%
      ungroup() %>%
      slice(1) %>%
      mutate(Day = i - 2) %>%
      select(Day,LogCorr,SVMCorr,GBMCorr,RFCorr,NNCorr,EnsCorr, E2Corr,BookCorrect)
    
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
    mutate(Model = "E3")
  ovr6 <- ovrcorr %>%
    select(Day,"Corr"=SVMCorr) %>%
    mutate(Model = "SVM")
  ovr7 <- ovrcorr %>%
    select(Day,"Corr"=E2Corr) %>%
    mutate(Model = "Ens")
  ovr8 <- ovrcorr %>%
    select(Day,"Corr"=BookCorrect) %>%
    mutate(Model = "Book")
  ovrcorr<- rbind(ovr1,ovr2,ovr3,ovr4,ovr5,ovr6,ovr7,ovr8)
  write_csv(ovrcorr,"/Users/seanfloersch/FloStrength/FloStrengthApp/NCAAMBModOvrSuc")
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
  write_csv(dfcomp,"/Users/seanfloersch/FloStrength/FloStrengthApp/NCAAMBModSucDF")
}
predictNCAAMBgames<- function(todgameID,masterdf){
  h <- read_html(str_c("https://www.sportsline.com/college-basketball/picks/?pickType=MONEY_LINE"))
  
  away <- html_nodes(h, ".laFSs:nth-child(1) .cfYQTQ") %>% html_text
  
  home <- html_nodes(h, ".laFSs+ .laFSs .cfYQTQ") %>% html_text
  spread <- html_nodes(h, ".odds-line .ciLHdB+ .ciLHdB") %>% html_text %>% str_remove("\\+") %>% as.numeric()
  preddf <- data.frame(away,home, spread)
  awaydf<- masterdf %>% 
    rename("away" = "Team")
  preddf1<- left_join(preddf, awaydf, by = "away")
  teams <- c("Abilene Chr."="Abilene Christian","Alcorn St." = "Alcorn","PFW"="Purdue Fort Wayne","App. St." = "App State","FDU"="Fairleigh Dickinson", "Lamar" = "Lamar University","Tenn. Tech" = "Tennessee Tech","San Diego"="San DiegoMIA", "E. Kentucky"="Eastern Ky.",
             "Ga. Tech"="Georgia Tech","La. Tech"="Louisiana Tech","McNeese St."= "McNeese","Miss Valley St."="Mississippi Val.","Mt St Mary's"="Mount St. Mary's","N. Arizona"="Northern Ariz.","C. Carolina"="Coastal Carolina","CSNorthridge"="CSUN","Cal Baptist"="California Baptist","Cal-Baker."="CSU Bakersfield","E. Michigan"="Eastern Mich.","N.J. Tech"="NJIT",
             "San Diego St"="San Diego St.","South Florida"="South Fla.","UC San Diego"="San Diego","N. Alabama"= "North Ala.","N. Illinois" = "NIU","USC"="Southern California","W. Kentucky"="Western Ky.","C. Michigan"="Central Mich.","S. Illinois"="Southern Ill.","Clev. St." = "Cleveland St.","UT-Arlington" = "UT Arlington","Charleston"="Col. of Charleston","St. Peter's"="Saint Peter's",
             "UMKC"= "Kansas City","Texas A&M-CC"="A&M-Corpus Christi","Jax. State"="Jacksonville St.", "SC State"= "South Carolina St.","Saint Mary's"="Saint Mary's (CA)","St. Fran.-NY"="St. Francis Brooklyn","CCSU"="Central Conn. St.","LMU"="LMU (CA)","UCSB"="UC Santa Barbara","So. Miss"="Southern Miss.","Miami (Fla.)" = "Miami (FL)","N. Kentucky"="Northern Ky.",
             "St. Bona."="St. Bonaventure","St. John's"="St. John's (NY)","George Wash."="George Washington", "Miss. St."="Mississippi St.","N. Carolina"="North Carolina","Army" = "Army West Point","E. Illinois"="Eastern Ill.", "Loyola Chi." = "Loyola Chicago","N. Dak. St."="North Dakota St.","S. Dak. St."="South Dakota St.","SC Upstate"= "USC Upstate","SF Austin"="SFA",
             "UNCG"="UNC Greensboro","San Fran."="San Francisco","So. Utah"="Southern Utah", "UL-Monroe"="ULM", "Va. Tech"="Virginia Tech","Incarnate Word"="UIW","N. Colorado"="Northern Colo.","LBSU"="Long Beach St.","N. Mex. St."="New Mexico St.", "Loyola-Md."= "Loyola Maryland","E. Washington", "Eastern Wash.", "Detroit"= "Detroit Mercy", "Okla. St." = "Oklahoma St.","Neb.-Omaha"="Omaha",
             "Bethune-Cook."="Bethune-Cookman","CS Fullerton"="Cal St. Fullerton","FAU"="Fla. Atlantic","Houston Chr."="Houston Christian","Ill.-Chicago"="UIC","New Hamp."="New Hampshire","Nicholls St."="Nicholls","SE Missouri St."="Southeast Mo. St.","UT-Rio Grande Valley"="UTRGV","W. Carolina"="Western Caro.","Albany"="UAlbany", "Cent. Arkansas"="Central Ark.", 
             "Colo. St."="Colorado St.", "Miami (Ohio)"="Miami (OH)", "NC Central"="N.C. Central", "UNC-Ash."="UNC Asheville", "W. Michigan"="Western Mich.", "W. Illinois"="Western Ill.","Seattle"="Seattle U","Texas So."="Texas Southern","SE Louisiana"="Southeastern La.","NC A&T"="N.C. A&T")
  ind <- which(is.na(preddf1$W)== TRUE|preddf1$away == "San Diego")
  preddf$away[ind]<- as.character(teams[preddf$away[ind]])
  preddf1<- left_join(preddf, awaydf, by = "away")%>%
    group_by(away,home) %>%
    slice(1) %>%
    ungroup
  ###
  homedf<- masterdf %>% 
    rename("home" = "Team")
  preddf1<- left_join(preddf, homedf, by = "home")
  ind <- which(is.na(preddf1$W)== TRUE|preddf1$home == "San Diego")
  preddf$home[ind]<- as.character(teams[preddf$home[ind]])
  preddf <- preddf %>%
    na.omit()
  preddf1<- left_join(preddf, awaydf, by = "away") %>%
    select(-W,-L,-yearID.x,-yearID.y,-Conference,-ConfRating)
  for (i in c(4:16)){
    colnames(preddf1)[i] = str_c("Away", colnames(preddf1)[i], sep = "")
  }
  preddf1<- left_join(preddf1, homedf, by = "home")%>%
    select(-W,-L,-yearID.x,-yearID.y,-Conference,-ConfRating)
  for (i in c(17:29)){
    colnames(preddf1)[i] = str_c("Home", colnames(preddf1)[i], sep = "")
  }
  preddf<- preddf1 %>%
    na.omit %>%
    mutate(Date = todgameID) %>%
    mutate(BookWin = ifelse(spread>0,away,home)) %>%
    mutate(predPD = -3+(AwayTmRating-HomeTmRating)*40) %>%
    mutate(FloWin = ifelse(predPD>0,away,home))
  return(preddf)
}
predModelNCAAMB <- function(ncaambpred,todgameID){
  modeldf1<- read.csv("/Users/seanfloersch/FloStrength/NCAAMBFloStrength/NCAAMBModDF")%>%
    mutate(AWin =ifelse(away==WTeam, 1, 0))%>%
    mutate(PD =ifelse(away==WTeam, PD, -1*PD)) 
  modeldf<-  modeldf1[,c(3,5:16,18:29,38)]
  modlindf <-modeldf1[,c(3,5:16,18:29,36)]
  set.seed(1212)
  logmod <- glm(AWin~., data = modeldf,family = binomial)
  logpred <- predict(logmod, newdata = ncaambpred, type = "response")
  logwin <- ifelse(logpred>.5,1,0)
  
  set.seed(1212)
  svmmod <- svm(AWin~., data = modeldf,probability = TRUE)
  svmpred <- predict(svmmod, newdata = ncaambpred, probability = TRUE)
  svmwin <- as.vector(ifelse(svmpred>.5,1,0))
  
  set.seed(1212)
  rfmod <- randomForest(AWin~., data = modeldf)
  rfpred<-as.vector(predict(rfmod, newdata = ncaambpred))
  rfwin=ifelse(rfpred>0.5,1,0)
  
  set.seed(1212)
  nnmod  <- neuralnet(AWin~., data = modeldf, hidden=1,act.fct = "logistic",linear.output = FALSE)
  nnpred<-as.vector((compute(nnmod,ncaambpred))$net.result)
  nnwin <- as.vector(ifelse(nnpred>0.5, 1, 0))
  
  set.seed(1212)
  gbmmod <- gbm(AWin~., data = modeldf,interaction.depth = 1,  distribution = "bernoulli",n.trees = 10000) 
  gbmpred <- predict(gbmmod, n.trees = gbmmod$n.trees, ncaambpred, type = "response")
  gbmwin=ifelse(gbmpred>0.5,1,0)
  
  linmod <- lm(PD~., data = modlindf)
  linpred1 <- predict(linmod, newdata = ncaambpred)
  
  predact <- ncaambpred %>%
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
    mutate(ensprob = (log+svm+gbm+nn+rf)/5)%>%
    mutate(ens = ifelse(ensprob>.5, 1, 0)) %>%
    mutate(book = ifelse(BookWin==away,1,0)) %>%
    mutate(diff = ifelse(ens == book, "no", "yes"))
  strwins <- predact %>%
    filter(diff == "yes") %>%
    mutate(Bet = ifelse(ens == 1, away, home)) %>%
    mutate(Game = str_c(away, home, sep = " @ ")) %>%
    select(Date,Game, spread,predPD, BookWin, ensprob,Bet) %>%
    mutate(ensprob = ifelse(ensprob<.5, 1-ensprob, ensprob)) %>%
    arrange(-ensprob)
  sprwins <-predact %>%
    filter(diff == "no") %>%
    mutate(spread = abs(spread)) %>%
    mutate(spread = ifelse(away == BookWin, spread, -1 * spread)) %>%
    mutate(spdiff = spread-predPD) %>%
    filter(abs(spdiff) > 2)%>%
    mutate(spread = abs(spread)) %>%
    mutate(Bet = ifelse((spdiff>0&BookWin==home)|(spdiff<0&BookWin==away), -1 * spread, spread)) %>%
    mutate(Game = str_c(away, home, sep = " @ ")) %>%
    select(Date,Game, spread, predPD, BookWin, "ensprob"=spdiff, Bet) %>%
    mutate(ensprob= abs(ensprob))%>%
    arrange(-ensprob)
  betsdf <- rbind(strwins, sprwins)
  return(betsdf)
}
updateNCAAMBResults <- function(yestdate){
  sosdf <-read.csv("/Users/seanfloersch/FloStrength/NCAAMBFloStrength/NCAAMBResultsDF")
  year <- str_extract(yestdate,"\\d{1,4}")
  md <- str_remove(yestdate,"\\d{1,4}")
  month <- str_extract(md,"\\d{1,2}")
  day <- str_remove(md,"\\d{1,2}")
  
  h <- read_html(str_c("https://www.ncaa.com/scoreboard/basketball-men/d1/", year,"/",month,"/",day,"/all-conf"))
  away <- html_nodes(h, "#scoreboardGames li:nth-child(1) .gamePod-game-team-name") %>% html_text
  home <- html_nodes(h, "#scoreboardGames li:nth-child(2) .gamePod-game-team-name") %>% html_text
  awayscore <- html_nodes(h, "#scoreboardGames li:nth-child(1) .gamePod-game-team-score") %>% html_text %>% as.numeric()
  homescore <- html_nodes(h, "#scoreboardGames li:nth-child(2) .gamePod-game-team-score") %>% html_text%>% as.numeric()
  sosdf1 <- data.frame(away,home,awayscore,homescore) %>%
    mutate(PD = abs(awayscore - homescore)) %>%
    mutate(Date = yestdate)
  sosdf <- rbind(sosdf, sosdf1)
  sosdf <- unique(sosdf)
  return(sosdf)
}
updateNCAAMBModDF <- function(NCAAMBResultsDF,yestgameID){
  moddf<- NCAAMBResultsDF %>%
    filter(Date == yestgameID) %>%
    mutate(Date = as.character(Date)) %>%
    mutate(gamenum = c(1:length(Date)))
  teama <- moddf %>%
    select(-home) %>%
    rename("team" = away)
  teamh <- moddf %>%
    select(-away) %>%
    rename("team" = home)
  moddf <- rbind(teama,teamh) %>%
    group_by(gamenum)   %>%
    arrange(team) %>%
    mutate(gameID = str_c(team[1],team[2])) %>%
    slice(1) %>%
    ungroup() %>%
    select(-team)
  yestpreds <- read.csv("/Users/seanfloersch/FloStrength/NCAAMBFloStrength/NCAAMByestpred")%>%
    mutate(Date = as.character(Date))%>%
    mutate(gamenum = c(1:length(Date)))
  teama <- yestpreds %>%
    select(-home) %>%
    rename("team" = away)
  teamh <- yestpreds %>%
    select(-away) %>%
    rename("team" = home)
  yestpreds <- rbind(teama,teamh) %>%
    group_by(gamenum)%>%
    mutate(away = team[1]) %>%
    mutate(home = team[2]) %>%
    arrange(team) %>%
    mutate(gameID = str_c(team[1],team[2])) %>%
    slice(1) %>%
    ungroup() %>%
    select(-team)
  
  df <- left_join(yestpreds, moddf, by = c("gameID","Date"))%>%
    mutate(WTeam = ifelse(awayscore-homescore>0, away, home)) %>%
    na.omit() %>%
    select(-gamenum.x,-gamenum.y,-gameID)
  dfcomp<-read.csv("/Users/seanfloersch/FloStrength/NCAAMBFloStrength/NCAAMBModDF") %>%
    mutate(WTeam = ifelse(awayscore-homescore>0, away, home))
  df <- rbind(df, dfcomp) %>% na.omit
  df <- unique(df) %>%
    relocate(away,home)
  return(df)
}
updateBetDf <- function(NCAAMBModDF,yestgameID){
  moddf<- NCAAMBModDF %>%
    filter(Date == yestgameID) %>%
    mutate(Date = as.character(Date)) %>%
    select(Date, BookWin, awayscore, homescore, PD,WTeam)
  yestpreds <- read.csv("/Users/seanfloersch/FloStrength/NCAAMBFloStrength/games2watchyest")%>%
    mutate(Date = as.character(Date)) %>%
    mutate(BookWin = as.character(BookWin))
  df <- left_join(yestpreds, moddf, by = c("BookWin","Date"))%>%
    mutate(actsp = ifelse((BookWin==WTeam & PD>abs(spread)), -1*abs(spread), abs(spread))) %>%
    mutate(bettype = as.numeric(Bet))
  dfspread <- df %>%
    na.omit() %>%
    mutate(BetCorr = ifelse(Bet==actsp, 1,0))%>%
    mutate(BetEarn = ifelse(BetCorr==1, .91,-1)) %>%
    mutate(bettype= "Spread") 
  dfstraight <- df %>%
    filter(is.na(bettype)==TRUE) %>%
    mutate(bettype = "Straight") %>%
    na.omit() %>%
    mutate(BetCorr = ifelse(WTeam==Bet, 1,0))%>%
    mutate(spread = abs(spread)) %>%
    mutate(BetEarn = ifelse(BetCorr==1, (exp(spread)^.125),-1)) 
  df<- rbind(dfstraight,dfspread)%>% 
    group_by(Date) %>%
    mutate(Earn = round(sum(BetEarn),2)) %>%
    slice(1) %>%
    ungroup() %>%
    select("Day"="Date", Earn) %>%
    mutate(Net = ifelse(Earn>0, "pos","neg")) %>%
    mutate(Sport = "NCAAMB") %>%
    na.omit()
  dfcomp <- read.csv("/Users/seanfloersch/FloStrength/NCAAMBFloStrength/BetDFncaamb")
  df <- rbind(df, dfcomp) 
  df <- unique(df)
  return(df)
}