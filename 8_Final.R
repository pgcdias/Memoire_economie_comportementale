rm(list=ls())
library(httr)

# run : 
{
  API_Key <- "x"
}

# functions 
{
  lastMatch <- function(name,count=1)
  {
    # On définit puuid à partir du nom
    x1 <- paste0("https://euw1.api.riotgames.com/lol/summoner/v4/summoners/by-name/",name,"?api_key=",API_Key)
    puuid <- content(GET(x1))$puuid
    # On définit la liste des matchId des parties ranked précédentes :
    nURL <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/",
                   puuid,"/ids?queue=420&start=0&count=",count,"&api_key=",API_Key)
    x2 <- unlist(content(GET(nURL)))
    return(x2)
  }
  playersId <- function (match_id)
  {
    x <- content(
      GET(paste0("https://europe.api.riotgames.com/lol/match/v5/matches/",
                 matchId,
                 "?api_key=",
                 API_Key)))
    pId <- unlist(x$metadata$participants[1:10])
    return (pId)
    
  }
  variables <- function (match_id)
  {
    url <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/",
                  match_id,
                  "/timeline",
                  "?api_key=",
                  API_Key)
    data <- content(GET(url))
    
    # Gold
    {
      TGdf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
      colnames(TGdf) <- paste0("t",(1:length(data$info$frames)))
      rownames(TGdf) <- paste0("j",(1:10))
      for (d in (1:length(data$info$frames)))
      {
        for (j in (1:10))
        {
          TGdf[j,d] <- data$info$frames[[d]]$participantFrames[[j]]$totalGold
        }
      }
    }
    
    # Farm
    {
      TFdf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
      colnames(TFdf) <- paste0("t",(1:length(data$info$frames)))
      rownames(TFdf) <- paste0("j",(1:10))
      for (d in (1:length(data$info$frames)))
      {
        for (j in (1:10))
        {
          TFdf[j,d] <- data$info$frames[[d]]$participantFrames[[j]]$minionsKilled+
            data$info$frames[[d]]$participantFrames[[j]]$jungleMinionsKilled
        }
      }
      
    }
    
    # Kills 
    {
      TKdf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
      colnames(TKdf) <- paste0("t",(1:length(data$info$frames)))
      rownames(TKdf) <- paste0("j",(1:10))
      counter=numeric(10)
      
      for (t in (1:length(data$info$frames)))
      {
        for (k in (1:length(data$info$frames[[t]]$events)))
        {
          if (data$info$frames[[t]]$events[[k]]$type=="CHAMPION_KILL")
          {
            a <- data$info$frames[[t]]$events[[k]]$killerId
            counter[a] <- counter[a]+1
          }
        }
        TKdf[,t] <- counter
      }
      
    }
    
    # Assistances
    {
      TAdf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
      colnames(TAdf) <- paste0("t",(1:length(data$info$frames)))
      rownames(TAdf) <- paste0("j",(1:10))
      counter=numeric(10)
      
      for (t in (1:length(data$info$frames)))
      {
        for (k in (1:length(data$info$frames[[t]]$events)))
        {
          if (data$info$frames[[t]]$events[[k]]$type=="CHAMPION_KILL")
          {
            a <- data$info$frames[[t]]$events[[k]]$assistingParticipantIds
            for (z in (1:length(a)))
            {
              counter[a[[z]]] <- counter[a[[z]]]+1
            }
          }
        }
        TAdf[,t] <- counter
      }
    }
    
    result <- list(TGdf,TFdf,TKdf,TAdf)
    return (result)
    
  }
  timeline <- function(match_id)
  {
    url <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/",
                  match_id,
                  "/timeline",
                  "?api_key=",
                  API_Key)
    data <- content(GET(url))
    return(data)
  }
}

# Run : 
{
  id <- lastMatch("hardcorepgcd")
  V <- variables(id)
  TGdf <- V[[1]]
  TFdf <- V[[2]]
  TKAdf <- V[[3]]+V[[4]]
  fit <- list(NA)
  for (d in (1:length(TGdf)))
  {
    fit[[d]] <- lm(TGdf[,d] ~ TFdf[,d] + TKAdf[,d])
  }
  data <- timeline(lastMatch("hardcorepgcd"))
  
}

# Farm 
{
  # F par période
  {
    Fd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(Fd) <- paste0("t",(1:length(data$info$frames)))
    rownames(Fd) <- paste0("j",(1:10))
    
    for (j in (1:10))
    {
      for (d in (1:length(data$info$frames)))
      {
        z <- d-1
        if (z<1)
        {
          Fd[j,d] <- TFdf[j,d]
        }
        if (z>=1)
        {
          Fd[j,d] <-
            TFdf[j,d] - 
            TFdf[j,d-1]
        }
      }
    } 
  } 
  # dataframe Fd 
  
  # PO de Farm cumulées
  {
    # POF <- data.frame(matrix(NA, nrow = 10, ncol = length(data$info$frames)))
    # colnames(POF) <- paste0("t",(1:length(data$info$frames)))
    # rownames(POF) <- paste0("j",(1:10))
    
    Gold_per_creep <- numeric(0)
    for (i in (1:length(fit)))
    {
      Gold_per_creep[i] <- as.numeric(fit[[i]]$coefficients[2])
    }
    Gold_per_creep[is.na(Gold_per_creep)] <- 0
    
    # for (d in (1:length(TGdf)))
    # {
    #   for (j in (1:10))
    #   {
    #     POF[j,d] <- TFdf[j,d]*Gold_per_creep[d]
    #   }
  }
  # list Gold_per_creep (à chaque période)
  
  # PO empiriques de Farm par période
  {
    POFd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(POFd) <- paste0("t",(1:length(data$info$frames)))
    rownames(POFd) <- paste0("j",(1:10))
    
    for (d in (1:length(data$info$frames)))
    {
      for (j in 1:10)
      {
        POFd[j,d] <- Gold_per_creep[d]*Fd[j,d]
      }
    }
  }
  # dataframe POFd
}

# Combat
{
  # KA par période 
  {
    KAd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(KAd) <- paste0("t",(1:length(data$info$frames)))
    rownames(KAd) <- paste0("j",(1:10))
    
    for (j in (1:10))
    {
      for (d in (1:length(TKAdf)))
      {
        z <- d-1
        if (z<1)
        {
          KAd[j,d] <- TKAdf[j,d]
        }
        if (z>=1)
        {
          KAd[j,d] <-
            TKAdf[j,d] - 
            TKAdf[j,d-1]
        }
      }
    }  
  }
  # dataframe KAd

  # PO de combat cumulées
  {
    # POka <- data.frame(matrix(NA, nrow = 10, ncol = length(TGdf)))
    # colnames(POka) <- paste0("t",(1:length(TGdf)))
    # rownames(POka) <- paste0("j",(1:10))
    
    Gold_per_ka <- numeric(0)
    for (i in (1:length(fit)))
    {
      Gold_per_ka[i] <- as.numeric(fit[[i]]$coefficients[3])
    }
    Gold_per_ka[is.na(Gold_per_ka)] <- 0
    
    # for (d in (1:length(TGdf)))
    # {
    #   for (j in (1:10))
    #   {
    #     POka[j,d] <- TKAdf[j,d]*Gold_per_ka[d]
    #   }
    # }
  }
  # list Gold_per_ka (par période)
  
  # PO empiriques de combat par période
  {
    POKAd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(POKAd) <- paste0("t",(1:length(data$info$frames)))
    rownames(POKAd) <- paste0("j",(1:10))
    
    Gold_per_ka <- numeric(0)
    for (i in (1:length(fit)))
    {
      Gold_per_ka[i] <- as.numeric(fit[[i]]$coefficients[3])
    }
    Gold_per_ka[is.na(Gold_per_ka)] <- 0
    
    for (d in (1:length(Gold_per_ka)))
    {
      for (j in 1:10)
      {
        POKAd[j,d] <- Gold_per_ka[d]*KAd[j,d]
      }
    }
  }
  # dataframe POKAd
}

# Fonction d'utilité par période : 
{
  Ud <- data.frame(matrix(NA, nrow = 10, ncol = length(data$info$frames)))
  colnames(Ud) <- paste0("t",(1:length(data$info$frames)))
  rownames(Ud) <- paste0("j",(1:10))
  alpha=2
  beta=2
  Ud = alpha*sqrt(POFd)+beta*sqrt(POKAd)
}
# Fonction :
Ud <- function (alpha=2,beta=2)
{
  U <- data.frame(matrix(NA, nrow = 10, ncol = length(data$info$frames)))
  colnames(U) <- paste0("t",(1:length(data$info$frames)))
  rownames(U) <- paste0("j",(1:10))
  
  U = alpha*sqrt(POFd)+beta*sqrt(POKAd)
  
  return(U)
}

# Total Damage Done to Champions par période 
{
  TDDtCpd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
  colnames(TDDtCpd) <- paste0("t",(1:length(data$info$frames)))
  rownames(TDDtCpd) <- paste0("j",(1:10))
  
  for (d in (1:length(data$info$frames)))
  {
    for (j in (1:10))
    {
      z <- d -1
      if (z <1)
      {
        TDDtCpd[j,d] <- data$info$frames[[d]]$participantFrames[[j]]$damageStats$totalDamageDoneToChampions
      }
      if (z>=1)
      {
        TDDtCpd[j,d] <-
          data$info$frames[[d]]$participantFrames[[j]]$damageStats$totalDamageDoneToChampions -
          data$info$frames[[z]]$participantFrames[[j]]$damageStats$totalDamageDoneToChampions
      }
    }
  }
}
# TDDtCpd

# Total Damage Done par période : 
{
  TDDpd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
  colnames(TDDpd) <- paste0("t",(1:length(data$info$frames)))
  rownames(TDDpd) <- paste0("j",(1:10))
  
  for (d in (1:length(data$info$frames)))
  {
    for (j in (1:10))
    {
      z <- d -1
      if (z <1)
      {
        TDDpd[j,d] <- data$info$frames[[d]]$participantFrames[[j]]$damageStats$totalDamageDone
      }
      if (z>=1)
      {
        TDDpd[j,d] <-
          data$info$frames[[d]]$participantFrames[[j]]$damageStats$totalDamageDone -
          data$info$frames[[z]]$participantFrames[[j]]$damageStats$totalDamageDone
      }
    }
  }
}
# TDDpd 

# On observe quelques rares anomalies, où TDDtCpd>TDDpd.

# "Temps" empiriques, observés
{
  tOka <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
  colnames(tOka) <- paste0("t",(1:length(data$info$frames)))
  rownames(tOka) <- paste0("j",(1:10))
  for (d in (1:length(data$info$frames)))
  {
    for (j in (1:10))
    {
      if (TDDpd[j,d]>0)
      {
        tOka[j,d] <- TDDtCpd[j,d]/TDDpd[j,d]
      }
      if (TDDpd[j,d]==0)
      {
        tOka[j,d] <- 0
      }
    }
    
  }
  
  tOf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
  colnames(tOf) <- paste0("t",(1:length(data$info$frames)))
  rownames(tOf) <- paste0("j",(1:10))
  tOf <- 1-tOka
}
# t0f & t0ka

# Productivité Farming
{
  nFd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
  colnames(nFd) <- paste0("t",(1:length(data$info$frames)))
  rownames(nFd) <- paste0("j",(1:10))
  
  for (d in (1:length(data$info$frames)))
  {
    for (j in (1:10))
    {
      if (tOf[j,d]>0)
      {
        if(POFd[j,d]==0|is.na(POFd[j,d])|is.nan(POFd[j,d]))
        {
          nFd[j,d] <- 0
        }
        nFd[j,d] <- (POFd[j,d])/tOf[j,d]
      }
      if (tOf[j,d]==0)
      {
        nFd[j,d] <- 0
      }
    }
  }
  nFd[is.na(nFd)] <- 0
}
# nFd

# Productivité Combat
{
  nKAd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
  colnames(nKAd) <- paste0("t",(1:length(data$info$frames)))
  rownames(nKAd) <- paste0("j",(1:10))
  
  for (d in (1:length(data$info$frames)))
  {
    for (j in (1:10))
    {
      if (tOka[j,d]>0)
      {
        if(POKAd[j,d]==0)
        {
          nKAd[j,d] <- 0
        }
        nKAd[j,d] <- (POKAd[j,d])/tOka[j,d]
      }
      if (tOka[j,d]==0)
      {
        nKAd[j,d] <- 0
      }
    }
  }
}
# nKAd

# Valeur tf prédite par le modèle rationnel
{
  tTf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
  colnames(tTf) <- paste0("t",(1:length(data$info$frames)))
  rownames(tTf) <- paste0("j",(1:10))
  
  tTka <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
  colnames(tTka) <- paste0("t",(1:length(data$info$frames)))
  rownames(tTka) <- paste0("j",(1:10))
  
  alpha <- 2
  beta <- 3
  
  tTf = (alpha^(2)*nFd)/(beta^(2)*nKAd+alpha^(2)*nFd)
  tTka = (beta^(2)*nKAd)/(beta^(2)*nKAd+alpha^(2)*nFd)
}

# Tester la fiabilité de notre modèle
{
  plot(unlist(nFd[2,]),unlist(tTf[2,]),
       xlab="Productivité du Farming",
       ylab="Temps théorique investi dans le Farming")
  
  plot(unlist(nFd[2,]),unlist(tOf[2,]),
       xlab="Productivité du Farming",
       ylab="Temps observé investi dans le Farming")
  
  plot((1:35),unlist(nFd[2,]), type="l",
       xlab="Temps dans la partie",
       ylab="Productivité moyenne du farming")
  
  plot((1:35),unlist(tTf[4,]),
       xlab="Temps dans la partie",
       ylab="Temps théorique investi dans le farming")
  
  plot(
    unlist(tOf[2,]),unlist(tTf[2,]),
       xlab="Temps empirique investi dans le Farming",
       ylab="Temps théorique investi dans le Farming",
    type='p')
  abline(lm(unlist(tOf[2,]) ~ unlist(tTf[2,])))
  
  lm(unlist(tOf[2,]) ~ unlist(tTf[2,]))$coefficients
  
  mean(na.omit(unlist(tTf[2,])))
  mean(na.omit(unlist(tTka[2,])))
  mean(na.omit(unlist(tOf[2,])))
  mean(na.omit(unlist(tOka[2,])))
  
  median(na.omit(unlist(tTf[2,])))
  median(na.omit(unlist(tTka[2,])))
  median(na.omit(unlist(tOf[2,])))
  median(na.omit(unlist(tOka[2,])))
  
  sd(na.omit(unlist(tTf[2,])))
  sd(na.omit(unlist(tTka[2,])))
  sd(na.omit(unlist(tOf[2,])))
  sd(na.omit(unlist(tOka[2,])))
  
  t.test(na.omit(unlist(tTf[2,])))
  t.test(na.omit(unlist(tOf[2,])))
}

# Fonction pour obtenir les valeurs théoriques et empiriques 
# d'un match : 
times <- function(match_id,alpha=2,beta=2)
{
  url <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/",
                match_id,
                "/timeline",
                "?api_key=",
                API_Key)
  data <- content(GET(url))
  
  # Gold cumulé
  # TGdf
  {
    TGdf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(TGdf) <- paste0("t",(1:length(data$info$frames)))
    rownames(TGdf) <- paste0("j",(1:10))
    for (d in (1:length(data$info$frames)))
    {
      for (j in (1:10))
      {
        TGdf[j,d] <- data$info$frames[[d]]$participantFrames[[j]]$totalGold
      }
    }
  }
  
  # Farm cumulé
  # TFdf
  {
    TFdf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(TFdf) <- paste0("t",(1:length(data$info$frames)))
    rownames(TFdf) <- paste0("j",(1:10))
    for (d in (1:length(data$info$frames)))
    {
      for (j in (1:10))
      {
        TFdf[j,d] <- data$info$frames[[d]]$participantFrames[[j]]$minionsKilled+
          data$info$frames[[d]]$participantFrames[[j]]$jungleMinionsKilled
      }
    }
    
  }
  
  # Kills cumulé
  # TKdf
  {
    TKdf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(TKdf) <- paste0("t",(1:length(data$info$frames)))
    rownames(TKdf) <- paste0("j",(1:10))
    counter=numeric(10)
    
    for (t in (1:length(data$info$frames)))
    {
      for (k in (1:length(data$info$frames[[t]]$events)))
      {
        if (data$info$frames[[t]]$events[[k]]$type=="CHAMPION_KILL")
        {
          a <- data$info$frames[[t]]$events[[k]]$killerId
          counter[a] <- counter[a]+1
        }
      }
      TKdf[,t] <- counter
    }
    
  }
  
  # Assistances cumulé
  # TAdf
  {
    TAdf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(TAdf) <- paste0("t",(1:length(data$info$frames)))
    rownames(TAdf) <- paste0("j",(1:10))
    counter=numeric(10)
    
    for (t in (1:length(data$info$frames)))
    {
      for (k in (1:length(data$info$frames[[t]]$events)))
      {
        if (data$info$frames[[t]]$events[[k]]$type=="CHAMPION_KILL")
        {
          a <- data$info$frames[[t]]$events[[k]]$assistingParticipantIds
          for (z in (1:length(a)))
          {
            counter[a[[z]]] <- counter[a[[z]]]+1
          }
        }
      }
      TAdf[,t] <- counter
    }
  }
  
  TKAdf <- TKdf+TAdf
  
  fit <- list(NA)
  for (d in (1:length(TGdf)))
  {
    fit[[d]] <- lm(TGdf[,d] ~ TFdf[,d] + TKAdf[,d])
  }
  
  # Gold du farm
  {
    # Farm par période
    {
      Fd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
      colnames(Fd) <- paste0("t",(1:length(data$info$frames)))
      rownames(Fd) <- paste0("j",(1:10))
      
      for (j in (1:10))
      {
        for (d in (1:length(data$info$frames)))
        {
          z <- d-1
          if (z<1)
          {
            Fd[j,d] <- TFdf[j,d]
          }
          if (z>=1)
          {
            Fd[j,d] <-
              TFdf[j,d] - 
              TFdf[j,d-1]
          }
        }
      } 
    } 
    # dataframe Fd 
    
    # PO de Farm cumulées
    {
      # POF <- data.frame(matrix(NA, nrow = 10, ncol = length(data$info$frames)))
      # colnames(POF) <- paste0("t",(1:length(data$info$frames)))
      # rownames(POF) <- paste0("j",(1:10))
      
      Gold_per_creep <- numeric(0)
      for (i in (1:length(fit)))
      {
        Gold_per_creep[i] <- as.numeric(fit[[i]]$coefficients[2])
      }
      Gold_per_creep[is.na(Gold_per_creep)] <- 0
      
      # for (d in (1:length(TGdf)))
      # {
      #   for (j in (1:10))
      #   {
      #     POF[j,d] <- TFdf[j,d]*Gold_per_creep[d]
      #   }
    }
    # list Gold_per_creep (à chaque période)
    
    # PO empiriques de Farm par période
    {
      POFd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
      colnames(POFd) <- paste0("t",(1:length(data$info$frames)))
      rownames(POFd) <- paste0("j",(1:10))
      
      for (d in (1:length(data$info$frames)))
      {
        for (j in 1:10)
        {
          POFd[j,d] <- Gold_per_creep[d]*Fd[j,d]
        }
      }
    }
    # dataframe POFd
  }
  
  # Gold du combat
  {
    # KA par période 
    {
      KAd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
      colnames(KAd) <- paste0("t",(1:length(data$info$frames)))
      rownames(KAd) <- paste0("j",(1:10))
      
      for (j in (1:10))
      {
        for (d in (1:length(TKAdf)))
        {
          z <- d-1
          if (z<1)
          {
            KAd[j,d] <- TKAdf[j,d]
          }
          if (z>=1)
          {
            KAd[j,d] <-
              TKAdf[j,d] - 
              TKAdf[j,d-1]
          }
        }
      }  
    }
    # dataframe KAd
    
    # PO de combat cumulées
    {
      # POka <- data.frame(matrix(NA, nrow = 10, ncol = length(TGdf)))
      # colnames(POka) <- paste0("t",(1:length(TGdf)))
      # rownames(POka) <- paste0("j",(1:10))
      
      Gold_per_ka <- numeric(0)
      for (i in (1:length(fit)))
      {
        Gold_per_ka[i] <- as.numeric(fit[[i]]$coefficients[3])
      }
      Gold_per_ka[is.na(Gold_per_ka)] <- 0
      
      # for (d in (1:length(TGdf)))
      # {
      #   for (j in (1:10))
      #   {
      #     POka[j,d] <- TKAdf[j,d]*Gold_per_ka[d]
      #   }
      # }
    }
    # list Gold_per_ka (par période)
    
    # PO empiriques de combat par période
    {
      POKAd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
      colnames(POKAd) <- paste0("t",(1:length(data$info$frames)))
      rownames(POKAd) <- paste0("j",(1:10))
      
      Gold_per_ka <- numeric(0)
      for (i in (1:length(fit)))
      {
        Gold_per_ka[i] <- as.numeric(fit[[i]]$coefficients[3])
      }
      Gold_per_ka[is.na(Gold_per_ka)] <- 0
      
      for (d in (1:length(Gold_per_ka)))
      {
        for (j in 1:10)
        {
          POKAd[j,d] <- Gold_per_ka[d]*KAd[j,d]
        }
      }
    }
    # dataframe POKAd
  }
  
  # Total Damage Done to Champions par période 
  {
    TDDtCpd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(TDDtCpd) <- paste0("t",(1:length(data$info$frames)))
    rownames(TDDtCpd) <- paste0("j",(1:10))
    
    for (d in (1:length(data$info$frames)))
    {
      for (j in (1:10))
      {
        z <- d -1
        if (z <1)
        {
          TDDtCpd[j,d] <- data$info$frames[[d]]$participantFrames[[j]]$damageStats$totalDamageDoneToChampions
        }
        if (z>=1)
        {
          TDDtCpd[j,d] <-
            data$info$frames[[d]]$participantFrames[[j]]$damageStats$totalDamageDoneToChampions -
            data$info$frames[[z]]$participantFrames[[j]]$damageStats$totalDamageDoneToChampions
        }
      }
    }
  }
  # TDDtCpd
  
  # Total Damage Done par période : 
  # TDDpd 
  {
    TDDpd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(TDDpd) <- paste0("t",(1:length(data$info$frames)))
    rownames(TDDpd) <- paste0("j",(1:10))
    
    for (d in (1:length(data$info$frames)))
    {
      for (j in (1:10))
      {
        z <- d -1
        if (z <1)
        {
          TDDpd[j,d] <- data$info$frames[[d]]$participantFrames[[j]]$damageStats$totalDamageDone
        }
        if (z>=1)
        {
          TDDpd[j,d] <-
            data$info$frames[[d]]$participantFrames[[j]]$damageStats$totalDamageDone -
            data$info$frames[[z]]$participantFrames[[j]]$damageStats$totalDamageDone
        }
      }
    }
  }
  
  # "Temps" empiriques, observés
  # t0f & t0ka
  {
    tOka <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(tOka) <- paste0("t",(1:length(data$info$frames)))
    rownames(tOka) <- paste0("j",(1:10))
    for (d in (1:length(data$info$frames)))
    {
      for (j in (1:10))
      {
        if (TDDpd[j,d]>0)
        {
          tOka[j,d] <- TDDtCpd[j,d]/TDDpd[j,d]
        }
        if (TDDpd[j,d]==0)
        {
          tOka[j,d] <- 0
        }
      }
      
    }
    
    tOf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(tOf) <- paste0("t",(1:length(data$info$frames)))
    rownames(tOf) <- paste0("j",(1:10))
    tOf <- 1-tOka
  }
  
  # Productivité Farming
  # nFd
  {
    nFd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(nFd) <- paste0("t",(1:length(data$info$frames)))
    rownames(nFd) <- paste0("j",(1:10))
    
    for (d in (1:length(data$info$frames)))
    {
      for (j in (1:10))
      {
        if (tOf[j,d]>0)
        {
          if(POFd[j,d]==0|is.na(POFd[j,d])|is.nan(POFd[j,d]))
          {
            nFd[j,d] <- 0
          }
          nFd[j,d] <- (POFd[j,d])/tOf[j,d]
        }
        if (tOf[j,d]==0)
        {
          nFd[j,d] <- 0
        }
      }
    }
    nFd[is.na(nFd)] <- 0
  }
  
  # Productivité Combat
  # nKAd
  {
    nKAd <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(nKAd) <- paste0("t",(1:length(data$info$frames)))
    rownames(nKAd) <- paste0("j",(1:10))
    
    for (d in (1:length(data$info$frames)))
    {
      for (j in (1:10))
      {
        if (tOka[j,d]>0)
        {
          if(isTRUE(POKAd[j,d]==0))
          {
            nKAd[j,d] <- 0
          }
          nKAd[j,d] <- (POKAd[j,d])/tOka[j,d]
        }
        if (tOka[j,d]==0)
        {
          nKAd[j,d] <- 0
        }
      }
    }
  }

  # Valeur tf prédite par le modèle rationnel
  # tTf & tTka
  {
    tTf <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(tTf) <- paste0("t",(1:length(data$info$frames)))
    rownames(tTf) <- paste0("j",(1:10))
    
    tTka <- data.frame(matrix(NA,nrow=10,ncol=length(data$info$frames)))
    colnames(tTka) <- paste0("t",(1:length(data$info$frames)))
    rownames(tTka) <- paste0("j",(1:10))
    
    tTf = (alpha^(2)*nFd)/(beta^(2)*nKAd+alpha^(2)*nFd)
    tTka = (beta^(2)*nKAd)/(beta^(2)*nKAd+alpha^(2)*nFd)
  }

  result <- list("temps théorique du farm"=tTf,
                 "temps théorique du combat"=tTka,
                 "temps empirique du farm"=tOf,
                 "temps empirique du combat"=tOka)
  
  return(result)
}


