rm(list=ls())
library(httr)

# run : 
{
  API_Key <- "X"
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

# On obtient deux ensembles de valeurs : 
# les valeurss théoriques tTf et tTka
# les valeurs empiriques t0f et t0ka

# Pour les comparer, on mesure
# la moyenne 
# la médiane 
# l'écart-type 
# la variation en % entre ces valeurs
# la moyenne de cette variation 
# l'écart-type de cette variation

{
for (d in (1:length(t0f)))
{
  for (j in (1:10))
  {
    
  }
}
}

# Par exemple :
id <- lastMatch("MagiFelix5")
# https://www.leagueofgraphs.com/match/euw/5901719759#participant9

time <- times(id,20,5)

# Run 
{
  tTf <- time[[1]]
  tTka <- time[[2]]
  tOf <- time[[3]]
  tOka <- time[[4]]
  
  mean_tTf <- numeric(0)
  mean_tOf <- numeric(0)
  diff_tTf_tOf <- numeric(0)
  sd_tTf <- numeric(0)
  sd_tOf <- numeric(0)
  
  sd_diff <- numeric(0)
  
  for (j in (1:10))
  {
    mean_tTf[j] <- mean(na.omit(unlist(tTf[j,])))
    sd_tTf[j] <- sd(na.omit(unlist(tTf[j,])))
    mean_tOf[j] <- mean(na.omit(unlist(tOf[j,])))
    sd_tOf[j] <- sd(na.omit(unlist(tOf[j,])))
    diff_tTf_tOf[j] <- 100*mean_tTf[j]/mean_tOf[j]
  }
  
  test <- data.frame(mean_tTf,sd_tTf,mean_tOf,sd_tOf,diff_tTf_tOf)
  colnames(test) <- c("theorical mean","theoretical sd", 
                      "empirical mean", "empirical sd",
                      "theoretical/empirical difference")
  
  
}

a <- mean_diff <- mean(diff_tTf_tOf)
b <- sd_diff <- sd(diff_tTf_tOf)




