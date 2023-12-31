#simulate the draft process 10,000 times with a loop
set.seed(872436)           # Set seed
for(it in 1:10000){
  #get players and league
  #####
  print(it)
  players <- rbind(qbsw, wrs, rbs, tes)
  players <- players %>% select(player_id, player_name, pos, rank, games, fantasy_points_ppr, touches, ppt) %>% 
    filter(games > 5)
  players$str_id <- as.character(players$player_id)
  #players$ppt[players$player_name == "M.Gordon"] <- 36.9165191
  #players$ppt <- (players$ppt/100)*13
  #####players#####
  #player1
  np1 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(np1) <- c('id', 'name', 'pos', 'FWP', 'rank')
  
  #player2
  np2 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(np2) <- c('id', 'name', 'pos', 'FWP', 'rank')
  
  #player3
  np3 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(np3) <- c('id', 'name', 'pos', 'FWP', 'rank')
  
  #player4
  np4 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(np4) <- c('id', 'name', 'pos', 'FWP', 'rank')
  
  #player5
  np5 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(np5) <- c('id', 'name', 'pos', 'FWP', 'rank')
  
  #player6
  np6 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(np6) <- c('id', 'name', 'pos', 'FWP', 'rank')
  
  #player7
  np7 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(np7) <- c('id', 'name', 'pos', 'FWP', 'rank')
  
  #player8
  np8 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(np8) <- c('id', 'name', 'pos', 'FWP', 'rank')
  
  #player9
  np9 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(np9) <- c('id', 'name', 'pos', 'FWP', 'rank')
  
  #player10
  np10 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(np10) <- c('id', 'name', 'pos', 'FWP', 'rank')
  
  league <- c("np1", "np2", "np3", "np4", "np5", "np6", "np7", "np8", "np9", "np10")
  league <- sample(league) 
  #####loop##### use a double for loop since we want to execute the draft as a 'snake draft'
  for(i in 1:8){
    r = i
    #selecting players in this loop using AI functions
    for(team in league){
      if(team == "np1"){
        
        x <- normal_AI_1(nrow(np1[np1$pos == 'RB',]), nrow(np1[np1$pos == 'WR',]), 
                             nrow(np1[np1$pos == 'QB',]), nrow(np1[np1$pos == 'TE',]),
                             np1, players)
        np1[nrow(np1)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np2"){

        x <- normal_AI_2(nrow(np2[np2$pos == 'RB',]), nrow(np2[np2$pos == 'WR',]), 
                             nrow(np2[np2$pos == 'QB',]), nrow(np2[np2$pos == 'TE',]),
                             np2, players)
        np2[nrow(np2)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np3"){
        x <- normal_AI_3(nrow(np3[np3$pos == 'RB',]), nrow(np3[np3$pos == 'WR',]), 
                             nrow(np3[np3$pos == 'QB',]), nrow(np3[np3$pos == 'TE',]),
                             np3, players)
        np3[nrow(np3)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np4"){
        x <- normal_AI_4(nrow(np4[np4$pos == 'RB',]), nrow(np4[np4$pos == 'WR',]), 
                             nrow(np4[np4$pos == 'QB',]), nrow(np4[np4$pos == 'TE',]),
                             np4, players)
        np4[nrow(np4)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np5"){
        x <- normal_AI_5(nrow(np5[np5$pos == 'RB',]), nrow(np5[np5$pos == 'WR',]), 
                             nrow(np5[np5$pos == 'QB',]), nrow(np5[np5$pos == 'TE',]),
                             np5, players)
        np5[nrow(np5)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np6"){
        x <- normal_AI_6(nrow(np6[np6$pos == 'RB',]), nrow(np6[np6$pos == 'WR',]), 
                             nrow(np6[np6$pos == 'QB',]), nrow(np6[np6$pos == 'TE',]),
                             np6, players)
        np6[nrow(np6)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np7"){
        x <- normal_AI_7(nrow(np7[np7$pos == 'RB',]), nrow(np7[np7$pos == 'WR',]), 
                             nrow(np7[np7$pos == 'QB',]), nrow(np7[np7$pos == 'TE',]),
                             np7, players)
        np7[nrow(np7)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np8"){
        x <- normal_AI_8(nrow(np8[np8$pos == 'RB',]), nrow(np8[np8$pos == 'WR',]), 
                             nrow(np8[np8$pos == 'QB',]), nrow(np8[np8$pos == 'TE',]),
                             np8, players)
        np8[nrow(np8)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np9"){

        x <- normal_AI_9(nrow(np9[np9$pos == 'RB',]), nrow(np9[np9$pos == 'WR',]), 
                             nrow(np9[np9$pos == 'QB',]), nrow(np9[np9$pos == 'TE',]),
                             np9, players)
        np9[nrow(np9)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np10"){
        x <- normal_AI_10(nrow(np10[np10$pos == 'RB',]), nrow(np10[np10$pos == 'WR',]), 
                             nrow(np10[np10$pos == 'QB',]), nrow(np10[np10$pos == 'TE',]),
                             np10, players)
        np10[nrow(np10)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
    }
    for(team in rev(league)){
      if(team == "np1"){
        
        x <- normal_AI_1(nrow(np1[np1$pos == 'RB',]), nrow(np1[np1$pos == 'WR',]), 
                         nrow(np1[np1$pos == 'QB',]), nrow(np1[np1$pos == 'TE',]),
                         np1, players)
        np1[nrow(np1)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np2"){
        
        x <- normal_AI_2(nrow(np2[np2$pos == 'RB',]), nrow(np2[np2$pos == 'WR',]), 
                         nrow(np2[np2$pos == 'QB',]), nrow(np2[np2$pos == 'TE',]),
                         np2, players)
        np2[nrow(np2)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np3"){
        x <- normal_AI_3(nrow(np3[np3$pos == 'RB',]), nrow(np3[np3$pos == 'WR',]), 
                         nrow(np3[np3$pos == 'QB',]), nrow(np3[np3$pos == 'TE',]),
                         np3, players)
        np3[nrow(np3)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np4"){
        x <- normal_AI_4(nrow(np4[np4$pos == 'RB',]), nrow(np4[np4$pos == 'WR',]), 
                         nrow(np4[np4$pos == 'QB',]), nrow(np4[np4$pos == 'TE',]),
                         np4, players)
        np4[nrow(np4)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np5"){
        x <- normal_AI_5(nrow(np5[np5$pos == 'RB',]), nrow(np5[np5$pos == 'WR',]), 
                         nrow(np5[np5$pos == 'QB',]), nrow(np5[np5$pos == 'TE',]),
                         np5, players)
        np5[nrow(np5)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np6"){
        x <- normal_AI_6(nrow(np6[np6$pos == 'RB',]), nrow(np6[np6$pos == 'WR',]), 
                         nrow(np6[np6$pos == 'QB',]), nrow(np6[np6$pos == 'TE',]),
                         np6, players)
        np6[nrow(np6)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np7"){
        x <- normal_AI_7(nrow(np7[np7$pos == 'RB',]), nrow(np7[np7$pos == 'WR',]), 
                         nrow(np7[np7$pos == 'QB',]), nrow(np7[np7$pos == 'TE',]),
                         np7, players)
        np7[nrow(np7)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np8"){
        x <- normal_AI_8(nrow(np8[np8$pos == 'RB',]), nrow(np8[np8$pos == 'WR',]), 
                         nrow(np8[np8$pos == 'QB',]), nrow(np8[np8$pos == 'TE',]),
                         np8, players)
        np8[nrow(np8)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np9"){
        
        x <- normal_AI_9(nrow(np9[np9$pos == 'RB',]), nrow(np9[np9$pos == 'WR',]), 
                         nrow(np9[np9$pos == 'QB',]), nrow(np9[np9$pos == 'TE',]),
                         np9, players)
        np9[nrow(np9)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
      else if(team == "np10"){
        x <- normal_AI_10(nrow(np10[np10$pos == 'RB',]), nrow(np10[np10$pos == 'WR',]), 
                          nrow(np10[np10$pos == 'QB',]), nrow(np10[np10$pos == 'TE',]),
                          np10, players)
        np10[nrow(np10)+1,] <- c(x[1][1], x[2][1], x[3][1], x[4][1], x[5][1])
        players <- players %>% filter(str_id != x[1])
      }
    }
  }
  #change to num####
  np1$FWP <- as.numeric(np1$FWP)
  np1$rank <- as.numeric(np1$rank)
  np2$FWP <- as.numeric(np2$FWP)
  np2$rank <- as.numeric(np2$rank)
  np3$FWP <- as.numeric(np3$FWP)
  np3$rank <- as.numeric(np3$rank)
  np4$FWP <- as.numeric(np4$FWP)
  np4$rank <- as.numeric(np4$rank)
  np5$FWP <- as.numeric(np5$FWP)
  np5$rank <- as.numeric(np5$rank)
  np6$FWP <- as.numeric(np6$FWP)
  np6$rank <- as.numeric(np6$rank)
  np7$FWP <- as.numeric(np7$FWP)
  np7$rank <- as.numeric(np7$rank)
  np8$FWP <- as.numeric(np8$FWP)
  np8$rank <- as.numeric(np8$rank)
  np9$FWP <- as.numeric(np9$FWP)
  np9$rank <- as.numeric(np9$rank)
  np10$FWP <- as.numeric(np10$FWP)
  np10$rank <- as.numeric(np10$rank)

  #analyzing the total results via total average points from starters
  one <- max(np1$FWP[np1$pos=="QB"]) + top_n(np1, 'WR', 2, FWP)$FWP[1] + 
    top_n(np1, 'WR', 2, FWP)$FWP[2] + top_n(np1, 'RB', 2, FWP)$FWP[1] + top_n(np1, 'RB', 2, FWP)$FWP[2] +
    max(np1$FWP[np1$pos=="TE"]) + top_n_flex(np1,2,FWP,id,pos)$FWP[1] + top_n_flex(np1,2,FWP,id,pos)$FWP[2]
  two <- max(np2$FWP[np2$pos=="QB"]) + top_n(np2, 'WR', 2, FWP)$FWP[1] + 
    top_n(np2, 'WR', 2, FWP)$FWP[2] + top_n(np2, 'RB', 2, FWP)$FWP[1] + top_n(np2, 'RB', 2, FWP)$FWP[2] +
    max(np2$FWP[np2$pos=="TE"]) + top_n_flex(np2,2,FWP,id,pos)$FWP[1] + top_n_flex(np2,2,FWP,id,pos)$FWP[2]
  three <- max(np3$FWP[np3$pos=="QB"]) + top_n(np3, 'WR', 2, FWP)$FWP[1] + 
    top_n(np3, 'WR', 2, FWP)$FWP[2] + top_n(np3, 'RB', 2, FWP)$FWP[1] + top_n(np3, 'RB', 2, FWP)$FWP[2] +
    max(np3$FWP[np3$pos=="TE"]) + top_n_flex(np3,2,FWP,id,pos)$FWP[1] + top_n_flex(np3,2,FWP,id,pos)$FWP[2]
  four <- max(np4$FWP[np4$pos=="QB"]) + top_n(np4, 'WR', 2, FWP)$FWP[1] + 
    top_n(np4, 'WR', 2, FWP)$FWP[2] + top_n(np4, 'RB', 2, FWP)$FWP[1] + top_n(np4, 'RB', 2, FWP)$FWP[2] +
    max(np4$FWP[np4$pos=="TE"]) + top_n_flex(np4,2,FWP,id,pos)$FWP[1] + top_n_flex(np4,2,FWP,id,pos)$FWP[2]
  five <- max(np5$FWP[np5$pos=="QB"]) + top_n(np5, 'WR', 2, FWP)$FWP[1] + 
    top_n(np5, 'WR', 2, FWP)$FWP[2] + top_n(np5, 'RB', 2, FWP)$FWP[1] + top_n(np5, 'RB', 2, FWP)$FWP[2] +
    max(np5$FWP[np5$pos=="TE"]) + top_n_flex(np5,2,FWP,id,pos)$FWP[1] + top_n_flex(np5,2,FWP,id,pos)$FWP[2]
  six <- max(np6$FWP[np6$pos=="QB"]) + top_n(np6, 'WR', 2, FWP)$FWP[1] + 
    top_n(np6, 'WR', 2, FWP)$FWP[2] + top_n(np6, 'RB', 2, FWP)$FWP[1] + top_n(np6, 'RB', 2, FWP)$FWP[2] +
    max(np6$FWP[np6$pos=="TE"]) + top_n_flex(np6,2,FWP,id,pos)$FWP[1] + top_n_flex(np6,2,FWP,id,pos)$FWP[2]
  seven <- max(np7$FWP[np7$pos=="QB"]) + top_n(np7, 'WR', 2, FWP)$FWP[1] + 
    top_n(np7, 'WR', 2, FWP)$FWP[2] + top_n(np7, 'RB', 2, FWP)$FWP[1] + top_n(np7, 'RB', 2, FWP)$FWP[2] +
    max(np7$FWP[np7$pos=="TE"]) + top_n_flex(np7,2,FWP,id,pos)$FWP[1] + top_n_flex(np7,2,FWP,id,pos)$FWP[2]
  eight <- max(np8$FWP[np8$pos=="QB"]) + top_n(np8, 'WR', 2, FWP)$FWP[1] + 
    top_n(np8, 'WR', 2, FWP)$FWP[2] + top_n(np8, 'RB', 2, FWP)$FWP[1] + top_n(np8, 'RB', 2, FWP)$FWP[2] +
    max(np8$FWP[np8$pos=="TE"]) + top_n_flex(np8,2,FWP,id,pos)$FWP[1] + top_n_flex(np8,2,FWP,id,pos)$FWP[2]
  nine <- max(np9$FWP[np9$pos=="QB"]) + top_n(np9, 'WR', 2, FWP)$FWP[1] + 
    top_n(np9, 'WR', 2, FWP)$FWP[2] + top_n(np9, 'RB', 2, FWP)$FWP[1] + top_n(np9, 'RB', 2, FWP)$FWP[2] +
    max(np9$FWP[np9$pos=="TE"]) + top_n_flex(np9,2,FWP,id,pos)$FWP[1] + top_n_flex(np9,2,FWP,id,pos)$FWP[2]
  ten <- max(np10$FWP[np10$pos=="QB"]) + top_n(np10, 'WR', 2, FWP)$FWP[1] + 
    top_n(np10, 'WR', 2, FWP)$FWP[2] + top_n(np10, 'RB', 2, FWP)$FWP[1] + top_n(np10, 'RB', 2, FWP)$FWP[2] +
    max(np1$FWP[np10$pos=="TE"]) + top_n_flex(np10,2,FWP,id,pos)$FWP[1] + top_n_flex(np10,2,FWP,id,pos)$FWP[2]
  
  
  #new####
#add to the master dfs 
  round = it
  for(team in league){
    #store data####
    if(team == "np1"){
      p1[nrow(p1) +1, ] <- c(which(league == "np1"), sum(np1$FWP), mean(np1$FWP), mean(np1$rank), first(which(np1$pos == 'QB')), round, one)
    }
    else if(team == "np2"){
      p2[nrow(p2) +1, ] <- c(which(league == "np2"), sum(np2$FWP), mean(np2$FWP), mean(np2$rank), first(which(np2$pos == 'QB')), round, two)
    }
    else if(team == "np3"){
      p3[nrow(p3) +1, ] <- c(which(league == "np3"), sum(np3$FWP), mean(np3$FWP), mean(np3$rank), first(which(np3$pos == 'QB')), round, three)
    }
    else if(team == "np4"){
      p4[nrow(p4) +1, ] <- c(which(league == "np4"), sum(np4$FWP), mean(np4$FWP), mean(np4$rank), first(which(np4$pos == 'QB')), round, four)
    }
    else if(team == "np5"){
      p5[nrow(p5) +1, ] <- c(which(league == "np5"), sum(np5$FWP), mean(np5$FWP), mean(np5$rank), first(which(np5$pos == 'QB')), round, five)
    }
    else if(team == "np6"){
      p6[nrow(p6) +1, ] <- c(which(league == "np6"), sum(np6$FWP), mean(np6$FWP), mean(np6$rank), first(which(np6$pos == 'QB')), round, six)
    }
    else if(team == "np7"){
      p7[nrow(p7) +1, ] <- c(which(league == "np7"), sum(np7$FWP), mean(np7$FWP), mean(np7$rank), first(which(np7$pos == 'QB')), round, seven)
    }
    else if(team == "np8"){
      p8[nrow(p8) +1, ] <- c(which(league == "np8"), sum(np8$FWP), mean(np8$FWP), mean(np8$rank), first(which(np8$pos == 'QB')), round, eight)
    }
    else if(team == "np9"){
      p9[nrow(p9) +1, ] <- c(which(league == "np9"), sum(np9$FWP), mean(np9$FWP), mean(np9$rank), first(which(np9$pos == 'QB')), round, nine)
    }
    else if(team == "np10"){
      p10[nrow(p10) +1, ] <- c(which(league == "np10"), sum(np10$FWP), mean(np10$FWP), mean(np10$rank), first(which(np10$pos == 'QB')), round, ten)
    }
  
  }
}






