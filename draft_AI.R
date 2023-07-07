#creating the AI draft bots--these are simple decision tree like algorithms that are based on a number of if-else statements
normal_AI_1 <- function(rb, wr, qb, te, df, players){
  if(nrow(df) == 4 & qb == 0){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 5 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if((nrow(df) < 13 | nrow(df) > 11) & te == 1){
    one = max(players$ppt[players$pos != 'QB' & players$pos != 'TE'])
    player <- players$player_id[players$pos != 'QB' & players$pos != 'TE' & players$ppt == one][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else{
    one = max(players$ppt[players$pos != 'QB'])
    player <- players$player_id[players$ppt == one & players$pos != 'QB'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  return(df)
}

normal_AI_2 <- function(rb, wr, qb, te, df, players){
  if(nrow(df) == 3 & qb == 0){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 5 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 7 & wr == 1){
    one = max(players$ppt[players$pos == 'WR'])
    player <- players$player_id[players$ppt == one & players$pos == 'WR'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 3 & wr == 0){
    one = max(players$ppt[players$pos == 'WR'])
    player <- players$player_id[players$ppt == one & players$pos == 'WR'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 10 & wr == 2){
    one = max(players$ppt[players$pos == 'WR'])
    player <- players$player_id[players$ppt == one & players$pos == 'WR'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 12 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else{
    
    one = max(players$ppt[players$pos != 'QB' & players$pos != 'TE'])
    player <- players$player_id[players$pos != 'QB' & players$pos != 'TE' & players$ppt == one][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  return(df)
}
normal_AI_3 <- function(rb, wr, qb, te, df, players){
  if(nrow(df) == 3 & qb == 0){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 8 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else{
    one = max(players$ppt[players$pos != 'QB' & players$pos != 'TE'])
    player <- players$player_id[players$pos != 'QB' & players$pos != 'TE'& players$ppt == one][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  return(df)
}
normal_AI_4 <- function(rb, wr, qb, te, df, players){
  if(nrow(df) == 2 & qb == 0){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 6 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) < 14 & te == 1){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else{
    one = max(players$ppt[players$pos != 'QB' & players$pos != 'TE'])
    player <- players$player_id[players$pos != 'QB' & players$pos != 'TE' & players$ppt == one][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  return(df)
}
normal_AI_5 <- function(rb, wr, qb, te, df, players){
  if(nrow(df) == 13 & qb == 0){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 3 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) < 13 & te == 1){
    one = max(players$ppt[players$pos != 'QB' & players$pos != 'TE'])
    player <- players$player_id[players$pos != 'QB' & players$pos != 'TE' & players$ppt == one][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else{
    one = max(players$ppt[players$pos != 'QB'])
    player <- players$player_id[players$ppt == one & players$pos != 'QB'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  return(df)
}
normal_AI_6 <- function(rb, wr, qb, te, df, players){
  if(nrow(df) == 7 & qb == 0){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 3 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) < 14 & te == 1){
    one = max(players$ppt[players$pos != 'QB' & players$pos != 'TE'])
    player <- players$player_id[players$pos != 'QB' & players$pos != 'TE' & players$ppt == one][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 14){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else{
    one = max(players$ppt[players$pos != 'QB'])
    player <- players$player_id[players$ppt == one & players$pos != 'QB'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  return(df)
}
normal_AI_7 <- function(rb, wr, qb, te, df, players){
  if(nrow(df) == 8 & qb == 0){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 0 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) < 13 & te == 1){
    one = max(players$ppt[players$pos != 'QB' & players$pos != 'TE'])
    player <- players$player_id[players$pos != 'QB' & players$pos != 'TE' & players$ppt == one][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 14){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else{
    one = max(players$ppt[players$pos != 'QB'])
    player <- players$player_id[players$ppt == one & players$pos != 'QB'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  return(df)
}
normal_AI_8 <- function(rb, wr, qb, te, df, players){
  if(nrow(df) == 4 & qb == 0){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 8 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 13){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 14){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else{
    one = max(players$ppt[players$pos != 'QB' & players$pos != 'TE'])
    player <- players$player_id[players$pos != 'QB' & players$pos != 'TE' & players$ppt == one][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  return(df)
}
normal_AI_9 <- function(rb, wr, qb, te, df, players){
  if(nrow(df) == 0 & qb == 0){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 5 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 13){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 14){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else{
    one = max(players$ppt[players$pos != 'QB' & players$pos != 'TE'])
    player <- players$player_id[players$pos != 'QB' & players$pos != 'TE' & players$ppt == one][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  return(df)
}
normal_AI_10 <- function(rb, wr, qb, te, df, players){
  if(nrow(df) == 0 & qb == 0){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 12 & te == 0){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 14){
    one = max(players$ppt[players$pos == 'TE'])
    player <- players$player_id[players$ppt == one & players$pos == 'TE'][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else if(nrow(df) == 13){
    one = max(players$ppt[players$pos == 'QB'])
    player <- players$player_id[players$ppt == one & players$pos == 'QB']
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  else{
    one = max(players$ppt[players$pos != 'QB' & players$pos != 'TE'])
    player <- players$player_id[players$pos != 'QB' & players$pos != 'TE' & players$ppt == one][1]
    id <- player
    name <- players$player_name[players$player_id == player]
    pos <- players$pos[players$player_id == player]
    fwp <- players$ppt[players$player_id == player]
    rank <- players$rank[players$player_id == player]
    df = df[nrow(df) + 1,] <- c(id, name, pos, fwp, rank)
    
  }
  return(df)
}

#function to get the top n number of players (used for draft analysis after simulation) 
top_n <- function(df, filter1, number, col){
  temp <- df %>% filter(pos==filter1) %>% arrange(df$col)
  return(head(temp, number))
}

  #function to het the top n number of FLEX players (used for draft analysis after simulation)
top_n_flex <- function(df, number, col, id, pos){
  rb <- df %>% filter(pos=='RB') %>% arrange(df$col)
  wr <- df %>% filter(pos=='WR') %>% arrange(df$col)
  te <- df %>% filter(pos=='TE') %>% arrange(df$col)
  rb1 = rb$id[1]
  rb2 = rb$id[2]
  wr1 = wr$id[1]
  wr2 = wr$id[2]
  te1 = te$id[1]
  temp <- df %>% filter(pos=='RB' | pos=='WR' | pos=='TE') %>% arrange(df$col)
  temp <- temp %>% filter(id!=rb1 & id!=rb2 & id!=wr1 & id!=wr2 & id!=te1)
  return(head(temp,2))
}


#creates empty dataframes which will be used for analysis--these frames are added to after each iteration of a draft
p1 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(p1) <- c('startpos', 'total_FWP', 'average_FWP', 'average_rank', 'first_qb_round', 'iteration', 'starters')
p2 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(p2) <- c('startpos', 'total_FWP', 'average_FWP', 'average_rank', 'first_qb_round',  'iteration', 'starters')
p3 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(p3) <- c('startpos', 'total_FWP', 'average_FWP', 'average_rank', 'first_qb_round',  'iteration', 'starters')
p4 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(p4) <- c('startpos', 'total_FWP', 'average_FWP', 'average_rank', 'first_qb_round',  'iteration', 'starters')
p5 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(p5) <- c('startpos', 'total_FWP', 'average_FWP', 'average_rank', 'first_qb_round',  'iteration', 'starters')
p6 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(p6) <- c('startpos', 'total_FWP', 'average_FWP', 'average_rank', 'first_qb_round',  'iteration', 'starters')
p7 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(p7) <- c('startpos', 'total_FWP', 'average_FWP', 'average_rank', 'first_qb_round',  'iteration', 'starters')
p8 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(p8) <- c('startpos', 'total_FWP', 'average_FWP', 'average_rank', 'first_qb_round',  'iteration', 'starters')
p9 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(p9) <- c('startpos', 'total_FWP', 'average_FWP', 'average_rank', 'first_qb_round',  'iteration', 'starters')
p10 <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(p10) <- c('startpos', 'total_FWP', 'average_FWP', 'average_rank', 'first_qb_round',  'iteration', 'starters')


