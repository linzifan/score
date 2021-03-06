matches <- data.frame(name=c("AB","CD","BC","DA","AC","DB"), g1=rep(NA,6), g2=rep(NA,6))
result <- data.frame(Pos=c(1:4), Team=c("A","B","C","D"), Name=rep(NA,4),
                     Pld=rep(0,4), W=rep(0,4), D=rep(0,4), L=rep(0,4), 
                     GF=rep(0,4), GA=rep(0,4), GD=rep(0,4), Pt=rep(0,4))






soccer.rank <- function(names,G1,G2){
  matches <- data.frame(name=c("AB","CD","BC","DA","AC","DB"), g1=G1, g2=G2)
  result <- data.frame(Pos=c(1:4), Team=c("A","B","C","D"), Name=names,
                       Pld=rep(0,4), W=rep(0,4), D=rep(0,4), L=rep(0,4), 
                       GF=rep(0,4), GA=rep(0,4), GD=rep(0,4), Pt=rep(0,4))
  
  match <- list(NA)
  match$A <- matrix(as.numeric(c(matches[1,2:3], matches[4,3:2], matches[5,2:3])), 3, 2, byrow=T)
  match$B <- matrix(as.numeric(c(matches[1,3:2], matches[3,2:3], matches[6,3:2])), 3, 2, byrow=T)
  match$C <- matrix(as.numeric(c(matches[2,2:3], matches[3,3:2], matches[5,3:2])), 3, 2, byrow=T)
  match$D <- matrix(as.numeric(c(matches[2,3:2], matches[4,2:3], matches[6,2:3])), 3, 2, byrow=T)
  team <- list(NA)
  team$A <- c(W=0,D=0,L=0,GF=0,GA=0)
  team$B <- c(W=0,D=0,L=0,GF=0,GA=0)
  team$C <- c(W=0,D=0,L=0,GF=0,GA=0)
  team$D <- c(W=0,D=0,L=0,GF=0,GA=0)
  
  for (j in 2:5){
    for (i in 1:3)
    {
      if (!is.na(match[[j]][i,1]))  {team[[j]][1] <- team[[j]][1] + (match[[j]][i,1]>match[[j]][i,2])
                                     team[[j]][2] <- team[[j]][2] + (match[[j]][i,1]==match[[j]][i,2])
                                     team[[j]][3] <- team[[j]][3] + (match[[j]][i,1]<match[[j]][i,2])
                                     team[[j]][4] <- team[[j]][4] + match[[j]][i,1]
                                     team[[j]][5] <- team[[j]][5] + match[[j]][i,2]}
    }
  }
  
  result[result$Team=="A",5:9] <- team$A
  result[result$Team=="B",5:9] <- team$B
  result[result$Team=="C",5:9] <- team$C
  result[result$Team=="D",5:9] <- team$D
  result$Pld <- result$W+result$D+result$L
  result$GD <- result$GF-result$GA
  result$Pt <- result$W*3+result$D*1+result$W*0
  result$Pos <- order(result$Pt, decreasing=T)
  # Tiebreaking criteria
  # 1.Greater number of points obtained in the group matches between the teams concerned
  # 2.Goal difference resulting from the group matches between the teams concerned
  # 3.Greater number of goals scored in the group matches between the teams concerned
  result <- result[order(-result$Pt, -result$GD, -result$GF),]
  result$Pos <- c(1:4)
  result <- result[,-2]
  return(result)
}

# input 
G1 <- c(1,0,1,2,3,2)
G2 <- c(0,1,4,1,1,1)
names <- c("Uzbekistan", "North Korea", "Saudi Arabia", "China PR")
result <- soccer.rank(names, G1, G2)
library(graphics)
plot.table(result)

plot.table(result)

