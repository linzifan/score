library(shiny)
library(UsingR)

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




shinyServer(
  function(input, output){
    output$GAB <- renderPrint({input$namerefresh
                               isolate(cat(input$name1, "vs.", input$name2))})       
    output$GCD <- renderPrint({input$namerefresh
                               isolate(cat(input$name3, "vs.", input$name4))}) 
    output$GBC <- renderPrint({input$namerefresh
                               isolate(cat(input$name2, "vs.", input$name3))}) 
    output$GDA <- renderPrint({input$namerefresh
                               isolate(cat(input$name4, "vs.", input$name1))}) 
    output$GAC <- renderPrint({input$namerefresh
                               isolate(cat(input$name1, "vs.", input$name3))}) 
    output$GDB <- renderPrint({input$namerefresh
                               isolate(cat(input$name4, "vs.", input$name2))}) 

    output$result <- renderPrint({input$namerefresh
                                  isolate(names <- c(input$name1,input$name2,input$name3,input$name4))
                                  input$gamerefresh
                                  isolate(G1 <- c(input$A1,input$C1,input$B2,input$D2,input$A3,input$D3))
                                  isolate(G2 <- c(input$B1,input$D1,input$C2,input$A2,input$C3,input$B3))
                                  print(soccer.rank(names, G1, G2), row.names=F)})
    output$summary <- renderPrint({input$namerefresh
                                   isolate(names <- c(input$name1,input$name2,input$name3,input$name4))                  
                                   input$gamerefresh 
                                   isolate(G1 <- c(input$A1,input$C1,input$B2,input$D2,input$A3,input$D3))
                                   isolate(G2 <- c(input$B1,input$D1,input$C2,input$A2,input$C3,input$B3))
                                   result <- soccer.rank(names, G1, G2)
                                   if (sum(result[,3])==0) summ <- "Games not start yet." 
                                     else if (sum(result[,3])==4) summ <- c("Two more rounds.\n",
                                                                            as.character(result[1,2]),
                                                                            "and", 
                                                                            as.character(result[2,2]),
                                                                            "take the lead.")
                                         else if (sum(result[,3])==8) summ <- c("One more round.\n",
                                                                                as.character(result[1,2]),
                                                                                "and", 
                                                                                as.character(result[2,2]),
                                                                                "take the lead.")
                                            else if (sum(result[,3])==12) summ <- c("Group stage finished.\n",
                                                                                    as.character(result[1,2]),
                                                                                    "and", 
                                                                                    as.character(result[2,2]),
                                                                                    "advance to knockout stage.")
                                                  else summ <- "Incomplete information."
                                     
                                  cat(summ)
                                   })
    
    }
)