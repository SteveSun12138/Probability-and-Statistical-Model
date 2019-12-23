sim2 <- function(nreps) {
  totaltoss = 0
  totalmoney = 0
  money = 0
  situation1 = 0   #HHH___
  situation2 = 0   #THHH__
  situation3 = 0   #_THHH_
  situation4 = 0   #TTTHHH
  situation5 = 0   # all situations other than 1 to 4
  for (i in 1:nreps) {#take nreps turns
    toss1 <- sample (0:1 ,1) #1 means head and 0 means tail
    toss2 <- sample (0:1 ,1)
    toss3 <- sample (0:1 ,1)
    toss4 <- sample (0:1 ,1)
    toss5 <- sample (0:1 ,1)
    toss6 <- sample (0:1 ,1)
    
    totaltoss = totaltoss + toss1
    #situation 1
    if(toss1 == 1 && toss2 == 1 && toss3 == 1) {
        money = money + 3
        situation1 = situation1 + 1
          
    } else if(toss1 == 0 && toss2 == 1 && toss3 == 1 && toss4 == 1) {#situation2
        money = money + 4
        situation2 = situation2 + 1
          
    } else if(toss2 == 0 && toss3 == 1 && toss4 == 1 && toss5 == 1){#situation3
        money = money + 5
        situation3 = situation3 + 1
          
    } 
    else if(toss1 == 0 && toss2 == 0 && toss3 == 0 && toss4 == 1 && toss5 == 1 && toss6 == 1){#situation4
        money = money + 6
        situation4 = situation4 + 1
         
    } else {#situation5
        money = money + 6
        situation5 = situation5 + 1
      
    }

  }
  totalmoney = totalmoney + situation1 * 3
  totalmoney = totalmoney + situation2 * 4
  totalmoney = totalmoney + situation3 * 5
  totalmoney = totalmoney + situation4 * 6
  totalmoney = totalmoney + situation5 * 6
  
  return(totalmoney/nreps)
}
  