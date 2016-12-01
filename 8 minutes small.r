
deck <- read.csv("8 minute empire.csv", header=TRUE)

deck$sorting <- sample(1:36, 36, replace = F)
library(plyr)
arrangement <- arrange(deck, sorting)
Player1 <- arrangement[1,]

a <- 1
for (i in 2:8) {
  Player1 <- rbind(Player1, arrangement[4*a+1,])
  a <- a+1
}

result_P1 <- table(Player1$Type)


numbers <- c(0:9)
points_of_carrot <- c(0,0,1,1,1,2,2,3,5,5)
carrot <- data.frame(numbers,points_of_carrot)
numbers <- c(0:6)
points_of_orb <- c(0,0,1,2,3,5,5)
orb <- data.frame(numbers,points_of_orb)
numbers <- c(0:5)
points_of_ruby <- c(0,1,2,3,5,5)
ruby <- data.frame(numbers,points_of_ruby)
numbers <- c(0:8)
points_of_steel <- c(0,0,1,1,2,2,3,5,5)
steel <- data.frame(numbers,points_of_steel)
numbers <- c(0:7)
points_of_wood <- c(0,0,1,1,2,3,5,5)
wood <- data.frame(numbers,points_of_wood)

carrot_count <- result_P1[1]*2+result_P1[3]

R1_carrot <- carrot[carrot_count==carrot$numbers,2]
R1_orb <- orb[result_P1[4]==orb$numbers,2]
R1_steel <- steel[result_P1[6]==steel$numbers,2]
R1_wood  <- wood[result_P1[7]==wood$numbers,2]
R1_ruby <- ruby[result_P1[5]==ruby$numbers,2]
R1_boat <- result_P1[2]

R1_turnout <- c(R1_carrot,R1_orb,R1_steel,R1_wood,R1_ruby,R1_boat,
                R1_carrot+R1_orb+R1_steel+R1_wood+R1_ruby+R1_boat)
R1_sum <- R1_turnout
type_name <- c("carrot","orb","steel","wood","ruby","boat","summ")
names(R1_sum) <- type_name


for (i in 1:100000) {
  
  deck$sorting <- sample(1:36, 36, replace = F)
  library(plyr)
  arrangement <- arrange(deck, sorting)
  Player1 <- arrangement[1,]
  a <- 1
  for (i in 2:8) {
    Player1 <- rbind(Player1, arrangement[4*a+1,])
    a <- a+1
  }
  
  result_P1 <- table(Player1$Type)

  carrot_count <- result_P1[1]*2+result_P1[3]
  
  R1_carrot <- carrot[carrot_count==carrot$numbers,2]
  R1_orb <- orb[result_P1[4]==orb$numbers,2]
  R1_steel <- steel[result_P1[6]==steel$numbers,2]
  R1_wood  <- wood[result_P1[7]==wood$numbers,2]
  R1_ruby <- ruby[result_P1[5]==ruby$numbers,2]
  R1_boat <- result_P1[2]
  
  R1_turnout <- c(R1_carrot,R1_orb,R1_steel,R1_wood,R1_ruby,R1_boat,
                  R1_carrot+R1_orb+R1_steel+R1_wood+R1_ruby+R1_boat)
  
  R1_sum <- rbind(R1_sum,R1_turnout, deparse.level=0)
}
