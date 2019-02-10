setwd("C:/Users/User/Desktop/Чина/CEU/2nd Year/2.Winter2016/Advanced Labor Economics/Replication project")

numeric <- numeric(length(16))
a1 <- matrix(data = c(15:30), nrow =16, ncol = 2)
a1[,2] <- numeric
a2 <- a1

b1 <- read.csv("b_univ.csv")
b2 <- read.csv("b_hs.csv")
b1[,2] <- b1[,2]*12
b2[,2] <- b2[,2]*12

rate <- read.table("interestrate2012KG.txt")
r1 <- rate[,2]/100
r <- mean(r1) # mean interest rate for 2012 is 6.85%

c1 <- matrix(data = 0, nrow = nrow(b1), ncol = 16)
c2 <- matrix(data = 0, nrow = nrow(b2), ncol = 16)

for (i in 1:nrow(a1)){
  for (j in 1:nrow(b1)){
    c1[j,i] <- b1[j,2]/((1 - r^(b1[j,1] - a1[i,1])))
  }
}

for (i in 1:nrow(a2)){
  for (j in 1:nrow(b2)){
    c2[j,i] <- sum(b2[j,2]/((1 - r^(b2[j,1] - a2[i,1]))))
  }
}


for (i in 1:nrow(a1)){
  a1[i,2] <- sum(c1[,i])
}

for (i in 1:nrow(a2)){
  a2[i,2] <- sum(c2[,i])
}

write.csv(a1, "ELE_univ.csv") # import these two spreadsheets into stata for further mergings
write.csv(a2, "ELE_hs.csv")
