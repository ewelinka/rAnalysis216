setwd("/Users/ewe/Phd/eval216")

maxT = 9
maxB = 13
should_exclude = FALSE #TRUE
questions13 =c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13")
questions9 = questions13[1:9]
questions9abilities = c("Alg1","Alg2","Alg3","Alg4","Alg5","Rep1","Rep2","Control1","Control2")
questions13abilities = c("Seq1","Seq2","Seq3","Seq4","Seq5","Seq6","If1","If2","sLoop","nLoop1","nLoop2","nLoop3","nLoop4")
questionsSeqAbilities = c("Seq1","Seq2","Seq3","Seq4","Seq5","Seq6")
initVars <- function() {
  # 1/4 questions right / 4 excluded 
  robotito25 = c("Sebastián Jacinto", "María Pía Rivero", "Sofía Acevedo", "Leandra Suarez") 
  # 1/2 questions right / 7 excluded 
  robotito50 = c("Benjamín Luzardo", "Ainhoa Hernandez", "Lucas Amorin", "Juana Silva", "Libero Ciamarra","Catalina Baliña", "Ariandna Anderzon")
  robotitoExclude <<- c(robotito25,robotito50)
}
initVars()

robotitoV2 <- read.csv("./csv/robotitoV2.csv",sep = ",")
robotitoV2$TOTAL=apply(robotitoV2[,c(3:6)],1,FUN=sum)
colnames(robotitoV2)[7] = "TOTAL_R"
if(should_exclude){
  robotitoV2 = robotitoV2[which(!robotitoV2$NAME %in% robotitoExclude),]
}
