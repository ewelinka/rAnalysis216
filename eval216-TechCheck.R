####### TechCheck-K
techCheck <- read.csv("./csv/TechCheck.csv",sep = ",")

if(should_exclude){
  techCheck = techCheck[which(!techCheck$NAME %in% robotitoExclude),]
}

# add total value
techCheck$TOTAL=apply(techCheck[,c(4:12)],1,FUN=sum)
techCheck$GROUP2 = factor(techCheck$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("ACTIVE","ACTIVE","PASSIVE"))
# changing TYPE from chr to factor
techCheck$TYPE = factor(techCheck$TYPE, levels=c("PRE","POST"), labels=c("PRE","POST"))
# changing GROUP from chr to factor
techCheck$GROUP = factor(techCheck$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("A2","A1","CG"))
# TOTAL OF GOOD ANSWERS
library(dplyr)  
techCheckQ = techCheck %>%                                        
  group_by(GROUP,TYPE) %>% 
  summarise_at(vars(all_of(questions9)), list(name = sum)) %>%
  as.data.frame()

# change names from P1_name, P2_name, etc to P1, P2, etc
colnames(techCheckQ)[(dim(techCheckQ)[2]-(9-1)):(dim(techCheckQ)[2])]=questions9
# add total size of each group
addGroupSize <- function() {
  groupSize=c()
  for (i in 1:dim(techCheckQ)[1]) {
    if (techCheckQ$GROUP[i] == "A2"){
      groupSize[i] = dim(techCheck[techCheck$TYPE=="PRE",][techCheck[techCheck$TYPE=="PRE",]$GROUP=="A2",])[1]
    }else if(techCheckQ$GROUP[i] == "A1"){
      groupSize[i] = dim(techCheck[techCheck$TYPE=="PRE",][techCheck[techCheck$TYPE=="PRE",]$GROUP=="A1",])[1]
    } else{
      groupSize[i] = dim(techCheck[techCheck$TYPE=="PRE",][techCheck[techCheck$TYPE=="PRE",]$GROUP=="CG",])[1]
    }
  }
  techCheckQ$TOTAL <<- groupSize # <<- important to make the magic happen
}
addGroupSize()
rownames(techCheckQ) = paste(techCheckQ$TYPE,techCheckQ$GROUP,sep="-")

## see TOTAL
with(techCheck, tapply(TOTAL, list(GROUP,TYPE), mean))[c(2,1,3),]
with(techCheck, tapply(TOTAL, list(GROUP2,TYPE), mean))

# The R function shapiro.test() can be used to perform the Shapiro-Wilk test of normality for one variable (univariate):
# From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(techCheck$TOTAL) # Reduced p-value = 0.01027

shapiro.test(subset(techCheck[techCheck$TYPE=="PRE",], GROUP=="A2")$TOTAL)
shapiro.test(subset(techCheck[techCheck$TYPE=="POST",], GROUP=="A2")$TOTAL)
shapiro.test(subset(techCheck[techCheck$TYPE=="PRE",], GROUP=="A1")$TOTAL)
shapiro.test(subset(techCheck[techCheck$TYPE=="POST",], GROUP=="A1")$TOTAL)
shapiro.test(subset(techCheck[techCheck$TYPE=="PRE",], GROUP=="CG")$TOTAL)
shapiro.test(subset(techCheck[techCheck$TYPE=="POST",], GROUP=="CG")$TOTAL)

shapiro.test(subset(techCheck[techCheck$TYPE=="PRE",], GROUP2=="ACTIVE")$TOTAL) #falla shapiro p-value = 0.009 en el excluded
shapiro.test(subset(techCheck[techCheck$TYPE=="POST",], GROUP2=="ACTIVE")$TOTAL)

## boxplots
boxplot(TOTAL ~ TYPE * GROUP, data=techCheck, ylab="Total score", main="Boxplots of TechCheck total scores (3 groups)")
boxplot(TOTAL ~ TYPE * GROUP2, data=techCheck, ylab="Total score", main="Boxplots of TechCheck total scores (active vs passive)")

####### Correlación pre y post TODOS los grupos
# preB=techCheck[techCheck$TYPE=="PRE",] 
# postB=techCheck[techCheck$TYPE=="POST",] 
techCheckPrePost = techCheck[techCheck$TYPE=="PRE",][,c("NAME","TOTAL","GROUP")] # pre name and total
colnames(techCheckPrePost)[2] = "TOTAL_pre"
techCheckPrePost$TOTAL_post = techCheck[techCheck$TYPE=="POST",]$TOTAL # total post
ggscatter(techCheckPrePost, x = "TOTAL_pre", y = "TOTAL_post", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Pre techCheckt score", ylab = "Post techCheckt score")

shapiro.test(techCheckPrePost$TOTAL_pre) # mal 0.01926, reduced mal 0.02184
shapiro.test(techCheckPrePost$TOTAL_post) #ok
cor.test(techCheckPrePost$TOTAL_pre,techCheckPrePost$TOTAL_post) #ya lo vimos en el ggscatter

####### Correlación pre y diff pre-post TODOS los grupos
techCheckPrePost$pre_post_diff <- techCheckPrePost$TOTAL_post-techCheckPrePost$TOTAL_pre
ggscatter(techCheckPrePost, x = "TOTAL_pre", y = "pre_post_diff", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Pre techCheckt score", ylab = "Diff techCheckt pre post scores")
shapiro.test(techCheckPrePost$TOTAL_pre) # mal 0.01926, reduced mal 0.02184
shapiro.test(techCheckPrePost$pre_post_diff) #ok
ggqqplot(techCheckPrePost$TOTAL_pre, ylab = "TOTAL")
ggqqplot(techCheckPrePost$pre_post_diff, ylab = "techCheck_pre_post_diff")
cor.test(techCheckPrePost$TOTAL_pre, techCheckPrePost$pre_post_diff, method = "pearson") # REDUCIDO p-value = 0.0002182, cor = -0.5191767 
####### Correlación pre y diff pre-post por grupo
groups <- c("A2", "A1", "CG") 
for(g in groups) {    
  print(g)
  current_group = subset(techCheckPrePost, GROUP==g)
  r= shapiro.test(current_group$TOTAL_pre)
  print(r)
  r= shapiro.test(current_group$pre_post_diff)
  print(r)
  r= cor.test(current_group$TOTAL_pre, current_group$pre_post_diff, method = "pearson")
  print(r)
}
####### Correlación pre y post por grupo
# ninguno significativo
for(g in groups) {    
  print(g)
  current_group_pre = subset(techCheckPrePost, GROUP==g)
  current_group_post = subset(techCheckPrePost, GROUP==g)
  r= shapiro.test(current_group_pre$TOTAL_pre)
  print(r)
  r= shapiro.test(current_group_post$TOTAL_post)
  print(r)
  r= cor.test(current_group_pre$TOTAL_pre, current_group_post$TOTAL_post, method = "pearson")
  print(r)
}


####### preparing data for factorial annova
# Check cell sizes are equal (ish) using replications()
replications(TOTAL ~ TYPE * GROUP, data=techCheck)
summary(aov(TOTAL ~ TYPE * GROUP, data = techCheck))
summary(aov(TOTAL ~ TYPE * GROUP2, data = techCheck))


#### GROUPED BY ABILITY
#### P1-P5 ALGORITHMS, P6-P7 REPRESENTATION, P8-P9 CONTROL
# select NAME, TYPE and GROUP
techCheckAbilities <- techCheck[,c(1:3)]
techCheckAbilities$ALGORITHMS = techCheck$P1+techCheck$P2+techCheck$P3+techCheck$P4+techCheck$P5
techCheckAbilities$REPRESENTATION = techCheck$P6+techCheck$P7
techCheckAbilities$CONTROL = techCheck$P8+techCheck$P9
techCheckAbilities$GROUP2 = techCheck$GROUP2 
## ALGORITHMS / not sig annova
boxplot(ALGORITHMS ~ TYPE * GROUP, data=techCheckAbilities, ylab="Score in  Algorithms", main="Boxplots of TechCheck Algorithms scores")
boxplot(ALGORITHMS ~ TYPE * GROUP2, data=techCheckAbilities, ylab="Score in  Algorithms", main="Boxplots of TechCheck Algorithms scores")
aggregate( ALGORITHMS ~ TYPE * GROUP2, techCheckAbilities, mean )
aggregate( ALGORITHMS ~ TYPE * GROUP, techCheckAbilities, mean )
summary(aov(ALGORITHMS ~ TYPE * GROUP, data = techCheckAbilities))
summary(aov(ALGORITHMS ~ TYPE * GROUP2, data = techCheckAbilities))
## REPRESENTATION / not sig annova
boxplot(REPRESENTATION ~ TYPE * GROUP, data=techCheckAbilities, ylab="Score in Representation", main="Boxplots of TechCheck Representation scores")
boxplot(REPRESENTATION ~ TYPE * GROUP2, data=techCheckAbilities, ylab="Score in Representation", main="Boxplots of TechCheck Representation scores")
aggregate( REPRESENTATION ~ TYPE * GROUP2, techCheckAbilities, mean )
aggregate( REPRESENTATION ~ TYPE * GROUP, techCheckAbilities, mean )
summary(aov(REPRESENTATION ~ TYPE * GROUP, data = techCheckAbilities))
summary(aov(REPRESENTATION ~ TYPE * GROUP2, data = techCheckAbilities))
## CONTROL
boxplot(CONTROL ~ TYPE * GROUP, data=techCheckAbilities, ylab="Score in Control", main="Boxplots of TechCheck Control scores")
boxplot(CONTROL ~ TYPE * GROUP2, data=techCheckAbilities, ylab="Score in Control", main="Boxplots of TechCheck Control scores")
aggregate( CONTROL ~ TYPE * GROUP2, techCheckAbilities, mean )
aggregate( CONTROL ~ TYPE * GROUP, techCheckAbilities, mean )
summary(aov(CONTROL ~ TYPE * GROUP, data = techCheckAbilities)) # Reducido significativo de tipo y grupos peor nada particular d egrupos activos
summary(aov(CONTROL ~ TYPE * GROUP2, data = techCheckAbilities)) # Reducido significativo de tipo
TukeyHSD(aov(CONTROL ~ TYPE * GROUP, data = techCheckAbilities))
TukeyHSD(aov(CONTROL ~ TYPE * GROUP2, data = techCheckAbilities))

# COMO NO TIENE DISTRIBUCION NORMAL HAGO WILCOXON TEST
control_pre <- techCheckAbilities$CONTROL[techCheckAbilities$GROUP2 == "PASSIVE" & techCheckAbilities$TYPE == "PRE"]
control_post <- techCheckAbilities$CONTROL[techCheckAbilities$GROUP2 == "PASSIVE" & techCheckAbilities$TYPE == "POST"]
result_control <- wilcox.test(control_pre, control_post, paired = TRUE)
print(result_control)

active_pre <- techCheckAbilities$CONTROL[techCheckAbilities$GROUP2 == "ACTIVE" & techCheckAbilities$TYPE == "PRE"]
active_post <- techCheckAbilities$CONTROL[techCheckAbilities$GROUP2 == "ACTIVE" & techCheckAbilities$TYPE == "POST"]
result_active <- wilcox.test(active_pre, active_post, paired = TRUE)
print(result_active)

wilcox.test(techCheckAbilities$CONTROL[techCheckAbilities$GROUP == "ELO" & techCheckAbilities$TYPE == "PRE"], techCheckAbilities$CONTROL[techCheckAbilities$GROUP == "ELO" & techCheckAbilities$TYPE == "POST"], paired = TRUE)

### techCheckAbilities pre-post ACTIVE not significant
summary(aov(ALGORITHMS~TYPE,techCheckAbilities[techCheckAbilities$GROUP2=="ACTIVE",]))
summary(aov(REPRESENTATION~TYPE,techCheckAbilities[techCheckAbilities$GROUP2=="ACTIVE",]))
summary(aov(CONTROL~TYPE,techCheckAbilities[techCheckAbilities$GROUP2=="ACTIVE",]))
shapiro.test(filter(techCheckAbilities, TYPE=="PRE" & GROUP=="CG")$CONTROL)
shapiro.test(filter(techCheckAbilities, TYPE=="PRE" & GROUP=="A1")$CONTROL)
shapiro.test(filter(techCheckAbilities, TYPE=="PRE" & GROUP=="A2")$CONTROL)

## abilities and robotito POST
techCheckAbilitiesRobotito = techCheckAbilities[techCheckAbilities$GROUP2=="ACTIVE",]
techCheckAbilitiesRobotito = techCheckAbilitiesRobotito[techCheckAbilitiesRobotito$TYPE=="POST",]
techCheckAbilitiesRobotito$TOTAL_R = robotitoV2$TOTAL_R/4
techCheckAbilitiesRobotito$ALGORITHMS = techCheckAbilitiesRobotito$ALGORITHMS/5
techCheckAbilitiesRobotito$REPRESENTATION = techCheckAbilitiesRobotito$REPRESENTATION/2
techCheckAbilitiesRobotito$CONTROL = techCheckAbilitiesRobotito$CONTROL/2
shapiro.test(techCheckAbilitiesRobotito$TOTAL_R) #p-value = 0.0001209 ALL

techCheckAbilitiesRobotito = techCheckAbilitiesRobotito[order(techCheckAbilitiesRobotito[, "TOTAL_R"], techCheckAbilitiesRobotito[, "ALGORITHMS"]),]

techCheckAbilitiesRobotito$NAME <- factor(techCheckAbilitiesRobotito$NAME, levels = techCheckAbilitiesRobotito$NAME) #important to avoid alphabetic order in y axis
techCheckAbilitiesRobotitoMelted<-melt(techCheckAbilitiesRobotito[,c("NAME","TOTAL_R","ALGORITHMS","REPRESENTATION","CONTROL")])

ggplot(techCheckAbilitiesRobotitoMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


##### techcheck habilities POST and robotito score correlation
install.packages("Hmisc")
cor(techCheckAbilitiesRobotito[,c("ALGORITHMS","REPRESENTATION","CONTROL","TOTAL_R")])
Hmisc::rcorr(as.matrix(techCheckAbilitiesRobotito[,c("ALGORITHMS","REPRESENTATION","CONTROL","TOTAL_R")]),,type = "spearman")

##### techCheck habilities PRE and robotito score correlation
techCheckAbilitiesRobotitoPre = techCheckAbilities[techCheckAbilities$GROUP2=="ACTIVE",]
techCheckAbilitiesRobotitoPre = techCheckAbilitiesRobotitoPre[techCheckAbilitiesRobotitoPre$TYPE=="PRE",]
techCheckAbilitiesRobotitoPre$TOTAL_R = robotitoV2$TOTAL_R/4
techCheckAbilitiesRobotitoPre$ALGORITHMS = techCheckAbilitiesRobotitoPre$ALGORITHMS/5
techCheckAbilitiesRobotitoPre$REPRESENTATION = techCheckAbilitiesRobotitoPre$REPRESENTATION/2
techCheckAbilitiesRobotitoPre$CONTROL = techCheckAbilitiesRobotitoPre$CONTROL/2
Hmisc::rcorr(as.matrix(techCheckAbilitiesRobotitoPre[,c("ALGORITHMS","REPRESENTATION","CONTROL","TOTAL_R")]),type = "spearman")



#### Pre vs  post-pre/(maxVal-pre) 
#techCheckPreDiffMaxVal = techCheckPrePost[,c("NAME","TOTAL_pre")]
techCheckPrePost$diff_max_val = techCheckPrePost$pre_post_diff/(maxT - techCheckPrePost$TOTAL_pre)
techCheckPrePost$NAME <- factor(techCheckPrePost$NAME, levels = techCheckPrePost$NAME) #important to avoid alphabetic order in y axis

library(reshape2)
techCheckPrePostDiffMelted<-melt(techCheckPrePost[,c("NAME","diff_max_val")])

ggplot(techCheckPrePostDiffMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

### variante ordenada con pre en %
techCheckPrePost$TOTAL_pre_perc = techCheckPrePost$TOTAL_pre/maxT # change pre value to %
# techCheckPrePost[order(techCheckPrePost[, 3], techCheckPrePost[, 2]),] #order by pre group and then by pre score

techCheckPrePostToMelt = techCheckPrePost[order(techCheckPrePost[, "GROUP"], techCheckPrePost[, "TOTAL_pre_perc"]),]
techCheckPrePostToMelt$NAME <- factor(techCheckPrePostToMelt$NAME, levels = techCheckPrePostToMelt$NAME)
techCheckPrePostDiffMelted<-melt(techCheckPrePostToMelt[,c("NAME","TOTAL_pre_perc","diff_max_val")])

ggplot(techCheckPrePostDiffMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

### variante ordenada PRE + resultado robotito
# REDUCED poco interesante porque los puntages de robotito son todos buenos
# just active group
techCheckPrePostToMelt = techCheckPrePost[techCheckPrePost$GROUP!="CG",]
# add robotito
techCheckPrePostToMelt$TOTAL_R_perc = robotitoV2$TOTAL_R/4

techCheckPrePostToMelt = techCheckPrePostToMelt[order(techCheckPrePostToMelt[, "GROUP"], techCheckPrePostToMelt[, "diff_max_val"]),]
techCheckPrePostToMelt$NAME <- factor(techCheckPrePostToMelt$NAME, levels = techCheckPrePostToMelt$NAME)

techCheckPrePostDiffMelted<-melt(techCheckPrePostToMelt[,c("NAME","TOTAL_pre_perc","diff_max_val","TOTAL_R_perc")])

ggplot(techCheckPrePostDiffMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

### variante ordenada POST + resultado robotito
# REDUCED poco interesante porque los puntages de robotito son todos buenos
techCheckPrePost$TOTAL_post_perc = techCheckPrePost$TOTAL_post/maxT # change pre value to %
# just active group
techCheckPrePostToMelt = techCheckPrePost[techCheckPrePost$GROUP!="CG",]
# add robotito
techCheckPrePostToMelt$TOTAL_R_perc = robotitoV2$TOTAL_R/4

techCheckPrePostToMelt = techCheckPrePostToMelt[order(techCheckPrePostToMelt[, "TOTAL_post_perc"]),]
techCheckPrePostToMelt$NAME <- factor(techCheckPrePostToMelt$NAME, levels = techCheckPrePostToMelt$NAME)

techCheckPrePostDiffMelted<-melt(techCheckPrePostToMelt[,c("NAME","TOTAL_post_perc","TOTAL_R_perc","diff_max_val")])

ggplot(techCheckPrePostDiffMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

####### Por pregunta
techQper=techCheckQ
for(q in questions9){
  techQper[,colnames(techQper)==q] = techCheckQ[,colnames(techCheckQ)==q]/techCheckQ$TOTAL
}

library("cowplot")
### percentage
P1t =ggplot(data=techQper, aes(x=GROUP, y=techQper[,colnames(techQper)=="P1"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P2t =ggplot(data=techQper, aes(x=GROUP, y=techQper[,colnames(techQper)=="P2"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P3t =ggplot(data=techQper, aes(x=GROUP, y=techQper[,colnames(techQper)=="P3"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P4t =ggplot(data=techQper, aes(x=GROUP, y=techQper[,colnames(techQper)=="P4"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
plot_grid(P1t, P2t, P3t, P4t, labels=c("P1 Alg", "P2 Alg", "P3 Alg", "P4 Alg"), ncol = 2, nrow = 2)
P5t =ggplot(data=techQper, aes(x=GROUP, y=techQper[,colnames(techQper)=="P5"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P6t =ggplot(data=techQper, aes(x=GROUP, y=techQper[,colnames(techQper)=="P6"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P7t =ggplot(data=techQper, aes(x=GROUP, y=techQper[,colnames(techQper)=="P7"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P8t =ggplot(data=techQper, aes(x=GROUP, y=techQper[,colnames(techQper)=="P8"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
plot_grid(P5t, P6t, P7t, P8t, labels=c("P5 Alg", "P6 Rep", "P7 Rep", "P8 Control"), ncol = 2, nrow = 2)
P9t =ggplot(data=techQper, aes(x=GROUP, y=techQper[,colnames(techQper)=="P9"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")

####### Correlación Robotito con una pregunta particular
techCheckRobotitoQuestion <- read.csv("./csv/techCheck.csv",sep = ",")
# add total value
techCheckRobotitoQuestion$GROUP2 = factor(techCheckRobotitoQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("ACTIVE","ACTIVE","PASSIVE"))
# changing TYPE from chr to factor
techCheckRobotitoQuestion$TYPE = factor(techCheckRobotitoQuestion$TYPE, levels=c("PRE","POST"), labels=c("PRE","POST"))
# changing GROUP from chr to factor
techCheckRobotitoQuestion$GROUP = factor(techCheckRobotitoQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("A2","A1","CG"))

techCheckRobotitoQuestion = techCheckRobotitoQuestion[techCheckRobotitoQuestion$GROUP2=="ACTIVE",]
techCheckRobotitoQuestion = techCheckRobotitoQuestion[techCheckRobotitoQuestion$TYPE=="POST",]
robotitoV2RobotitoQuestion <- read.csv("./csv/robotitoV2.csv",sep = ",")
robotitoV2RobotitoQuestion$TOTAL=apply(robotitoV2RobotitoQuestion[,c(3:6)],1,FUN=sum)
colnames(robotitoV2RobotitoQuestion)[7] = "TOTAL_R"

techCheckRobotitoQuestion$TOTAL_R = robotitoV2RobotitoQuestion$TOTAL_R/4
# order by robotito
techCheckRobotitoQuestion = techCheckRobotitoQuestion[order(techCheckRobotitoQuestion[, "TOTAL_R"]),]
techCheckRobotitoQuestion$NAME <- factor(techCheckRobotitoQuestion$NAME, levels = techCheckRobotitoQuestion$NAME)
colnames(techCheckRobotitoQuestion)[4:12] = questions9abilities

techCheckRobotitoQuestionMElted<-melt(techCheckRobotitoQuestion[,c("NAME","TOTAL_R", questions9abilities)])
#techCheckRobotitoQuestionMElted<-melt(techCheckRobotitoQuestion[,c("NAME","TOTAL_R", questions9abilities)][techCheckRobotitoQuestion$GROUP=="A2",]) #only one group


ggplot(techCheckRobotitoQuestionMElted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


####### Correlación Robotito con una pregunta particular
# VALOR POST techCheck
# 1 / pasó de 0 a 1
# 0.5 / se mantuve en 1
# 0 / tuvo 0, está en 0
# -1 / baj'o de 1 a 0

techCheckRobotitoQuestion <- read.csv("./csv/techCheck.csv",sep = ",")
# add total value
techCheckRobotitoQuestion$GROUP2 = factor(techCheckRobotitoQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("ACTIVE","ACTIVE","PASSIVE"))
# changing TYPE from chr to factor
techCheckRobotitoQuestion$TYPE = factor(techCheckRobotitoQuestion$TYPE, levels=c("PRE","POST"), labels=c("PRE","POST"))
# changing GROUP from chr to factor
techCheckRobotitoQuestion$GROUP = factor(techCheckRobotitoQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("A2","A1","CG"))

techCheckRobotitoQuestion = techCheckRobotitoQuestion[techCheckRobotitoQuestion$GROUP2=="ACTIVE",]
techCheckRobotitoQuestion = techCheckRobotitoQuestion[techCheckRobotitoQuestion$TYPE=="POST",]

# substract pre from post
# update 0 values for case pre 1 - post 1 from 0 to 0.5
# recorro preguntas, recorro ninos
for(p in questions9){
  # print(p)
  for(n in techCheckRobotitoQuestion$NAME){
    #  print(n)
    techCheckRobotitoQuestion[techCheckRobotitoQuestion$NAME==n,][,p] = techCheck[which(techCheck$NAME==n & techCheck$TYPE=="POST"),p] - techCheck[which(techCheck$NAME==n & techCheck$TYPE=="PRE"),p]
    if(techCheckRobotitoQuestion[techCheckRobotitoQuestion$NAME==n,][,p] == 0){
      cat(p,n,i,techCheck[which(techCheck$NAME==n & techCheck$TYPE=="PRE"),p],"\n")
      # pre value
      #techCheck[techCheck$NAME==n,][,p][1]
      if(techCheck[which(techCheck$NAME==n & techCheck$TYPE=="PRE"),p] == 1){
        techCheckRobotitoQuestion[techCheckRobotitoQuestion$NAME==n,][,p] = 0.5
      }
    }
  }
}

# add robotito
robotitoV2RobotitoQuestion <- read.csv("./csv/robotitoV2.csv",sep = ",")
robotitoV2RobotitoQuestion$TOTAL=apply(robotitoV2RobotitoQuestion[,c(3:6)],1,FUN=sum)
colnames(robotitoV2RobotitoQuestion)[7] = "TOTAL_R"
techCheckRobotitoQuestion$TOTAL_R = robotitoV2RobotitoQuestion$TOTAL_R/4

# order by robotito
techCheckRobotitoQuestion = techCheckRobotitoQuestion[order(techCheckRobotitoQuestion[, "TOTAL_R"]),]
techCheckRobotitoQuestion$NAME <- factor(techCheckRobotitoQuestion$NAME, levels = techCheckRobotitoQuestion$NAME)
colnames(techCheckRobotitoQuestion)[4:12] = questions9abilities

techCheckRobotitoQuestionMElted<-melt(techCheckRobotitoQuestion[,c("NAME","TOTAL_R", questions9abilities)])

ggplot(techCheckRobotitoQuestionMElted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
