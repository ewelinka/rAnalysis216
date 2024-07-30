source("./init.R")
####### BCTt
bct <- read.csv("./csv/BCTt.csv",sep = ",")
# if we need to exclude the children defined in init.R
if(should_exclude){
  bct = bct[which(!bct$NAME %in% robotitoExclude),]
}
# add total value
bct$TOTAL=apply(bct[,c(4:16)],1,FUN=sum)
bct$GROUP2 = factor(bct$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("ACTIVE","ACTIVE","PASSIVE"))
# changing TYPE from chr to factor
bct$TYPE = factor(bct$TYPE, levels=c("PRE","POST"), labels=c("PRE","POST"))
# changing GROUP from chr to factor
bct$GROUP = factor(bct$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("A2","A1","CG"))
# TOTAL OF GOOD ANSWERS
library(dplyr)  
bctQ = bct %>%                                        
  group_by(GROUP,TYPE) %>% 
  summarise_at(vars(all_of(questions13)), list(name = sum)) %>%
  as.data.frame()

# change names from P1_name, P2_name, etc to P1, P2, etc
colnames(bctQ)[(dim(bctQ)[2]-(13-1)):(dim(bctQ)[2])]=questions13
# add total size of each group
addGroupSize <- function() {
  groupSize=c()
  for (i in 1:dim(bctQ)[1]) {
    if (bctQ$GROUP[i] == "A2"){
      groupSize[i] = dim(bct[bct$TYPE=="PRE",][bct[bct$TYPE=="PRE",]$GROUP=="A2",])[1]
    }else if(bctQ$GROUP[i] == "A1"){
      groupSize[i] = dim(bct[bct$TYPE=="PRE",][bct[bct$TYPE=="PRE",]$GROUP=="A1",])[1]
    } else{
      groupSize[i] = dim(bct[bct$TYPE=="PRE",][bct[bct$TYPE=="PRE",]$GROUP=="CG",])[1]
    }
  }
  bctQ$TOTAL <<- groupSize # <<- important to make the magic happen
}
addGroupSize()

#bctQ$TYPE = factor(bctQ$TYPE, levels=c("PRE","POST"), labels=c("PRE","POST"))
# changing GROUP from chr to factor
#bctQ$GROUP = factor(bctQ$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("A2","A1","CG"))
#bctQ$GROUP2 = factor(bctQ$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("ACTIVE","ACTIVE","PASSIVE"))
rownames(bctQ) = paste(bctQ$TYPE,bctQ$GROUP,sep="-")

## see TOTAL
with(bct, tapply(TOTAL, list(GROUP,TYPE), mean))[c(2,1,3),]
with(bct, tapply(TOTAL, list(GROUP2,TYPE), mean))[,c(2,1)]
############# normal? https://www.statology.org/test-for-normality-in-r/
qqnorm(bct$TOTAL, main='BCTt total')
qqline(bct$TOTAL)

# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.
library("ggpubr")
ggdensity(bct$TOTAL, 
          main = "Density plot of total BCTt score",
          xlab = "Total BCTt score")

# Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution.
# As all the points fall approximately along this reference line, we can assume normality.
ggqqplot(bct$TOTAL)

# The R function shapiro.test() can be used to perform the Shapiro-Wilk test of normality for one variable (univariate):
# From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(bct$TOTAL)

shapiro.test(subset(bct[bct$TYPE=="PRE",], GROUP=="A2")$TOTAL)
shapiro.test(subset(bct[bct$TYPE=="POST",], GROUP=="A2")$TOTAL)
shapiro.test(subset(bct[bct$TYPE=="PRE",], GROUP=="A1")$TOTAL) #falla shapiro p-value = 0.04525 en el excluded
shapiro.test(subset(bct[bct$TYPE=="POST",], GROUP=="A1")$TOTAL)
shapiro.test(subset(bct[bct$TYPE=="PRE",], GROUP=="CG")$TOTAL)
shapiro.test(subset(bct[bct$TYPE=="POST",], GROUP=="CG")$TOTAL)

shapiro.test(subset(bct[bct$TYPE=="PRE",], GROUP2=="ACTIVE")$TOTAL)
shapiro.test(subset(bct[bct$TYPE=="POST",], GROUP2=="ACTIVE")$TOTAL)

####### Correlación pre y post TODOS los grupos
# preB=bct[bct$TYPE=="PRE",] 
# postB=bct[bct$TYPE=="POST",] 
bctPrePost = bct[bct$TYPE=="PRE",][,c("NAME","TOTAL","GROUP")] # pre name and total
colnames(bctPrePost)[2] = "TOTAL_pre"
bctPrePost$TOTAL_post = bct[bct$TYPE=="POST",]$TOTAL # total post
ggscatter(bctPrePost, x = "TOTAL_pre", y = "TOTAL_post", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Pre BCTt score", ylab = "Post BCTt score")

shapiro.test(bctPrePost$TOTAL_pre) #ok
shapiro.test(bctPrePost$TOTAL_post) #ok
cor.test(bctPrePost$TOTAL_pre,bctPrePost$TOTAL_post) #ya lo vimos en el ggscatter

####### Correlación pre y diff pre-post TODOS los grupos
bctPrePost$pre_post_diff <- bctPrePost$TOTAL_post-bctPrePost$TOTAL_pre
ggscatter(bctPrePost, x = "TOTAL_pre", y = "pre_post_diff", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Pre BCTt score", ylab = "Diff BCTt pre post scores")
shapiro.test(bctPrePost$TOTAL_pre) #ok
shapiro.test(bctPrePost$pre_post_diff) #ok
ggqqplot(bctPrePost$TOTAL_pre, ylab = "TOTAL")
ggqqplot(bctPrePost$pre_post_diff, ylab = "bct_pre_post_diff")
cor.test(bctPrePost$TOTAL_pre, bctPrePost$pre_post_diff, method = "pearson") #ok, ya lo vimos en el scatterplot
####### Correlación pre y diff pre-post por grupo
groups <- c("A2", "A1", "CG") 
for(g in groups) {    
  print(g)
  current_group = subset(bctPrePost, GROUP==g)
  r= shapiro.test(current_group$TOTAL_pre)
  print(r)
  r= shapiro.test(current_group$pre_post_diff)
  print(r)
  r= cor.test(current_group$TOTAL_pre, current_group$pre_post_diff, method = "pearson")
  print(r)
}
####### Correlación pre y post por grupo
# solo ELO da significativo en reducido
for(g in groups) {    
  print(g)
  current_group_pre = subset(bctPrePost, GROUP==g)
  current_group_post = subset(bctPrePost, GROUP==g)
  r= shapiro.test(current_group_pre$TOTAL_pre)
  print(r)
  r= shapiro.test(current_group_post$TOTAL_post)
  print(r)
  r= cor.test(current_group_pre$TOTAL_pre, current_group_post$TOTAL_post, method = "pearson")
  print(r)
}

####### preparing data for factorial annova
# Check cell sizes are equal (ish) using replications()
replications(TOTAL ~ TYPE * GROUP, data=bct)
bct$GROUP = factor(bct$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("A2","A1","CG"))
boxplot(TOTAL ~ TYPE * GROUP, data=bct, ylab="Total score", main="Boxplots of BCTt total scores")


# grouped by active and passive
boxplot(TOTAL ~ TYPE * GROUP2, data=bct, ylab="Total score", main="Boxplots of BCTt total scores")

aggregate( TOTAL ~ TYPE * GROUP2, bct, mean )
aggregate( TOTAL ~ TYPE * GROUP, bct, mean )
summary(aov(TOTAL ~ TYPE * GROUP, data = bct))
summary(aov(TOTAL ~ TYPE * GROUP2, data = bct))
#### GROUPED BY ABILITY
#### P1-P6 SEQUENCES, P7-P8 IF, P9 SIMPLE, P10-P13 NESTED
# select NAME, TYPE and GROUP
bctAbilities <- bct[,c(1:3)]
bctAbilities$GROUP2 = bct$GROUP2 
bctAbilities$SEQUENCES = bct$P1+bct$P2+bct$P3+bct$P4+bct$P5+bct$P6
bctAbilities$IF = bct$P7+bct$P8
bctAbilities$SIMPLE = bct$P9
bctAbilities$NESTED = bct$P10+bct$P11+bct$P12+bct$P13

## SEQUENCES
boxplot(SEQUENCES ~ TYPE * GROUP, data=bctAbilities, ylab="Score in Sequences", main="Boxplots of BCTt Sequences scores")
boxplot(SEQUENCES ~ TYPE * GROUP2, data=bctAbilities, ylab="Score in Sequences", main="Boxplots of BCTt Sequences scores")
aggregate( SEQUENCES ~ TYPE * GROUP2, bctAbilities, mean )
aggregate( SEQUENCES ~ TYPE * GROUP, bctAbilities, mean )
summary(aov(SEQUENCES ~ TYPE * GROUP, data = bctAbilities))
summary(aov(SEQUENCES ~ TYPE * GROUP2, data = bctAbilities))
## IF
boxplot(IF ~ TYPE * GROUP, data=bctAbilities, ylab="Score in If-then", main="Boxplots of BCTt If-then scores")
boxplot(IF ~ TYPE * GROUP2, data=bctAbilities, ylab="Score in If-then", main="Boxplots of BCTt If-then scores")
aggregate( IF ~ TYPE * GROUP2, bctAbilities, mean )
aggregate( IF ~ TYPE * GROUP, bctAbilities, mean )
summary(aov(IF ~ TYPE * GROUP, data = bctAbilities))
summary(aov(IF ~ TYPE * GROUP2, data = bctAbilities))
## NESTED
boxplot(NESTED ~ TYPE * GROUP, data=bctAbilities, ylab="Score in Nested loops", main="Boxplots of BCTt Nested loops scores")
boxplot(NESTED ~ TYPE * GROUP2, data=bctAbilities, ylab="Score in Nested loops", main="Boxplots of BCTt Nested loops scores")
aggregate( NESTED ~ TYPE * GROUP2, bctAbilities, mean )
aggregate( NESTED ~ TYPE * GROUP, bctAbilities, mean )
summary(aov(NESTED ~ TYPE * GROUP, data = bctAbilities))
summary(aov(NESTED ~ TYPE * GROUP2, data = bctAbilities))
TukeyHSD(aov(NESTED ~ TYPE * GROUP, data = bctAbilities))

### bctAbilities pre-post ACTIVE not significant
summary(aov(SEQUENCES~TYPE,bctAbilities[bctAbilities$GROUP2=="ACTIVE",]))
summary(aov(IF~TYPE,bctAbilities[bctAbilities$GROUP2=="ACTIVE",]))
summary(aov(NESTED~TYPE,bctAbilities[bctAbilities$GROUP2=="ACTIVE",]))
## abilities and robotito
bctAbilitiesRobotito = bctAbilities[bctAbilities$GROUP!="ELO",]
bctAbilitiesRobotito = bctAbilitiesRobotito[bctAbilitiesRobotito$TYPE=="POST",]
bctAbilitiesRobotito$TOTAL_R = robotitoV2$TOTAL_R/4
bctAbilitiesRobotito$SEQUENCES = bctAbilitiesRobotito$SEQUENCES/6
bctAbilitiesRobotito$IF = bctAbilitiesRobotito$IF/2
bctAbilitiesRobotito$NESTED = bctAbilitiesRobotito$NESTED/4
shapiro.test(bctAbilitiesRobotito$TOTAL_R) #p-value = 0.0001209 ALL

#bctAbilitiesRobotito = bctAbilitiesRobotito[order(bctAbilitiesRobotito[, "GROUP"], bctAbilitiesRobotito[, "TOTAL_R"]),]
bctAbilitiesRobotito = bctAbilitiesRobotito[order(bctAbilitiesRobotito[, "TOTAL_R"], bctAbilitiesRobotito[, "SEQUENCES"]),]

bctAbilitiesRobotito$NAME <- factor(bctAbilitiesRobotito$NAME, levels = bctAbilitiesRobotito$NAME) #important to avoid alphabetic order in y axis
bctAbilitiesRobotitoMelted<-melt(bctAbilitiesRobotito[,c("NAME","TOTAL_R","SEQUENCES","IF","SIMPLE","NESTED")])

ggplot(bctAbilitiesRobotitoMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

##### bct habilities POST and robotito score correlation
install.packages("Hmisc")
cor(bctAbilitiesRobotito[,c("SEQUENCES","IF","SIMPLE","NESTED","TOTAL_R")])
Hmisc::rcorr(as.matrix(bctAbilitiesRobotito[,c("SEQUENCES","IF","SIMPLE","NESTED","TOTAL_R")]),type = "spearman")
# Sequencing dio bien, a ver como estaba en pre (miro pre activo ya que solo activos tienen robotito test)
bctAbilitiesSequencesPre = filter(bctAbilities,TYPE=="PRE" & GROUP2=="ACTIVE")$SEQUENCES/6
Hmisc::rcorr(cbind(bctAbilitiesRobotito[,c("TOTAL_R")],bctAbilitiesSequencesPre),type = "spearman")
# double check post -- OK!
bctAbilitiesSequencesPost = filter(bctAbilities,TYPE=="POST" & GROUP2=="ACTIVE")$SEQUENCES/6
Hmisc::rcorr(cbind(bctAbilitiesRobotito[,c("TOTAL_R")],bctAbilitiesSequencesPost),type = "spearman")

##### bct habilities PRE and robotito score correlation
## abilities and robotito
bctAbilitiesRobotitoPre = bctAbilities[bctAbilities$GROUP!="ELO",]
bctAbilitiesRobotitoPre = bctAbilitiesRobotitoPre[bctAbilitiesRobotitoPre$TYPE=="PRE",]
bctAbilitiesRobotitoPre$TOTAL_R = robotitoV2$TOTAL_R/4
bctAbilitiesRobotitoPre$SEQUENCES = bctAbilitiesRobotitoPre$SEQUENCES/6
bctAbilitiesRobotitoPre$IF = bctAbilitiesRobotitoPre$IF/2
bctAbilitiesRobotitoPre$NESTED = bctAbilitiesRobotitoPre$NESTED/4
Hmisc::rcorr(as.matrix(bctAbilitiesRobotitoPre[,c("SEQUENCES","IF","SIMPLE","NESTED","TOTAL_R")]),type = "spearman")

#### Pre vs  post-pre/(maxVal-pre) 
#bctPreDiffMaxVal = bctPrePost[,c("NAME","TOTAL_pre")]
bctPrePost$diff_max_val = bctPrePost$pre_post_diff/(maxB - bctPrePost$TOTAL_pre)
bctPrePost$NAME <- factor(bctPrePost$NAME, levels = bctPrePost$NAME) #important to avoid alphabetic order in y axis

library(reshape2)
bctPrePostDiffMelted<-melt(bctPrePost[,c("NAME","diff_max_val")])

ggplot(bctPrePostDiffMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

### variante ordenada con pre en %
bctPrePost$TOTAL_pre_perc = bctPrePost$TOTAL_pre/maxB # change pre value to %
# bctPrePost[order(bctPrePost[, 3], bctPrePost[, 2]),] #order by pre group and then by pre score

bctPrePostToMelt = bctPrePost[order(bctPrePost[, "GROUP"], bctPrePost[, "TOTAL_pre_perc"]),]
bctPrePostToMelt$NAME <- factor(bctPrePostToMelt$NAME, levels = bctPrePostToMelt$NAME)
bctPrePostDiffMelted<-melt(bctPrePostToMelt[,c("NAME","TOTAL_pre_perc","diff_max_val")])

ggplot(bctPrePostDiffMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

### variante ordenada PRE + resultado robotito
# just active group
bctPrePostToMelt = bctPrePost[bctPrePost$GROUP!="CG",]
# add robotito
bctPrePostToMelt$TOTAL_R_perc = robotitoV2$TOTAL_R/4

bctPrePostToMelt = bctPrePostToMelt[order(bctPrePostToMelt[, "GROUP"], bctPrePostToMelt[, "diff_max_val"]),]
bctPrePostToMelt$NAME <- factor(bctPrePostToMelt$NAME, levels = bctPrePostToMelt$NAME)

bctPrePostDiffMelted<-melt(bctPrePostToMelt[,c("NAME","TOTAL_pre_perc","diff_max_val","TOTAL_R_perc")])

ggplot(bctPrePostDiffMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

### variante ordenada POST + resultado robotito
bctPrePost$TOTAL_post_perc = bctPrePost$TOTAL_post/maxB # change pre value to %
# just active group
bctPrePostToMelt = bctPrePost[bctPrePost$GROUP!="CG",]
# add robotito
bctPrePostToMelt$TOTAL_R_perc = robotitoV2$TOTAL_R/4

bctPrePostToMelt = bctPrePostToMelt[order(bctPrePostToMelt[, "TOTAL_post_perc"]),]
bctPrePostToMelt$NAME <- factor(bctPrePostToMelt$NAME, levels = bctPrePostToMelt$NAME)

bctPrePostDiffMelted<-melt(bctPrePostToMelt[,c("NAME","TOTAL_post_perc","TOTAL_R_perc","diff_max_val")])

ggplot(bctPrePostDiffMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
####### Por pregunta

#par(mfrow=c(2,2))
library("cowplot")

# me jode que se guarda con y=bctQ[,colnames(bctQ)==q] y al final q queda en el 'ultimo valor para TODOS
for(q in questions13){
  print(bctQ[,colnames(bctQ)==q])
  #q <- ggplot(data=bctQ, aes(x=GROUP, y=bctQ[,colnames(bctQ)==q], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge())
  assign(q,ggplot(data=bctQ, aes(x=GROUP, y=bctQ[,colnames(bctQ)==q], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()))
  #do.call("=",list(q, currentPlot))
}

# PERCENTAGE OF GOOD ANSWERS
bctQper=bctQ
for(q in questions13){
  bctQper[,colnames(bctQper)==q] = bctQ[,colnames(bctQ)==q]/bctQ$TOTAL
}


P1t =ggplot(data=bctQ, aes(x=GROUP, y=bctQ[,colnames(bctQ)=="P1"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Total of good answers")
P2t =ggplot(data=bctQ, aes(x=GROUP, y=bctQ[,colnames(bctQ)=="P2"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Total of good answers")
P3t =ggplot(data=bctQ, aes(x=GROUP, y=bctQ[,colnames(bctQ)=="P3"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Total of good answers")
P4t =ggplot(data=bctQ, aes(x=GROUP, y=bctQ[,colnames(bctQ)=="P4"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Total of good answers")
plot_grid(P1t, P2t, P3t, P4t, labels=c("P1", "P2", "P3", "P4"), ncol = 2, nrow = 2)

### percentage ONLY THING THAT WORKS!
P1 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P1"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P2 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P2"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P3 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P3"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P4 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P4"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
plot_grid(P1, P2, P3, P4, labels=c("P1 Seq", "P2 Seq", "P3 Seq", "P4 Seq"), ncol = 2, nrow = 2)
P5 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P5"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P6 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P6"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P7 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P7"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P8 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P8"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
plot_grid(P5, P6, P7, P8, labels=c("P5 Seq", "P6 Seq", "P7 If", "P8 If"), ncol = 2, nrow = 2)
P9 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P9"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P10 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P10"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P11 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P11"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
P12 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P12"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")
plot_grid(P9, P10, P11, P12, labels=c("P9 sLoop", "P10 nLoop", "P11 nLoop", "P12 nLoop"), ncol = 2, nrow = 2)
P13 =ggplot(data=bctQper, aes(x=GROUP, y=bctQper[,colnames(bctQper)=="P13"], fill=TYPE)) + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of good answers")



####### Correlación Robotito con una pregunta particular
bctRobotitoQuestion <- read.csv("./csv/BCTt.csv",sep = ",")
# add total value
bctRobotitoQuestion$GROUP2 = factor(bctRobotitoQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("ACTIVE","ACTIVE","PASSIVE"))
# changing TYPE from chr to factor
bctRobotitoQuestion$TYPE = factor(bctRobotitoQuestion$TYPE, levels=c("PRE","POST"), labels=c("PRE","POST"))
# changing GROUP from chr to factor
bctRobotitoQuestion$GROUP = factor(bctRobotitoQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("A2","A1","CG"))

bctRobotitoQuestion = bctRobotitoQuestion[bctRobotitoQuestion$GROUP2=="ACTIVE",]
bctRobotitoQuestion = bctRobotitoQuestion[bctRobotitoQuestion$TYPE=="POST",]
robotitoV2RobotitoQuestion <- read.csv("./csv/robotitoV2.csv",sep = ",")
robotitoV2RobotitoQuestion$TOTAL=apply(robotitoV2RobotitoQuestion[,c(3:6)],1,FUN=sum)
colnames(robotitoV2RobotitoQuestion)[7] = "TOTAL_R"

bctRobotitoQuestion$TOTAL_R = robotitoV2RobotitoQuestion$TOTAL_R/4
# order by robotito
bctRobotitoQuestion = bctRobotitoQuestion[order(bctRobotitoQuestion[, "TOTAL_R"]),]
bctRobotitoQuestion$NAME <- factor(bctRobotitoQuestion$NAME, levels = bctRobotitoQuestion$NAME)
colnames(bctRobotitoQuestion)[4:16] = questions13abilities

bctRobotitoQuestionMElted<-melt(bctRobotitoQuestion[,c("NAME","TOTAL_R", questions13abilities)][bctRobotitoQuestion$GROUP=="A2",])

ggplot(bctRobotitoQuestionMElted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


####### Correlación Robotito con una pregunta particular
# VALOR POST BCT
# 1 / pasó de 0 a 1
# 0.5 / se mantuve en 1
# 0 / tuvo 0, está en 0
# -1 / baj'o de 1 a 0

bctRobotitoQuestion <- read.csv("./csv/BCTt.csv",sep = ",")
# add total value
bctRobotitoQuestion$GROUP2 = factor(bctRobotitoQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("ACTIVE","ACTIVE","PASSIVE"))
# changing TYPE from chr to factor
bctRobotitoQuestion$TYPE = factor(bctRobotitoQuestion$TYPE, levels=c("PRE","POST"), labels=c("PRE","POST"))
# changing GROUP from chr to factor
bctRobotitoQuestion$GROUP = factor(bctRobotitoQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("A2","A1","CG"))

bctRobotitoQuestion = bctRobotitoQuestion[bctRobotitoQuestion$GROUP2=="ACTIVE",]
bctRobotitoQuestion = bctRobotitoQuestion[bctRobotitoQuestion$TYPE=="POST",]

# substract pre from post
# update 0 values for case pre 1 - post 1 from 0 to 0.5
# recorro preguntas, recorro ninos
for(p in questions13){
 # print(p)
  for(n in bctRobotitoQuestion$NAME){
  #  print(n)
    bctRobotitoQuestion[bctRobotitoQuestion$NAME==n,][,p] = bct[which(bct$NAME==n & bct$TYPE=="POST"),p] - bct[which(bct$NAME==n & bct$TYPE=="PRE"),p]
    if(bctRobotitoQuestion[bctRobotitoQuestion$NAME==n,][,p] == 0){
      cat(p,n,i,bct[which(bct$NAME==n & bct$TYPE=="PRE"),p],"\n")
      # pre value
      #bct[bct$NAME==n,][,p][1]
      if(bct[which(bct$NAME==n & bct$TYPE=="PRE"),p] == 1){
        bctRobotitoQuestion[bctRobotitoQuestion$NAME==n,][,p] = 0.5
      }
    }
  }
}

# add robotito
robotitoV2RobotitoQuestion <- read.csv("./csv/robotitoV2.csv",sep = ",")
robotitoV2RobotitoQuestion$TOTAL=apply(robotitoV2RobotitoQuestion[,c(3:6)],1,FUN=sum)
colnames(robotitoV2RobotitoQuestion)[7] = "TOTAL_R"
bctRobotitoQuestion$TOTAL_R = robotitoV2RobotitoQuestion$TOTAL_R/4

# order by robotito
bctRobotitoQuestion = bctRobotitoQuestion[order(bctRobotitoQuestion[, "TOTAL_R"],bctRobotitoQuestion[, "P1"],bctRobotitoQuestion[, "P2"],bctRobotitoQuestion[, "P3"],bctRobotitoQuestion[, "P4"],bctRobotitoQuestion[, "P5"],bctRobotitoQuestion[, "P6"]),]
bctRobotitoQuestion$NAME <- factor(bctRobotitoQuestion$NAME, levels = bctRobotitoQuestion$NAME)
colnames(bctRobotitoQuestion)[4:16] = questions13abilities

bctRobotitoQuestionMElted<-melt(bctRobotitoQuestion[,c("NAME","TOTAL_R", questions13abilities)])


ggplot(bctRobotitoQuestionMElted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


##### pre post BCT todos los grupos
# VALOR POST BCT
# 1 / pasó de 0 a 1
# 0.5 / se mantuve en 1
# 0 / tuvo 0, está en 0
# -1 / baj'o de 1 a 0

bctQuestion <- read.csv("./csv/BCTt.csv",sep = ",")
# add total value
bctQuestion$GROUP2 = factor(bctQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("ACTIVE","ACTIVE","PASSIVE"))
# changing TYPE from chr to factor
bctQuestion$TYPE = factor(bctQuestion$TYPE, levels=c("PRE","POST"), labels=c("PRE","POST"))
# changing GROUP from chr to factor
bctQuestion$GROUP = factor(bctQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("A2","A1","CG"))

bctQuestion = bctQuestion[bctQuestion$TYPE=="POST",]

# substract pre from post
# update 0 values for case pre 1 - post 1 from 0 to 0.5
# recorro preguntas, recorro ninos
for(p in questions13){
  # print(p)
  for(n in bctQuestion$NAME){
    #  print(n)
    bctQuestion[bctQuestion$NAME==n,][,p] = bct[which(bct$NAME==n & bct$TYPE=="POST"),p] - bct[which(bct$NAME==n & bct$TYPE=="PRE"),p]
    if(bctQuestion[bctQuestion$NAME==n,][,p] == 0){
      cat(p,n,i,bct[which(bct$NAME==n & bct$TYPE=="PRE"),p],"\n")
      # pre value
      #bct[bct$NAME==n,][,p][1]
      if(bct[which(bct$NAME==n & bct$TYPE=="PRE"),p] == 1){
        bctQuestion[bctQuestion$NAME==n,][,p] = 0.5
      }
    }
  }
}

# add robotito
# order by robotito
bctQuestion = bctQuestion[order(bctQuestion[, "GROUP"],bctQuestion[, "P1"],bctQuestion[, "P2"],bctQuestion[, "P3"]),]
bctQuestion$NAME <- factor(bctQuestion$NAME, levels = bctQuestion$NAME)
colnames(bctQuestion)[4:16] = questions13abilities

bctQuestionMelted<-melt(bctQuestion[,c("NAME", questions13abilities)][bctQuestion$GROUP=="CG",])

ggplot(bctQuestionMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


####### Correlación Robotito con una preguntas de Sequenciacion
# VALOR POST BCT
# 1 / pasó de 0 a 1
# 0.5 / se mantuve en 1
# 0 / tuvo 0, está en 0
# -1 / baj'o de 1 a 0

bctRobotitoQuestion <- read.csv("./csv/BCTt.csv",sep = ",")
# add total value
bctRobotitoQuestion$GROUP2 = factor(bctRobotitoQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("ACTIVE","ACTIVE","PASSIVE"))
# changing TYPE from chr to factor
bctRobotitoQuestion$TYPE = factor(bctRobotitoQuestion$TYPE, levels=c("PRE","POST"), labels=c("PRE","POST"))
# changing GROUP from chr to factor
bctRobotitoQuestion$GROUP = factor(bctRobotitoQuestion$GROUP, levels=c("ALE","SOFI","ELO"), labels=c("A2","A1","CG"))

bctRobotitoQuestion = bctRobotitoQuestion[bctRobotitoQuestion$GROUP2=="ACTIVE",]
bctRobotitoQuestion = bctRobotitoQuestion[bctRobotitoQuestion$TYPE=="POST",]

# substract pre from post
# update 0 values for case pre 1 - post 1 from 0 to 0.5
# recorro preguntas, recorro ninos
for(p in questions13){
  # print(p)
  for(n in bctRobotitoQuestion$NAME){
    #  print(n)
    bctRobotitoQuestion[bctRobotitoQuestion$NAME==n,][,p] = bct[which(bct$NAME==n & bct$TYPE=="POST"),p] - bct[which(bct$NAME==n & bct$TYPE=="PRE"),p]
    if(bctRobotitoQuestion[bctRobotitoQuestion$NAME==n,][,p] == 0){
      cat(p,n,i,bct[which(bct$NAME==n & bct$TYPE=="PRE"),p],"\n")
      # pre value
      #bct[bct$NAME==n,][,p][1]
      if(bct[which(bct$NAME==n & bct$TYPE=="PRE"),p] == 1){
        bctRobotitoQuestion[bctRobotitoQuestion$NAME==n,][,p] = 0.5
      }
    }
  }
}

# add robotito
robotitoV2RobotitoQuestion <- read.csv("./csv/robotitoV2.csv",sep = ",")
robotitoV2RobotitoQuestion$TOTAL=apply(robotitoV2RobotitoQuestion[,c(3:6)],1,FUN=sum)
colnames(robotitoV2RobotitoQuestion)[7] = "TOTAL_R"
bctRobotitoQuestion$TOTAL_R = robotitoV2RobotitoQuestion$TOTAL_R/4

# order by robotito
bctRobotitoQuestion = bctRobotitoQuestion[order(bctRobotitoQuestion[, "TOTAL_R"],bctRobotitoQuestion[, "P1"],bctRobotitoQuestion[, "P2"],bctRobotitoQuestion[, "P3"],bctRobotitoQuestion[, "P4"],bctRobotitoQuestion[, "P5"],bctRobotitoQuestion[, "P6"]),]
bctRobotitoQuestion$NAME <- factor(bctRobotitoQuestion$NAME, levels = bctRobotitoQuestion$NAME)
colnames(bctRobotitoQuestion)[4:16] = questions13abilities

bctRobotitoQuestionMElted<-melt(bctRobotitoQuestion[,c("NAME","TOTAL_R", questionsSeqAbilities)])


ggplot(bctRobotitoQuestionMElted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

###### bctt difficulty index zapata
bctDifficulty <- read.csv("./csv/BCTtDifficulty.csv",sep = ",")
cor.test(bctDifficulty$ESTIMATEDSCORE,bctDifficulty$PRESCORE, method="spearman")
cor.test(bctDifficulty$ESTIMATEDSCORE,bctDifficulty$PREA1, method="spearman")
cor.test(bctDifficulty$ESTIMATEDSCORE,bctDifficulty$PREA2, method="spearman")
cor.test(bctDifficulty$ESTIMATEDSCORE,bctDifficulty$PRECG, method="spearman")
