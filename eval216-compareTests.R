robotito <- read.csv("/Users/ewe/PhD/eval216/robotito.csv",sep = ",")
robotito$TOTAL=apply(robotito[,c(3:6)],1,FUN=sum)
hist(robotito[robotito$GROUP=="ALE",]$TOTAL)

robotitoGroup <- read.csv("/Users/ewe/PhD/eval216/robotitoGroup.csv",sep = ",")
# change to percentage as the number of children differs
robotitoGroup[robotitoGroup=="A2",c(2:5)] = robotitoGroup[robotitoGroup=="A2",c(2:5)]/19
robotitoGroup[robotitoGroup=="A1",c(2:5)] = robotitoGroup[robotitoGroup=="A1",c(2:5)]/17

robotitoGroupMelted = melt(robotitoGroup)
ggplot(robotitoGroupMelted, aes(fill=GROUP, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity")

robotitoV2 <- read.csv("/Users/ewe/PhD/eval216/robotitoV2.csv",sep = ",")
robotitoV2$TOTAL=apply(robotitoV2[,c(3:6)],1,FUN=sum)
colnames(robotitoV2)[7] = "TOTAL_R"

#### comparing ranked pre - post differences 
allTestsActiveGroup <- robotito[,c(1,2,7)] # names, group and total robotito score
colnames(allTestsActiveGroup)[3] = "TOTAL_R"
allTestsActiveGroup$TOTAL_R_rank = rank(allTestsActiveGroup$TOTAL_R)

allTestsActiveGroup$TOTAL_T_pre = activePreT$TOTAL
allTestsActiveGroup$TOTAL_T_post = activePostT$TOTAL
allTestsActiveGroup$TOTAL_T_preRank = rank(allTestsActiveGroup$TOTAL_T_pre) # higher rank = better score
allTestsActiveGroup$TOTAL_T_postRank = rank(allTestsActiveGroup$TOTAL_T_post)
allTestsActiveGroup$TOTAL_T_rankDiff = allTestsActiveGroup$TOTAL_T_postRank - allTestsActiveGroup$TOTAL_T_preRank # higher diff = better improver

allTestsActiveGroup$TOTAL_B_pre = activePreB$TOTAL
allTestsActiveGroup$TOTAL_B_post = activePostB$TOTAL
allTestsActiveGroup$TOTAL_B_preRank = rank(allTestsActiveGroup$TOTAL_B_pre) # higher rank = better score
allTestsActiveGroup$TOTAL_B_postRank = rank(allTestsActiveGroup$TOTAL_B_post)
allTestsActiveGroup$TOTAL_B_rankDiff = allTestsActiveGroup$TOTAL_B_postRank - allTestsActiveGroup$TOTAL_B_preRank # higher diff = better improver

# correlation between total scores
library("ggpubr")
ggscatter(allTestsActiveGroup, x = "TOTAL_R", y = "TOTAL_B_post", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Robotito score", ylab = "BCTt score")
#Is the covariation linear? NO!!

# ranked differences / higher rank = higher difference
ranks <- data.frame(allTestsActiveGroup$NAME, allTestsActiveGroup$TOTAL_R_rank, rank(allTestsActiveGroup$TOTAL_T_rankDiff), rank(allTestsActiveGroup$TOTAL_B_rankDiff))
colnames(ranks) = c("NAME","TOTAL_R_rank", "TOTAL_T_rankDiff_rank", "TOTAL_B_rankDiff_rank")
# differences between bct and robotito are very diverse 
sort(ranks$TOTAL_B_rankDiff_rank - ranks$TOTAL_R_rank)
# between bct and techcheck too
sort(ranks$TOTAL_B_rankDiff_rank - ranks$TOTAL_T_rankDiff_rank)

### COMO LA DISTRIBUCION NO ES NORMAL, ES MAS UNIFORME (se ve en hist(unlist(ranks[,c(2, 3, 4)]))) no se deberia hacer esto no?
summary(aov(TOTAL_B_rankDiff_rank ~ TOTAL_R_rank, data = ranks)) # robotito no predice bct
summary(aov(TOTAL_B_rankDiff_rank ~ TOTAL_T_rankDiff_rank, data = ranks)) # techcheck no predice bct
summary(aov(TOTAL_T_rankDiff_rank ~ TOTAL_R_rank, data = ranks)) # robotito no predice techcheck

####### totals as percentage correlation
allTotals <- allTestsActiveGroup[,c("NAME","GROUP")]
allTotals$TOTAL_R_per <- allTestsActiveGroup$TOTAL_R/4 # 4 total points Robotito
allTotals$TOTAL_T_pre_per <- allTestsActiveGroup$TOTAL_T_pre/9 # 9 total points TechCheck
allTotals$TOTAL_T_post_per <- allTestsActiveGroup$TOTAL_T_post/9

allTotals$TOTAL_B_pre_per <- allTestsActiveGroup$TOTAL_B_pre/13 # 13 total points TechCheck
allTotals$TOTAL_B_post_per <- allTestsActiveGroup$TOTAL_B_post/13

allTotals[,c(1,3,5,7)] # name / robotito per / t post per / b post per

## no se debe hacer porque TOTAL_R_per no esta normal distribution
#### http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
cor.test(allTotals$TOTAL_R_per, allTotals$TOTAL_T_post_per, method = "pearson")

####### Correlación pre bct y techcheck todos los grupos
shapiro.test(preT$TOTAL)
shapiro.test(preB$TOTAL)
cor.test(preT$TOTAL,preB$TOTAL)

####### Correlación post bct y techcheck todos los grupos
shapiro.test(postT$TOTAL)
shapiro.test(postB$TOTAL)
cor.test(postT$TOTAL,postB$TOTAL)

########### matrix with color pre-post differences bct and techCheck
#install.packages("reshape2")
library("reshape2")
prePostDiffMatrix = matrix(c((bctPrePost$TOTAL_post-bctPrePost$TOTAL_pre)/13, (techcheckPrePost$TOTAL_post-techcheckPrePost$TOTAL_pre)/9), ncol = 2, byrow = FALSE)
colnames(prePostDiffMatrix) = c("BCTt","TechCheck")
rownames(prePostDiffMatrix) = bctPrePost$NAME
print(prePostDiffMatrix)
#plot(prePostDiffMatrix)

prePostDiffMatrixMelted<-melt(prePostDiffMatrix)

ggplot(prePostDiffMatrixMelted, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

########### ########### ########### ########### ########### ###########
allTests=bctPrePost
colnames(allTests)[2]<-"bct_pre"
colnames(allTests)[3]<-"bct_post"
allTests$bct_pre_per = round(allTests$bct_pre/13,2)
allTests$bct_post_per = round(allTests$bct_post/13,2)
allTests = allTests[c(1,5,6,2,3,8,7)]
allTests$tech_pre = techcheckPrePost$TOTAL_pre
allTests$tech_post = techcheckPrePost$TOTAL_post
allTests$tech_pre_per = round(allTests$tech_pre/9,2)
allTests$tech_post_per = round(allTests$tech_post/9,2)
########### test pre and post separated
allTestsMini = allTests[,c("NAME", "bct_pre_per", "bct_post_per", "tech_pre_per", "tech_post_per")]
rownames(allTestsMini) = allTestsMini$NAME
allTestsMini$NAME <- factor(allTestsMini$NAME, levels = allTestsMini$NAME) #important to avoid alphabetic order in y axis

BTMelted<-melt(allTestsMini)

ggplot(BTMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
########### all children all questions
### TechCheck
techM = activePreT[,c(1,4,5,6,7,8,9,10,11,12)] 
techM = rbind(techM,passivePreT[,c(1,4,5,6,7,8,9,10,11,12)] )
colnames(techM)[c(2:10)]=paste(colnames(techM)[c(2:10)],"pre",sep="-")

techM[c(1:36),c(11:19)]=activePostT[,c(4,5,6,7,8,9,10,11,12)]
techM[c(37:56),c(11:19)]=passivePostT[,c(4,5,6,7,8,9,10,11,12)]
colnames(techM)[c(11:19)]=paste(colnames(techM)[c(11:19)],"post",sep="-")

#order pre with post
techM= techM[,c(1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)]

techM$NAME <- factor(techM$NAME, levels = techM$NAME) 
techMMelted = melt(techM)

ggplot(techMMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix showin good and bad answers in TechCheck pre and post") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

## post minus pre
techPP = activePostT[,c(1,c(4:12))] 
d = dim(techPP)
techPP[,c(2:d[2])] = techPP[,c(2:dim(techPP)[2])] - activePreT[,c(4:12)] 

techPP = rbind(techPP, passivePostT[,c(1,c(4:12))])
techPP[c((d[1]+1):dim(techPP)[1]),c(2:d[2])] = techPP[c((d[1]+1):dim(techPP)[1]),c(2:d[2])]- passivePreT[,c(4:12)] 

techPP$NAME <- factor(techPP$NAME, levels = techPP$NAME) 
techPPMelted = melt(techPP)

ggplot(techPPMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Question", y="Name", title="Matrix showing differences in answers TechCheck") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


## post minus pre with 0.5 (not 0) if we had 1 and stayed with 1 


#### solo sofi % pre vs post-pre %
sofiT = techcheckPrePost[techcheckPrePost$GROUP=="SOFI",c(1,2,3)]
sofiT$pre_post_diff = sofiT$TOTAL_post-sofiT$TOTAL_pre
sofiT$pre_post_diff = sofiT$pre_post_diff/9
sofiT$TOTAL_pre = sofiT$TOTAL_pre/9
sofiT=sofiT[,c(1,2,4)]

sofiT = sofiT[order(sofiT[, 2], sofiT[, 3]),] #order by pre value and then by the diff
sofiT$NAME <- factor(sofiT$NAME, levels = sofiT$NAME) 
sofiTMelted =melt(sofiT)
ggplot(sofiTMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

### BCTt
#pre
bctM = activePreB[,c(1,c(4:16))] 
bctM = rbind(bctM,passivePreB[,c(1,c(4:16))] )
colnames(bctM)[c(2:dim(bctM)[2])]=paste(colnames(bctM)[c(2:dim(bctM)[2])],"pre",sep="-")
#post
# bctM =activePreB[,c(1)]
bctM = cbind(bctM,rbind(activePostB[,c(4:16)],passivePostB[,c(4:16)]))

colnames(bctM)[c(15:dim(bctM)[2])]=paste(colnames(bctM)[c(15:dim(bctM)[2])],"post",sep="-")

#order pre with post
bctM= bctM[,c(1,c(rbind(c(2:14),c(15:dim(bctM)[2]))))]

bctM$NAME <- factor(bctM$NAME, levels = bctM$NAME) 
bctMMelted = melt(bctM)

ggplot(bctMMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Question", y="Name", title="Matrix showin good and bad answers in BCTt post") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
## solo post
# bctM=c()
# bctM$NAME=c(activePreB[,c(1)],passivePreB[,c(1)])
# bctM = cbind(bctM,rbind(activePostB[,c(4:16)],passivePostB[,c(4:16)]))
# bctM$NAME <- factor(bctM$NAME, levels = bctM$NAME)
# bctMMelted = melt(bctM)
# ggplot!

## post minus pre
bctPP = activePostB[,c(1,c(4:16))] 
d = dim(bctPP)
bctPP[,c(2:d[2])] = bctPP[,c(2:dim(bctPP)[2])] - activePreB[,c(4:16)] 

bctPP = rbind(bctPP, passivePostB[,c(1,c(4:16))])
bctPP[c((d[1]+1):dim(bctPP)[1]),c(2:d[2])] = bctPP[c((d[1]+1):dim(bctPP)[1]),c(2:d[2])]- passivePreB[,c(4:16)] 

bctPP$NAME <- factor(bctPP$NAME, levels = bctPP$NAME) 
bctPPMelted = melt(bctPP)

ggplot(bctPPMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="red", high="green") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
#### solo sofi % pre vs post-pre %
sofiB = bctPrePost[bctPrePost$GROUP=="SOFI",c(1,2,4)]
sofiB$TOTAL_pre = sofiB$TOTAL_pre/13
sofiB$pre_post_diff = sofiB$pre_post_diff/13
sofiB = sofiB[order(sofiB[, 2], sofiB[, 3]),] #order by pre value

sofiB$NAME <- factor(sofiB$NAME, levels = sofiB$NAME) 
sofiBMelted =melt(sofiB)
ggplot(sofiBMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

#### solo ale % pre vs post-pre %
aleB = bctPrePost[bctPrePost$GROUP=="ALE",c(1,2,4)]
aleB$TOTAL_pre = aleB$TOTAL_pre/13
aleB$pre_post_diff = aleB$pre_post_diff/13
aleB = aleB[order(aleB[, 2]), ] #order by pre value
aleB$NAME <- factor(aleB$NAME, levels = aleB$NAME) 
aleBMelted =melt(aleB)
ggplot(aleBMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

#### solo elo % pre vs post-pre %
eloB = bctPrePost[bctPrePost$GROUP=="ELO",c(1,2,4)]
eloB$TOTAL_pre = eloB$TOTAL_pre/13
eloB$pre_post_diff = eloB$pre_post_diff/13
eloB = eloB[order(eloB[, 2]), ] #order by pre value
eloB$NAME <- factor(eloB$NAME, levels = eloB$NAME) 
eloBMelted =melt(eloB)
ggplot(eloBMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
##### Comparar robotito - post PC - diff PC en formato matriz**
## id: activeAllTestsV2
activeAllTestsV2 <- robotitoV2[,c(1,2,7)] # names, group and total robotito score
activeAllTestsV2$TOTAL_R = robotitoV2$TOTAL_R/4
## techcheck
activeAllTestsV2$TOTAL_T_post = activePostT$TOTAL/9
activeAllTestsV2$diff_T = (activePostT$TOTAL - activePreT$TOTAL)/9
#bctt
activeAllTestsV2$TOTAL_B_post = activePostB$TOTAL/13
activeAllTestsV2$diff_B = (activePostB$TOTAL - activePreB$TOTAL)/13

activeAllTestsV2 = activeAllTestsV2[order(activeAllTestsV2[, 2], activeAllTestsV2[, 3]),] #order by group and then by robotito score
activeAllTestsV2$NAME <- factor(activeAllTestsV2$NAME, levels = activeAllTestsV2$NAME) #important to avoid alphabetic order in y axis
### techcheck
activeAllTestsV2TMelted<-melt(activeAllTestsV2[,c(1,3,4,5)])
ggplot(activeAllTestsV2TMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing Robotito and TechCheck tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
### bct
activeAllTestsV2BMelted<-melt(activeAllTestsV2[,c(1,3,6,7)])
ggplot(activeAllTestsV2BMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing Robotito and BCTt tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

### compare diffs
maxT = 9
techcheckBCTPreDiffMaxVal = techcheckPrePost[,c(1,2,4)]
names(techcheckBCTPreDiffMaxVal)[2] = "TOTAL_preT"
techcheckBCTPreDiffMaxVal$DiffMaxValT = preT$pre_post_diff/(maxT - techcheckBCTPreDiffMaxVal$TOTAL_preT)
maxB = 13
techcheckBCTPreDiffMaxVal$DiffMaxValB = bctPrePost$pre_post_diff/(maxB - bctPrePost$TOTAL_pre)
# just active group
techcheckBCTPreDiffMaxVal = techcheckBCTPreDiffMaxVal[techcheckBCTPreDiffMaxVal$GROUP!="ELO",]
# add robotito
techcheckBCTPreDiffMaxVal$TOTAL_R = robotitoV2$TOTAL_R/4

techcheckBCTPreDiffMaxVal = techcheckBCTPreDiffMaxVal[order( techcheckBCTPreDiffMaxVal[, 6]),] #order by diff BCT score and robotito score

techcheckBCTPreDiffMaxVal$NAME <- factor(techcheckBCTPreDiffMaxVal$NAME, levels = techcheckBCTPreDiffMaxVal$NAME) #important to avoid alphabetic order in y axis

techcheckBCTPreDiffMaxValMelted<-melt(techcheckBCTPreDiffMaxVal[,c(1,4,5,6)])

ggplot(techcheckBCTPreDiffMaxValMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

#### correlation BCT y TechCheck - POST
## all
abilities = cbind(bctAbilities[,c("TYPE","SEQUENCES","IF","SIMPLE","NESTED")],techCheckAbilities[, c("ALGORITHMS","REPRESENTATION","CONTROL")])
Hmisc::rcorr(as.matrix(abilities[,c(-1)]),type = "spearman")
cor.test(abilities$CONTROL,abilities$IF,method="kendall")
cor.test(abilities$SEQUENCES,abilities$ALGORITHMS,method="kendall")
## general score POST IF-CONTROL, SEQUENCES-ALGORITHMS
abilities
cor.test(abilities[abilities$TYPE=="POST",]$CONTROL,abilities[abilities$TYPE=="POST",]$IF,method="spearman")
cor.test(abilities[abilities$TYPE=="POST",]$SEQUENCES,abilities[abilities$TYPE=="POST",]$ALGORITHMS,method="spearman")

#### correlation BCT y TechCheck - POST
## ojo, los bctAbilitiesRobotito y techCheckAbilitiesRobotito no pueden estar ordenados de una manera particular!!
bctTechCheckAbilities = cbind(bctAbilitiesRobotito[,c("SEQUENCES","IF","SIMPLE","NESTED")],techCheckAbilitiesRobotito[, c("ALGORITHMS","REPRESENTATION","CONTROL")])
Hmisc::rcorr(as.matrix(bctTechCheckAbilities),type = "spearman")
#### correlation BCT y TechCheck - PRE
## ojo, los bctAbilitiesRobotito y techCheckAbilitiesRobotito no pueden estar ordenados de una manera particular!!
bctTechCheckAbilitiesPre = cbind(bctAbilitiesRobotitoPre[,c("SEQUENCES","IF","SIMPLE","NESTED")],techCheckAbilitiesRobotitoPre[, c("ALGORITHMS","REPRESENTATION","CONTROL")])
Hmisc::rcorr(as.matrix(bctTechCheckAbilitiesPre),type = "spearman")

#### ROBOTITO MATRIX
library(reshape2) #to use melt
library(ggplot2)
testRobotito = robotitoV2[,c(1:6)]
testRobotito = testRobotito[order(testRobotito[, "P2"], testRobotito[, "P4"], testRobotito[, "P3"]),]

testRobotito$NAME <- factor(testRobotito$NAME, levels = testRobotito$NAME) #important to avoid alphabetic order in y axis
testRobotitoMelted<-melt(testRobotito[,c("NAME","P1","P2","P3","P4")])

ggplot(testRobotitoMelted, aes(x = variable, y = NAME)) + 
  geom_raster(aes(fill=value)) + 
  geom_text(aes(label = value)) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Test", y="Name", title="Matrix comparing tests results in %") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

#### scatterplot bct vs techcheck (pre and post)
totals = bct$TOTAL
totals = cbind(totals,techCheck$TOTAL)
colnames(totals)=c("TOTAL_bct","TOTAL_techCheck")
totals = as.data.frame(totals)
totals$GROUP = bct$GROUP
totals$GROUP2 = bct$GROUP2

ggscatter(totals,x = "TOTAL_bct", y = "TOTAL_techCheck",add = "reg.line", cor.coef = TRUE, cor.method = "spearman", xlab = "BCTt score", ylab = "TechCheck-K score", color = "GROUP2")
ggplot(totals,  aes(TOTAL_bct,TOTAL_techCheck, colour=GROUP2, add = "reg.line")) + geom_point(size=3, position=position_jitter(h=0.15,w=0.15)) + labs(x = "BCTt score", y = "TechCheck score")

# correlacion de cada grupo!!
ggscatter(totals,x = "TOTAL_bct", y = "TOTAL_techCheck", size = 0.3,
          combine = TRUE,xlab = "BCTt score", ylab = "TechCheck-K score",
          color = "GROUP2", palette = "jco",
          add = "reg.line", conf.int = TRUE) + stat_cor(aes(color = GROUP2), method = "spearman")

ggscatter(totals,x = "TOTAL_bct", y = "TOTAL_techCheck",add = "reg.line", cor.coef = TRUE, cor.method = "spearman", xlab = "BCTt score", ylab = "TechCheck-K score", color = "GROUP2",point = "FALSE") + geom_jitter()


