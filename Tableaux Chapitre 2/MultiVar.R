library(MVN)
library(psych)
library(ellipse)
library(FactoMineR)
library(rgl)
library(NMF)

#Data #N = normal et T = avec chronologie
Data <- read.csv("A MODIFIER /Data_N.csv", sep=";")
Data <- data.frame(Data[,-1], row.names=Data[,1])
Data.S <- subset(Data, select=1:10)

Data2 <- read.csv("A MODIFIER /Data_T.csv", sep=";")
Data2 <- data.frame(Data2[,-1], row.names=Data2[,1])
Data2.S <- subset(Data2, select=1:17)

##1. TEST
##1.1 Distribution
#Normal distribution?
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
barplot(sapply(Data.S, var), horiz=T, las=1, cex.names=0.8)
barplot(sapply(Data.S, var), horiz=T, las=1, cex.names=0.8, log='x')
par(mar=mar)

roystonTest(Data.S, qqplot = TRUE)

##1.2 Measures of Appropriateness of Factor Analysis

KMO(cor(Data.S, use = "pairwise.complete.obs"))

plotcorr(cor(Data.S, use = "pairwise.complete.obs"))

	#A perfect circle stands for the correlation coefficient of zero
	#Ellipses with a smaller area reflect the relatively large distance of the correlation coefficient from zero
	#The tangent represents the negative/positive sign of the coefficient

##2. DATA ANALYSIS
##2.1 Scree test

	#How many components is required to get both 'elbow' and explains >85% variance
scree(cor(Data.S))
	#scree also shows the eigenvalues of a factor analysis besides PCA.
fa.parallel(Data.S)
	#compare the previously described scree of the components with a bit of a randomized data to identify the optimal number of components to keep:
	#Parallel analysis suggests that the number of factors =  4  and the number of components =  3

##2.2 PCA
res.pca = PCA(Data[,1:15],quali.sup=c(11:15),scale.unit=TRUE, ncp=4,graph=T)
summary(res.pca)

pairs(res.pca$ind$coord[,c(1,2,3)])

plot3d(res.pca$ind$coord[,1], res.pca$ind$coord[,2], res.pca$ind$coord[,3], col="blue",type="p",box = FALSE)
	#plot3d(res.pca$var$coord[,1], res.pca$var$coord[,2], res.pca$var$coord[,3], col="blue",type="p",box = FALSE)
	#plot3d(res.pca$quali.sup$coord[,1], res.pca$quali.sup$coord[,2], res.pca$quali.sup$coord[,3], col="blue",type="p",box = FALSE)

#PCA Description
dimdesc(res.pca,axes=1:2, proba=0.05)
dimdesc(res.pca,axes=3:4, proba=0.05)

plot.PCA(res.pca,autoLab=c("yes"),axes=c(1, 2), choix="ind", cex=0.7)
plot(res.pca, autoLab=c("yes"),choix="var")
plot(res.pca, autoLab=c("yes"),invisible=c("ind"))
	#Factor map with ellipse for A2, A1, A0, B1, B0
concat = cbind.data.frame(Data[,11],res.pca$ind$coord)
ellipse.coord = coord.ellipse(concat,bary=T)
plot.PCA(res.pca,habillage=11,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"))
	#Factor map with ellipse for Urb, EUrb, ER
concat = cbind.data.frame(Data[,12],res.pca$ind$coord)
ellipse.coord = coord.ellipse(concat,bary=T)
plot.PCA(res.pca,habillage=12,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"))
	#Factor map with ellipse mineral waters
concat = cbind.data.frame(Data[,13],res.pca$ind$coord)
ellipse.coord = coord.ellipse(concat,bary=T)
plot.PCA(res.pca,habillage=13,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"))
	#Factor map with ellipse TPQ
concat = cbind.data.frame(Data[,14],res.pca$ind$coord)
ellipse.coord = coord.ellipse(concat,bary=T)
plot.PCA(res.pca,habillage=14,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"))
	#Factor map with ellipse TAQ
concat = cbind.data.frame(Data[,15],res.pca$ind$coord)
ellipse.coord = coord.ellipse(concat,bary=T)
plot.PCA(res.pca,habillage=15,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"))

plot.PCA(res.pca,autoLab=c("yes"),axes=c(2, 3), choix="ind", cex=0.7)
plot(res.pca, autoLab=c("yes"),choix="var",axes=c(2, 3))
plot(res.pca, autoLab=c("yes"),invisible=c("ind"),axes=c(2, 3))

plot.PCA(res.pca,habillage=11,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"),axes=c(2, 3))
plot.PCA(res.pca,habillage=12,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"),axes=c(2, 3))
plot.PCA(res.pca,habillage=13,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"),axes=c(2, 3))
plot.PCA(res.pca,habillage=15,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"),axes=c(2, 3))

plot.PCA(res.pca,autoLab=c("yes"),axes=c(1, 3), choix="ind", cex=0.7)
plot(res.pca, autoLab=c("yes"),choix="var",axes=c(1, 3))
plot(res.pca, autoLab=c("yes"),invisible=c("ind"),axes=c(1, 3))

plot.PCA(res.pca,habillage=11,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"),axes=c(1, 3))
plot.PCA(res.pca,habillage=12,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"),axes=c(1, 3))
plot.PCA(res.pca,habillage=13,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"),axes=c(1, 3))
plot.PCA(res.pca,habillage=15,ellipse=ellipse.coord,cex=0.8,autoLab=c("yes"),axes=c(1, 3))

##2.3 FACTOR ANALYSIS = latent variable detection
	#Evaluation with maximum-likelihood FA#
(f <- fa(Data.S))
fa.diagram(f)

#Multiple Factor Analysis
res.mfa = MFA(Data2, group=c(1,2,3,1,3,7,3), type=c("s","s","s","s","s","s","n"), ncp=5, name.group=c("Str","StrH","QtMat","Mon","VarMat","Tps","Fact"),num.group.sup=c(1,5))
plot(res.mfa,choix="ind",autoLab=c("yes"),cex=0.7)
plot(res.mfa,choix="ind",autoLab=c("yes"),cex=0.7,partial="all")

##2.4 NMF clustering
estim.r <- nmf(Data2.S, 2:6, nrun = 10, seed = 123456)
plot(estim.r)
consensusmap(estim.r,annCol=Data2.S)
res.multi.method <- nmf(Data2.S, 5, list("brunet", "lee", "snmf/r"), seed = 123456,.options = "t")
compare(res.multi.method)

fit<-nmf(Data2.S, 5, "snmf/r", nrun=20)
coefmap(fit)
basismap(fit)

h<-coef(fit)
fa.sort(t(round(h,5)))
w<-basis(fit)
wp<-w/apply(w,1,sum)
fa.sort(round(wp,5))
type<-max.col(w)
table(type)
t(aggregate(Data2.S, by=list(type), FUN=mean))