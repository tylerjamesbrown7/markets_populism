library(tidyverse)
library(haven)
library(tibble)
library(car)
library(dplyr)
library(MASS)	
library(biotools)	
library(DiscriMiner)	
library(klaR)	
library(car)	
library(dplyr)	
library(lubridate)	
library(ggplot2)	
library(ggExtra)
library(corrplot)
library(sjPlot)
library(lattice)
library(moments)
library(rstatix)
library(Dict)
library(data.table)


ess9 <- ESS9e03_1

vars <- c('cntry','lrscale','emplrel','emplno','estsz','netilet','netinum','hinctnta','isco08')
ess <- ess9[vars]
relnames <- c('employee','selfemployed','fambiz')
# tests comparing employed vs. self-employed on political spectrum
empl.ess <- ess[ess$emplrel == 1 | ess$emplrel == 2 | ess$emplrel == 3, ]
empl.ess$lrscale <- as.integer(empl.ess$lrscale)
empl.ess['empName'] <- NA
empl.ess <- data.frame((empl.ess %>% drop_na(lrscale)))
empl.ess <- data.frame((empl.ess %>% drop_na(netinum)))
empl.ess <- data.frame((empl.ess %>% drop_na(isco08)))
empl.ess <- data.frame((empl.ess %>% drop_na(estsz)))
empl.ess <- data.frame((empl.ess %>% drop_na(hinctnta)))

empl.ess <- empl.ess %>% mutate(empName = case_when(
  emplrel == 1 ~ 'employee',
  emplrel == 2 ~ 'selfemployed',
  emplrel == 3 ~ 'fambiz'
  
))

for (i in 1:length(exCon)){
  empl.ess[empl.ess$cntry == names(exCon)[i],'netinum'] <- (empl.ess[empl.ess$cntry == names(exCon)[i],'netinum'])/exCon[i]
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

for (i in unique(empl.ess$cntry)){
  empl.ess[empl.ess$cntry == i,]$netinum <- remove_outliers(empl.ess[empl.ess$cntry == i,]$netinum)
}
hist(empl.ess$netinum, breaks = 300)

empl.ess['occup'] <- NA
empl.ess <- empl.ess %>% mutate(occup = case_when(
  between(isco08,1000,1999) ~ '1 managers',
  between(isco08,2000,2999) ~ '2 professionals',
  between(isco08,3000,3999) ~ '3 technicians and assoc. profs.',
  between(isco08,4000,4999) ~ '4 clerical',
  between(isco08,5000,5999) ~ '5 service & sales',
  between(isco08,6000,6999) ~ '6 skilled agro',
  between(isco08,7000,7999) ~ '7 craft & trade',
  between(isco08,8000,8999) ~ '8 plant & machine op.',
  between(isco08,9000,9999) ~ '9 elementary',
  between(isco08,0,999) ~ '0 armed forces',
))

incomeLRreg <- lm(lrscale ~ netinum, data = empl.ess)
summary(incomeLRreg)

ggplot(empl.ess, aes(x=netinum, y=lrscale)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 

beta <- incomeLRreg$coefficients[2]
beta0 <- incomeLRreg$coefficients[1]
lrMean <- mean(empl.ess$lrscale)

lrTrans <- (empl.ess$lrscale - (beta * empl.ess$netinum + beta0) + lrMean)
empl.ess <- cbind(empl.ess, lrTrans)


# adjusting income based on exchange rates from 2021
# Norway, UK, CH (Switzerland), Iceland (IS), Montenegro, Serbia, Sweden

exCon <- c('BG' = 1.95583, 'HR' = 7.44, 'CZ' =  25.701, 'DK' = 1/0.134, 'HU' = 322, 'IS' = 1/0.0073, 'NO' = 9.63, 'PL' = 4.3, 'RS' = 115, 'SE' = 10.17, 'CH' = 1.14, 'GB' = 0.89)

# also former communist countries
# Bulgaria, Czechia, Estonia*, Croatia, Hungary, Latvia, Lithuania, Montenegro, POland, Serbia, SLovenia, SLovakia 
communist <- c('BG','CZ','EE','HR','HU','LT','LV','ME','PL','RS','SI','SK')

empl.ess['communist10'] <- NA

empl.ess <- empl.ess %>% mutate(communist10 = case_when(
  cntry %in% communist ~ 1,
  !(cntry %in% communist) ~ 0,
))

empl.ess$occup <- as.factor(empl.ess$occup)


# DONE CLEANING THE DATA 





boxplot(lrscale ~ estsz, data = empfilfam, col = 'yellow', ylab = "LR Scale (10 is R)")	
#calculate means using the tapply function - could also use the by function	
means <- tapply(empfilfam[, 1], empfilfam$estsz, mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:5), y = means+0.5, labels = round(means,2))	










# Levene's test with one independent variable
leveneTest(lrTrans ~ empName, data = empl.ess[empl.ess$empName != 'fambiz',])

pairwise.t.test(empl.ess$lrscale, empl.ess$empName, p.adj = "none")
t.test(empl.ess[empl.ess$empName =='fambiz',1], empl.ess[empl.ess$empName == 'selfemployed',1])
mean(empl.ess[empl.ess$empName =='fambiz',1])

# creating a cross table of data 

# t test empl self

selfLR <- empl.ess[empl.ess$empType == 'self','lrscale']
emplLR <- empl.ess[empl.ess$empType == 'empl','lrscale']

res <- t.test(selfLR, emplLR)
res

typeof(empl.ess$lrscale)

# 3-Way Frequency Table
table(empl.ess$empName, empl.ess$lrscale) %>%
  addmargins()

sjPlot::tab_xtab(var.col = empl.essnofam$lrscale, var.row = empl.essnofam$empName, title = "Employment relation vs. political beliefs", show.row.prc = TRUE)

mean(empl.ess$lrscale)

plot(density(empl.ess[empl.ess$empName == 'employee','lrscale']), main="Density estimate of data")
lines(density(empl.ess[empl.ess$empName == 'selfemployed','lrscale']),col="green")
lines(density(empl.ess[empl.ess$empName == 'fambiz','lrscale']),col="green")
plot(density(empl.ess[empl.ess$empName == 'employee','lrscale']), main="Density estimate of dat", add = TRUE)

ggplot(empl.ess[empl.ess$empName == 'employee',], aes(lrscale)) +
  geom_bar(fill = "#5DADE2") +
  ggtitle('Employees')+ # for the main title 
  xlab('left-right scale') +# for the x axis label
  ylab('frequency') # for the y axis label

ggplot(empl.ess[empl.ess$empName == 'selfemployed',], aes(lrscale)) +
  geom_bar(fill = "#58D68D") +
  ggtitle('Self-employed')+ # for the main title 
  xlab('left-right scale') +# for the x axis label
  ylab('frequency') # for the y axis label

ggplot(empl.ess[empl.ess$empName == 'fambiz',], aes(lrscale)) +
  geom_bar(fill = "#AF7AC5") +
  ggtitle('Self-owned family business')+ # for the main title 
  xlab('left-right scale') +# for the x axis label
  ylab('frequency') # for the y axis label




empl.ess.oz <- empl.ess %>% mutate(emplrel = case_when(
  emplrel == 1 ~ '0',
  emplrel == 2 ~ '1'
))

reg <- lm(lrscale ~ emplno, data=empl.ess)
reg
names(reg)
summary(reg)

dim(reg$residuals[,1])

plot(, reg$residuals) 
>abline(0, 0)

mult <- as.numeric(empl.ess.oz$emplrel) * as.numeric(empl.ess.oz$estsz)

ggplot(empl.ess.oz, aes(x=mult, y=lrscale)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 




# running regression on self-employed, firm size, and lrscale

size.v.lr <- lm(lrscale ~ emplno, data = empl.ess[empl.ess$empName == 'selfemployed',])
summary(size.v.lr)

size.v.lre <- lm(lrscale ~ estsz, data = empl.ess[empl.ess$empName == 'selfemployed',])
summary(size.v.lre)


empl.ess$emplno <- as.integer(empl.ess$emplno)

ggplot(empl.ess[empl.ess$empName == 'selfemployed',], aes(x=estsz, y=lrscale)) +
  geom_point()


cor(empl.ess.plot[,c('lrscale','emplno')])




cat(' Employee skew: ', emplskew, '\n', 'Self-employed skew: ', selfskew, '\n', 'Family business skew: ', famskew)




# ^^ this is a hot mess

# here I'm trying to remove income effects from the LR scale between groups to see if employment category is still impactful

# first, making a regression for the relationship between income and LRscale

empl.ess <- data.frame((empl.ess %>% drop_na(hinctnta)))
empl
names(empl.ess)


empl.ess <- cbind(empl.ess, lrTrans)


hist(empl.ess[empl.ess$empName == 'employee',]$lrTrans, breaks = 11)
hist(empl.ess[empl.ess$empName == 'selfemployed',]$lrTrans, breaks = 11)
hist(empl.ess[empl.ess$empName == 'fambiz',]$lrTrans, breaks = 11)










ggplot(empl.ess[empl.ess$empName == 'employee',], aes(lrTrans)) +
  geom_bar(fill = "#5DADE2") +
  ggtitle('Employees')+ # for the main title 
  xlab('left-right scale') +# for the x axis label
  ylab('frequency') # for the y axis label

ggplot(empl.ess[empl.ess$empName == 'selfemployed',], aes(lrscale)) +
  geom_bar(fill = "#58D68D") +
  ggtitle('Self-employed')+ # for the main title 
  xlab('left-right scale') +# for the x axis label
  ylab('frequency') # for the y axis label

ggplot(empl.ess[empl.ess$empName == 'fambiz',], aes(lrscale)) +
  geom_bar(fill = "#AF7AC5") +
  ggtitle('Self-owned family business')+ # for the main title 
  xlab('left-right scale') +# for the x axis label
  ylab('frequency') # for the y axis label  
  
  
boxplot(lrTrans ~ empName, data = empl.ess, col = 'light green', ylab = "LR Scale (transformed) (10 is R)", main = 'Income-adjusted LR scale by employment relation')	
#calculate means using the tapply function - could also use the by function	
means <- tapply(empl.ess[!is.na(empl.ess$lrTrans), 'lrTrans'], empl.ess[!is.na(empl.ess$lrTrans), 'empName'], mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:3), y = means+0.5, labels = round(means,2))	

boxplot(empl.ess[,'lrscale'] ~ empName, data = empl.ess, col = 'light yellow', ylab = "LR Scale (10 is R)", main = 'Non-adjusted LR scale by employment relation')	
#calculate means using the tapply function - could also use the by function	
means <- tapply(empl.ess[, 'lrscale'], empl.ess$empName, mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:3), y = means+0.5, labels = round(means,2))	

boxplot(empl.ess[,'netinum'] ~ empName, data = empl.ess, col = 'light yellow', ylab = "LR Scale (10 is R)", main = 'Non-adjusted LR scale by employment relation')	
#calculate means using the tapply function - could also use the by function	
means <- tapply(empl.ess[!is.na(empl.ess$netinum), 'netinum'], empl.ess[!is.na(empl.ess$netinum), 'empName'], mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:3), y = means+0.5, labels = round(means,2))	




skew <- c()
skewness(empl.ess[empl.ess$empName == i,'lrscale'])

for (i in relnames){
  cat(i, ' ', skewness(empl.ess[empl.ess$empName == i,'lrTrans']), '\n')
}

selfskew <- skewness(empl.ess[empl.ess$empName == 'selfemployed',1])
emplskew <- skewness(empl.ess[empl.ess$empName == 'employee',1])
famskew <- skewness(empl.ess[empl.ess$empName == 'fambiz',2])

sum(is.na(empl.ess$netinum))



names(empl.ess)


plot(ecdf((empl.ess[,"netinum"])**(1/3)), xlab = 'income (€)',main = 'Cumulative Dist. Plot of Net Income')


# looks like income is much more normally distributed when we use a cube root transformation
hist(empl.ess$netinum**(1/3), breaks = 300)
hist(ess$netinum, breaks = 300)

boxplot((empl.ess[,'netinum'])**(1/3) ~ empName, data = empl.ess, col = 'light yellow', ylab = "cube root of income", main = 'Cube rt. of income by employer relation')	
#calculate means using the tapply function - could also use the by function	
means <- tapply((empl.ess[!is.na(empl.ess$netinum), 'netinum'])**(1/3), empl.ess[!is.na(empl.ess$netinum), 'empName'], mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:3), y = means+1, labels = round(means,2))	


# do the communists now


marx <- filter(empl.ess, cntry %in% communist)
nonmarx <- filter(empl.ess, !(cntry %in% communist))


# Communist countries
boxplot(lrTrans ~ empName, data = marx, col = 'pink', ylab = "LR Scale (transformed) (10 is R)", main = 'Income-adjusted LR scale by employment relation \n(Former Communist countries)')	
#calculate means using the tapply function - could also use the by function	
means <- tapply(marx[!is.na(marx$lrTrans), 'lrTrans'], marx[!is.na(marx$lrTrans), 'empName'], mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:3), y = means+0.5, labels = round(means,2))	


# non-Communist countries
boxplot(lrTrans ~ empName, data = nonmarx, col = 'light blue', ylab = "LR Scale (transformed) (10 is R)", main = 'Income-adjusted LR scale by employment relation \n(non-Former Communist countries)')	
#calculate means using the tapply function - could also use the by function	
means <- tapply(nonmarx[!is.na(nonmarx$lrTrans), 'lrTrans'], nonmarx[!is.na(nonmarx$lrTrans), 'empName'], mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:3), y = means+0.5, labels = round(means,2))	


pairwise.t.test(marx$lrTrans, marx$empName, p.adj = "none")
pairwise.t.test(nonmarx$lrTrans, nonmarx$empName, p.adj = "none")

names(empl.ess)





reg <- lm(lrTrans ~ estsz, data = empl.ess.rm)
summary(reg)
length(reg$residuals)
length(empl.ess.rm$estsz)
plot(empl.ess.rm$estsz, resid(reg))


ggplot(empl.ess, aes(x=estsz, y=lrTrans)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 


empl.ess.rm <- empl.ess[!is.na(empl.ess$estsz) & !(is.na(empl.ess$lrTrans)),]

empl.ess$emplrel <- as.factor(empl.ess$emplrel)

interaction.plot(x.factor = empl.ess$emplrel, #x-axis variable
                 trace.factor = empl.ess$empName, #variable for lines
                 response = empl.ess$lrscale, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "lrscale",
                 xlab = "emplrel",
                 col = c("green", "blue",'red'),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Employment relation",
                 main = 'Interaction plots for establishment size and lrscale \n(non-former Communist countries)')
















# here I'm looking at employment category (and transforming the data to fewer bins)


empl.ess <- empl.ess[empl.ess$occup != '0 armed forces',]
#FFC300
#ADD8E6
ggplot(empl.ess[empl.ess$empName == 'employee',], aes(x = reorder(occup, lrTrans, mean), y = lrTrans)) + 
  geom_boxplot(fill = '#ADD8E6') +
  ggtitle('lrTrans and occupation group, employee') +
  stat_summary(fun.y="mean", geom = 'point', color = 'red') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  geom_text(data = aggregate(occup~lrTrans, empl.ess, mean), aes(label = lrTrans, y = lrTrans + 0.08))

boxplot(lrTrans ~ occup, data = empl.ess, col = 'pink', ylab = "LR Scale (transformed) (10 is R)", main = 'Occupation vs. LRt')	
#calculate means using the tapply function - could also use the by function	
means <- tapply(empl.ess[!is.na(empl.ess$lrTrans), 'lrTrans'], empl.ess[!is.na(empl.ess$lrTrans), 'occup'], mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:3), y = means+0.5, labels = round(means,2))	

occupspecial <- paste(empl.ess[empl.ess$empName != 'fambiz',]$occup, empl.ess[empl.ess$empName != 'fambiz',]$empName)
results <- pairwise.t.test(empl.ess[empl.ess$empName != 'fambiz','lrTrans'], occupspecial, p.adj = "none")

View((results$p.value < 0.05))

ggplot(disprt, aes(occup, netinum)) + 
  geom_boxplot(fill = 'green') +
  stat_summary(fun.y="mean", geom = 'point', color = 'red') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



# looking at three groups across variables of industry and socioeconomic status
# comparing subset means

interaction.plot(x.factor = empl.ess$estsz, #x-axis variable
                 trace.factor = empl.ess$empName, #variable for lines
                 response = empl.ess$lrscale, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "lrscale",
                 xlab = "estsz",
                 col = c("green", "blue",'red'),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Employment relation",
                 main = 'Interaction plots for establishment size and lrscale \n(non-former Communist countries)')


# boxplot comparing SOLO self-employed vs. EMPLOYING self-employed

empl.ess['solo'] <- ifelse(empl.ess$emplno > 0, 'employer','solo')

soloESS <- subset(empl.ess,!is.na(empl.ess$solo))

ggplot(soloESS, aes(solo, lrTrans)) + 
  geom_boxplot(fill = 'green') +
  stat_summary(fun.y="mean", geom = 'point', color = 'red') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

boxplot( ~ solo, data = soloDF, col = 'pink', ylab = "LR Scale (transformed) (10 is R)", main = 'Occupation vs. LRt')	
#calculate means using the tapply function - could also use the by function	
means <- tapply(empl.ess[!is.na(empl.ess$emplno),'lrTrans'], solo, mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:2), y = means+0.5, labels = round(means,2))	

t.test(soloESS[soloESS$solo == 'solo', 'lrTrans'], soloESS[soloESS$solo == 'employer', 'lrTrans'])

# plot of the DN of LR scale based on solo vs. employer status for SE people

ggplot(soloESS[soloESS$solo == 'solo',], aes(lrscale)) +
  geom_bar(fill = "#5DADE2") 
 # for the y axis label
# 14;15 on friday

ggplot(soloESS[soloESS$solo == 'employer',], aes(lrscale)) +
  geom_bar(fill = "#5DADE2")

# 14;15 on friday


# solo vs. employer people by industry

cart <- (table(soloESS$occup, soloESS$solo))

cart
props <- c(cart[1:9,2] / cart[1:9,3])
props <- data.frame(props)
props <- cbind(rownames(props), props)
names(props)[1] <- 'occup'
names(props)[2] <- 'p solo self'
rownames(props) <- NULL


ggplot(props, aes(reorder(occup, -p),p)) +
  geom_bar(fill='#0073C2FF', stat = 'identity')+
  ggtitle('Proportion of solo self-employed people by group')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

cart

pairwise.prop.test(cart, p.adjust.method = 'none')
library(aplpack)	
library(fpc)	
library(cluster)	
library(ape)	
library(amap)


# looking for heterogeneity among self-employed people
# running cluster analysis to see if there are ways to identify distinct groups of self-employed people
# adding communist, mean income, and lrScale

names(empl.ess)

ESSoccupsum <- props

selfemp <- empl.ess[empl.ess$empName == 'selfemployed',]
occup.inc <- tapply(selfemp[,'hinctnta'], selfemp[,'occup'], mean)
occup.comm <- tapply(selfemp[,'communist10'], selfemp[,'occup'], mean)
occup.lrscale <- tapply(selfemp[,'lrscale'], selfemp[,'occup'], mean)

ESSoccupsum <- cbind(ESSoccupsum, occup.inc, occup.comm, occup.lrscale)

View(ESSoccupsum)


ESSpcatab <- na.omit(cbind(occup.inc, occup.comm, occup.lrscale))
View(ESSpcatab)
plot()


.dist <- dist(ESSpcatab, method = "euclidean")	
#Perform cluster analysis	
ESSoccupsum.clust <- hclust(.dist)	


#Make dendrogram	
ESSoccupsum.clust$labels <- as.character(rownames(ESSpcatab))	
plot(ESSoccupsum.clust, xlab = "",ylab = "Distance", main = "Clustering for Occupations, all self employed")	
#identify three groups	
rect.hclust(ESSoccupsum.clust, k = 3)	

library(aplpack)	
library(fpc)	
library(cluster)	
library(ape)	
library(amap)

plot(ESSoccupsum.clust,  main = "Plain dendrogram", hang = -1, cex = 1.5, 	
     xlab = "", ylab = "", sub = "", axes = FALSE) 	
plot(as.dendrogram(ESSoccupsum.clust), type = "triangle", main = "Triangle branches", 	
     cex = 1.5, axes = FALSE) 	

plot(as.phylo(ESSoccupsum.clust), type = "unrooted", main = "Unrooted tree") 	
plot(as.phylo(ESSoccupsum.clust), type = "fan", main = "Leaves spread in a circle")	



hclus_eval(ESSpcatab, dist_m = 'euclidean', clus_m = 'complete', plot_op = T, print_num = 15)	

cuts <- cutree(ESSoccupsum.clust, k = 3)	
for (i in 1:3){	
  print(paste("Occupations in Cluster ",i))	
  print(rownames(ESSpcatab)[cuts == i])	
  print (" ")	
}	

rownames(ESSpcatab)[cuts == 3]

# seems like 3 clusters are appropriate given the variables we have (but we could tweak variables as well)
# cluster 1: 1 managers, 2 professional, 3 technical and assoc profs
# cluster 2: 4 clerical, 7 craft and trade, 8 plant and machine op.
# cluster 3: 5 service and sales, 9 elementary, 6 skilled agriculture



selfemp['occupcluster'] <- NA

selfemp <- selfemp %>% mutate(occupcluster = case_when(
  occup %in% rownames(ESSpcatab)[cuts == 1] ~ 1,
  occup %in% rownames(ESSpcatab)[cuts == 2] ~ 2,
  occup %in% rownames(ESSpcatab)[cuts == 3] ~ 3,
))


boxplot(lrscale ~ occupcluster, data = selfemp, col = 'pink', ylab = "LR scale", main = 'Occupation cluster vs. LR scale \n (all self-employed)')	
#calculate means using the tapply function - could also use the by function	
means <- tapply(selfemp[,'lrscale'], selfemp$occupcluster, mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:3), y = means+0.5, labels = round(means,2))	



ggplot(selfemp, aes(occupcluster,hinctnta)) +
  geom_bar(fill='#0073C2FF', stat = 'identity')+
  ggtitle('Proportion of solo self-employed people by group')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


pairwise.t.test(selfemp$lrscale, selfemp$occupcluster, p.adjust.method = 'none')









#Cluster analysis for EMPLOYED
employed <- empl.ess[empl.ess$empName == 'employee',]

occup.incE <- tapply(employed[,'hinctnta'], employed[,'occup'], mean)
occup.commE <- tapply(employed[,'communist10'], employed[,'occup'], mean)
occup.lrscaleE <- tapply(employed[,'lrscale'], employed[,'occup'], mean)

ESSpcatabE <- na.omit(cbind(occup.incE, occup.commE, occup.lrscaleE))
ESSpcatabE <- ESSpcatabE[-1,]
View(ESSpcatabE)


.distE <- dist(ESSpcatabE, method = "euclidean")	
#Perform cluster analysis	
ESSoccupsum.clustE <- hclust(.distE)	

#Make dendrogram	
ESSoccupsum.clustE$labels <- as.character(rownames(ESSpcatabE))	
plot(ESSoccupsum.clustE, xlab = "",ylab = "Distance", main = "Clustering for Occupations, all employees")	
#identify three groups	
rect.hclust(ESSoccupsum.clustE, k = 4)	

plot(ESSoccupsum.clustE,  main = "Plain dendrogram", hang = -1, cex = 1.5, 	
     xlab = "", ylab = "", sub = "", axes = FALSE) 	
plot(as.dendrogram(ESSoccupsum.clustE), type = "triangle", main = "Triangle branches", 	
     cex = 1.5, axes = FALSE) 	

plot(as.phylo(ESSoccupsum.clustE), type = "unrooted", main = "Unrooted tree") 	
plot(as.phylo(ESSoccupsum.clust)E, type = "fan", main = "Leaves spread in a circle")	



hclus_eval(ESSpcatabE, dist_m = 'euclidean', clus_m = 'complete', plot_op = T, print_num = 15)	

cuts <- cutree(ESSoccupsum.clust, k = 3)	
for (i in 1:3){	
  print(paste("Occupations in Cluster ",i))	
  print(rownames(ESSpcatab)[cuts == i])	
  print (" ")	
}	

rownames(ESSpcatab)[cuts == 3]

# seems like 3 clusters are appropriate given the variables we have (but we could tweak variables as well)
# cluster 1: 1 managers, 2 professional, 3 technical and assoc profs
# cluster 2: 4 clerical, 7 craft and trade, 8 plant and machine op.
# cluster 3: 5 service and sales, 9 elementary, 6 skilled agriculture



selfemp['occupcluster'] <- NA

selfemp <- selfemp %>% mutate(occupcluster = case_when(
  occup %in% rownames(ESSpcatab)[cuts == 1] ~ 1,
  occup %in% rownames(ESSpcatab)[cuts == 2] ~ 2,
  occup %in% rownames(ESSpcatab)[cuts == 3] ~ 3,
))


boxplot(lrscale ~ occupcluster, data = selfemp, col = 'pink', ylab = "LR scale", main = 'Occupation cluster vs. LR scale \n (all self-employed)')	
#calculate means using the tapply function - could also use the by function	
means <- tapply(selfemp[,'lrscale'], selfemp$occupcluster, mean)
means
points(means, col = "red", pch = 19, cex = 1.2)	
text(x = c(1:3), y = means+0.5, labels = round(means,2))	


empl.ess['occupnum'] <- NA
empl.ess <- empl.ess %>% mutate(occupnum = case_when(
  between(isco08,1000,1999) ~ 1,
  between(isco08,2000,2999) ~ 2,
  between(isco08,3000,3999) ~ 3,
  between(isco08,4000,4999) ~ 4,
  between(isco08,5000,5999) ~ 5,
  between(isco08,6000,6999) ~ 6,
  between(isco08,7000,7999) ~ 7,
  between(isco08,8000,8999) ~ 8,
  between(isco08,9000,9999) ~ 9,
  between(isco08,0,999) ~ 0,
))

reg2 <- lm(lrscale ~ estsz + hinctnta + communist10 + occupnum, data = empl.ess)
summary(reg2)

plot(as.numeric(empl.ess$emplno), empl.ess$lrscale)

ggplot(argh, aes(x=emplno, y=lrscale)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 


argh <- empl.ess



argh$emplno <- remove_outliers(argh$emplno)
summary(lm(lrTrans ~ emplno, data = argh[argh$empName == 'selfemployed',]))

plot(emplno, )




crisis <- ESS9e03_1

unique(crisis$gincdif)




