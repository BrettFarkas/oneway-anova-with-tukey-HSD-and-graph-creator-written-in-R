#ENTER YOUR DATA BELOW:
group1<-c(2,3,2,3,3) #the data from each group goes here
group2<-c(5,4,3,3,3)
group3<-c(4,4,4,3,4)
group4<-c(4,5,4,5,4)
group5<-c(5,6,6,5,5)
                     #ALSO ENTER DATA HERE:
groups<-c(0,5,10,15,20) #your five variations go here, 
                    #the x axis, like 1 gram of fertlizer, 2, etc.
                    #ALSO CHANGE THE GRAPH TEXT LABELS BELOW
                    #AFTER THAT, YOU'RE DONE.  RUN THE CODE 
                    
GraphTitle <- "The effect of fertilizer on plant growth"                    
xAxisLabel <- "Fertilizer (g)"                   
yAxisLabel <- "Plant height (cm)"
################################################################################
# DON'T WORRY ABOUT THE CODE BELOW THIS
meangroup1<-mean(group1)
meangroup2<-mean(group2)
meangroup3<-mean(group3)
meangroup4<-mean(group4)
meangroup5<-mean(group5)

#largevalue1 = max*P(group1)
#largevalue2 = max(group2)
#largevalue3 = max(group3)
#largevalue4 = max(group4)
#largevalue5 = max(group5)
#largevalues <- c(largevalue1,largevalue2,largevalue3,largevalue4,largevalue5)
largevalues <- c(meangroup1,meangroup2,meangroup3,meangroup4,meangroup5)

standard1 <- sd(group1) 
standard2 <- sd(group2) 
standard3 <- sd(group3) 
standard4 <- sd(group4) 
standard5 <- sd(group5) 
largeststandard = max(standard1, standard2, standard3, standard4, standard5)
largevalues<- largevalues + largeststandard + 1

minvalue <- min(meangroup1,meangroup2,meangroup3,meangroup4,meangroup5)
minvalue <- minvalue - 1

#avariable <-(graphpoints,groups)

largestYvalue = max(largevalues)
graphpoints<-c(meangroup1,meangroup2,meangroup3,meangroup4,meangroup5)
plot(groups,graphpoints, xlab='', ylab='',pch=15,ylim=c(minvalue,largestYvalue))
lines(groups, graphpoints)

#axis(1,at=seq(10),labels = seq(1,10,2))



abline(lm(graphpoints~groups), col="blue") #if you don't want a trend line 
                                          #then erase this line
#########################################################################################
########################## LABEL THE AXES HERE ##########################################
#########################################################################################
title(main=GraphTitle, col.main="black", font.main=4)
title(xlab=xAxisLabel, col.lab=rgb(0,0,0))
title(ylab=yAxisLabel, col.lab=rgb(0,0,0))
#########################################################################################
#########################################################################################
#########################################################################################
sdgroups<-c(sd(group1),
            sd(group2),
            sd(group3),
            sd(group4),
            sd(group5))

arrows(groups,graphpoints-sdgroups,groups,graphpoints+sdgroups,length=0.1,angle=90,code=3) 

sem <- function(x) sd(x)/sqrt(length(x))

print("SCROLL DOWN FOR GRAPH, MEAN, AND S.D.")
####################################################
Data <- data.frame(
  Y=c(group1, group2, group3, group4, group5),
  Site =factor(rep(c("group1", "group2", "group3", "group4", "group5"), times=c(length(group1), length(group2), length(group3), length(group4), length(group5))))
)
fm1 <- aov(Y~Site, data=Data)
anova(fm1)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#NOTE FROM FARKAS:

print("THE Pr(>F) Value above is the important one.  If it is below .05 then you had groups that were significantly different. If it is greater than .05 then none of the groups were significantly different from each other")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

TukeyHSD(fm1)

#NOTE FROM FARKAS:
print("THE p adj VALUES ABOVE TELL YOU IF IT IS SIGNIFICANT, IF THE VALUE IS BELOW .05, THEN THERE IS A SIGNFICANT DIFFERENCE BETWEEN THOSE TWO GROUPS. YOU WANT YOUR DATA TO BE SIGNIFICANTLY DIFFERENT FROM YOUR CONTROL GROUP")

print ("Standard error of the mean Group 1:") 
sem(group1)
print ("Standard error of the mean Group 2:") 
sem(group2) #Standard Error of the Mean Group 2:
print ("Standard error of the mean Group 3:") 
sem(group3) #Standard Error of the Mean Group 3:
print ("Standard error of the mean Group 4:") 
sem(group4) #Standard Error of the Mean Group 4:
print ("Standard error of the mean Group 5:") 
sem(group5) #Standard Error of the Mean Group 5:

print ("Mean Group 1:") 
mean(group1)
print ("Mean Group 2:") 
mean(group2)
print ("Mean Group 3:") 
mean(group3)
print ("Mean Group 4:") 
mean(group4)
print ("Mean Group 5:") 
mean(group5)

print ("Standard Deviation Group 1:") 
sd(group1) #Standard Deviation Group 1:
print ("Standard Deviation Group 2:") 
sd(group2) #Standard Deviation Group 2:
print ("Standard Deviation Group 3:") 
sd(group3) #Standard Deviation Group 3:
print ("Standard Deviation Group 4:") 
sd(group4) #Standard Deviation Group 4:
print ("Standard Deviation Group 5:") 
sd(group5) #Standard Deviation Group 5:
#########################################################
