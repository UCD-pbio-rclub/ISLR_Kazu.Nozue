---
title: "chapter 7: March_21_2018"
author: "Kazu"
date: "3/21/2018"
output: 
  html_document: 
    keep_md: yes
---


# Chapter 7 Lab: Non-linear Modeling


```r
# 7.8 Lab: Non-linear Modeling
library(ISLR)
```

```
## Warning: package 'ISLR' was built under R version 3.4.2
```

```r
attach(Wage)
#7.8.1 Polynomial Regression and Step Functions
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))
```

```
##                 Estimate Std. Error    t value     Pr(>|t|)
## (Intercept)    111.70361  0.7287409 153.283015 0.000000e+00
## poly(age, 4)1  447.06785 39.9147851  11.200558 1.484604e-28
## poly(age, 4)2 -478.31581 39.9147851 -11.983424 2.355831e-32
## poly(age, 4)3  125.52169 39.9147851   3.144742 1.678622e-03
## poly(age, 4)4  -77.91118 39.9147851  -1.951938 5.103865e-02
```

```r
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
```

```
##                             Estimate   Std. Error   t value     Pr(>|t|)
## (Intercept)            -1.841542e+02 6.004038e+01 -3.067172 0.0021802539
## poly(age, 4, raw = T)1  2.124552e+01 5.886748e+00  3.609042 0.0003123618
## poly(age, 4, raw = T)2 -5.638593e-01 2.061083e-01 -2.735743 0.0062606446
## poly(age, 4, raw = T)3  6.810688e-03 3.065931e-03  2.221409 0.0263977518
## poly(age, 4, raw = T)4 -3.203830e-05 1.641359e-05 -1.951938 0.0510386498
```

```r
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)
```

```
##   (Intercept)           age      I(age^2)      I(age^3)      I(age^4) 
## -1.841542e+02  2.124552e+01 -5.638593e-01  6.810688e-03 -3.203830e-05
```

```r
# This does the same more compactly, using the cbind() function for building a matrix from a collection of vectors; any function call such as cbind() inside a formula also serves as a wrapper.
fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
# We now create a grid of values for age at which we want predictions, and then call the generic predict() function, specifying that we want standard errors as well.
agelims=range(age) # 
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
#par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))
```

```
## [1] 7.81597e-11
```

```r
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
```

```
## Analysis of Variance Table
## 
## Model 1: wage ~ age
## Model 2: wage ~ poly(age, 2)
## Model 3: wage ~ poly(age, 3)
## Model 4: wage ~ poly(age, 4)
## Model 5: wage ~ poly(age, 5)
##   Res.Df     RSS Df Sum of Sq        F    Pr(>F)    
## 1   2998 5022216                                    
## 2   2997 4793430  1    228786 143.5931 < 2.2e-16 ***
## 3   2996 4777674  1     15756   9.8888  0.001679 ** 
## 4   2995 4771604  1      6070   3.8098  0.051046 .  
## 5   2994 4770322  1      1283   0.8050  0.369682    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
coef(summary(fit.5))
```

```
##                 Estimate Std. Error     t value     Pr(>|t|)
## (Intercept)    111.70361  0.7287647 153.2780243 0.000000e+00
## poly(age, 5)1  447.06785 39.9160847  11.2001930 1.491111e-28
## poly(age, 5)2 -478.31581 39.9160847 -11.9830341 2.367734e-32
## poly(age, 5)3  125.52169 39.9160847   3.1446392 1.679213e-03
## poly(age, 5)4  -77.91118 39.9160847  -1.9518743 5.104623e-02
## poly(age, 5)5  -35.81289 39.9160847  -0.8972045 3.696820e-01
```

```r
(-11.983)^2
```

```
## [1] 143.5923
```

```r
fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)
```

```
## Analysis of Variance Table
## 
## Model 1: wage ~ education + age
## Model 2: wage ~ education + poly(age, 2)
## Model 3: wage ~ education + poly(age, 3)
##   Res.Df     RSS Df Sum of Sq        F Pr(>F)    
## 1   2994 3867992                                 
## 2   2993 3725395  1    142597 114.6969 <2e-16 ***
## 3   2992 3719809  1      5587   4.4936 0.0341 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Next we consider the task of predicting whether an individual earns more than $250,000 per year. We proceed much as before, except that first we create the appropriate response vector, and then apply the glm() function using family="binomial" in order to fit a polynomial logistic regression model.
fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial) 
# Note that we again use the wrapper I() to create this binary response variable on the fly. The expression wage>250 evaluates to a logical variable containing TRUEs and FALSEs, which glm() coerces to binary by setting the TRUEs to 1 and the FALSEs to 0.
preds=predict(fit,newdata=list(age=age.grid),se=T)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))
# Note that we could have directly computed the probabilities by selecting the type="response" option in the predict() function.
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)
# However, the corresponding confidence intervals would not have been sensible because we would end up with negative probabilities! Finally, the right-hand plot from Figure 7.1 was made as follows:
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
# We have drawn the age values corresponding to the observations with wage values above 250 as gray marks on the top of the plot, and those with wage values below 250 are shown as gray marks on the bottom of the plot. We used the jitter() function to jitter the age values a bit so that observations with the same age value do not cover each other up. This is often called a rug plot.In order to fit a step function, as discussed in Section 7.2, we use the cut() function.
table(cut(age,4))
```

```
## 
## (17.9,33.5]   (33.5,49]   (49,64.5] (64.5,80.1] 
##         750        1399         779          72
```

```r
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))
```

```
##                         Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)            94.158392   1.476069 63.789970 0.000000e+00
## cut(age, 4)(33.5,49]   24.053491   1.829431 13.148074 1.982315e-38
## cut(age, 4)(49,64.5]   23.664559   2.067958 11.443444 1.040750e-29
## cut(age, 4)(64.5,80.1]  7.640592   4.987424  1.531972 1.256350e-01
```

```r
# Here cut() automatically picked the cutpoints at 33.5, 49, and 64.5 years of age. We could also have specified our own cutpoints directly using the breaks option. The function cut() returns an ordered categorical variable; the lm() function then creates a set of dummy variables for use in the re- gression. The age<33.5 category is left out, so the intercept coefficient of $94,160 can be interpreted as the average salary for those under 33.5 years of age, and the other coefficients can be interpreted as the average addi- tional salary for those in the other age groups. We can produce predictions and plots just as we did in the case of the polynomial fit.
```
#7.8.2 Splines

```r
library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")
dim(bs(age,knots=c(25,40,60)))
```

```
## [1] 3000    6
```

```r
dim(bs(age,df=6))
```

```
## [1] 3000    6
```

```r
attr(bs(age,df=6),"knots")
```

```
##   25%   50%   75% 
## 33.75 42.00 51.00
```

```r
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
```

```
## Warning in smooth.spline(age, wage, cv = TRUE): cross-validation with non-
## unique 'x' values seems doubtful
```

```r
fit2$df
```

```
## [1] 6.794596
```

```r
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
detach()
```
# Practice
#6. In this exercise, you will further analyze the Wage data set considered throughout this chapter.

```r
## (a) Perform polynomial regression to predict wage using age. Use cross-validation to select the optimal degree d for the polynomial. What degree was chosen, and how does this compare to the results of hypothesis testing using ANOVA? Make a plot of the resulting polynomial fit to the data.
attach(Wage)
```

```
## The following objects are masked from Wage (pos = 3):
## 
##     age, education, health, health_ins, jobclass, logwage, maritl,
##     race, region, wage, year
```

```r
summary(Wage)
```

```
##       year           age                     maritl           race     
##  Min.   :2003   Min.   :18.00   1. Never Married: 648   1. White:2480  
##  1st Qu.:2004   1st Qu.:33.75   2. Married      :2074   2. Black: 293  
##  Median :2006   Median :42.00   3. Widowed      :  19   3. Asian: 190  
##  Mean   :2006   Mean   :42.41   4. Divorced     : 204   4. Other:  37  
##  3rd Qu.:2008   3rd Qu.:51.00   5. Separated    :  55                  
##  Max.   :2009   Max.   :80.00                                          
##                                                                        
##               education                     region    
##  1. < HS Grad      :268   2. Middle Atlantic   :3000  
##  2. HS Grad        :971   1. New England       :   0  
##  3. Some College   :650   3. East North Central:   0  
##  4. College Grad   :685   4. West North Central:   0  
##  5. Advanced Degree:426   5. South Atlantic    :   0  
##                           6. East South Central:   0  
##                           (Other)              :   0  
##            jobclass               health      health_ins      logwage     
##  1. Industrial :1544   1. <=Good     : 858   1. Yes:2083   Min.   :3.000  
##  2. Information:1456   2. >=Very Good:2142   2. No : 917   1st Qu.:4.447  
##                                                            Median :4.653  
##                                                            Mean   :4.654  
##                                                            3rd Qu.:4.857  
##                                                            Max.   :5.763  
##                                                                           
##       wage       
##  Min.   : 20.09  
##  1st Qu.: 85.38  
##  Median :104.92  
##  Mean   :111.70  
##  3rd Qu.:128.68  
##  Max.   :318.34  
## 
```

```r
summary(is.na(Wage)) # no NAs.
```

```
##     year            age            maritl           race        
##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
##  FALSE:3000      FALSE:3000      FALSE:3000      FALSE:3000     
##  NA's :0         NA's :0         NA's :0         NA's :0        
##  education         region         jobclass         health       
##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
##  FALSE:3000      FALSE:3000      FALSE:3000      FALSE:3000     
##  NA's :0         NA's :0         NA's :0         NA's :0        
##  health_ins       logwage           wage        
##  Mode :logical   Mode :logical   Mode :logical  
##  FALSE:3000      FALSE:3000      FALSE:3000     
##  NA's :0         NA's :0         NA's :0
```

```r
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Wage),rep=TRUE)
test=(-train)
# x=model.matrix(wage~.,Wage) # Does not remove the first column, (Intercept)
y=Wage$wage
y.test=y[test]
# 
fit.d1=lm(wage~poly(age,1),data=Wage[train,])
fit.d2=lm(wage~poly(age,2),data=Wage[train,])
fit.d3=lm(wage~poly(age,3),data=Wage[train,])
fit.d4=lm(wage~poly(age,4),data=Wage[train,])
fit.d5=lm(wage~poly(age,5),data=Wage[train,])
fit.d6=lm(wage~poly(age,6),data=Wage[train,])
fit.d7=lm(wage~poly(age,7),data=Wage[train,])

# prediction
preds.d1=predict(fit.d1,newdata=Wage[test,])
preds.d2=predict(fit.d2,newdata=Wage[test,])
preds.d3=predict(fit.d3,newdata=Wage[test,])
preds.d4=predict(fit.d4,newdata=Wage[test,])
preds.d5=predict(fit.d5,newdata=Wage[test,])
preds.d6=predict(fit.d6,newdata=Wage[test,])
preds.d7=predict(fit.d7,newdata=Wage[test,])

# MSE
mean((preds.d1-Wage[test,"wage"])^2)
```

```
## [1] 1675.55
```

```r
mean((preds.d2-Wage[test,"wage"])^2)
```

```
## [1] 1599.82
```

```r
mean((preds.d3-Wage[test,"wage"])^2)
```

```
## [1] 1595.708
```

```r
mean((preds.d4-Wage[test,"wage"])^2)
```

```
## [1] 1594.633
```

```r
mean((preds.d5-Wage[test,"wage"])^2)
```

```
## [1] 1596.692
```

```r
mean((preds.d6-Wage[test,"wage"])^2)
```

```
## [1] 1595.026
```

```r
mean((preds.d7-Wage[test,"wage"])^2)
```

```
## [1] 1594.098
```

```r
# ANOVA
anova(fit.d1,fit.d2,fit.d3,fit.d4,fit.d5,fit.d6,fit.d7)
```

```
## Analysis of Variance Table
## 
## Model 1: wage ~ poly(age, 1)
## Model 2: wage ~ poly(age, 2)
## Model 3: wage ~ poly(age, 3)
## Model 4: wage ~ poly(age, 4)
## Model 5: wage ~ poly(age, 5)
## Model 6: wage ~ poly(age, 6)
## Model 7: wage ~ poly(age, 7)
##   Res.Df     RSS Df Sum of Sq       F    Pr(>F)    
## 1   1556 2552783                                   
## 2   1555 2460607  1     92176 58.9446 2.854e-14 ***
## 3   1554 2442000  1     18608 11.8992 0.0005766 ***
## 4   1553 2434466  1      7534  4.8178 0.0283144 *  
## 5   1552 2428944  1      5522  3.5311 0.0604171 .  
## 6   1551 2426204  1      2739  1.7517 0.1858567    
## 7   1550 2423852  1      2353  1.5045 0.2201593    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# how about to use boot()? cv.glm()
library(boot)
set.seed(2)
# K=5, i is degree testing 1 to 10
all.deltas = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(wage~poly(age, i), data=Wage)
  all.deltas[i] = cv.glm(Wage, glm.fit, K=30)$delta[2]
}
# explanation of cv.glm()$delta from help
# delta: A vector of length two. The first component is the raw cross-validation estimate of prediction error. The second component is the adjusted cross-validation estimate. The adjustment is designed to compensate for the bias introduced by not using leave-one-out cross-validation.
all.deltas
```

```
##  [1] 1676.634 1600.298 1595.764 1594.444 1595.319 1593.935 1594.294
##  [8] 1594.731 1593.326 1594.931
```

```r
library(ggplot2)
qplot(1:10,all.deltas,geom="line",xlab="degree")
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## see Julin's tibble way

## (b) Fit a step function to predict wage using age, and perform cross- validation to choose the optimal number of cuts. Make a plot of the fit obtained.
fit.lm.cut2=lm(wage~cut(age,2),data=Wage[train,])
fit.lm.cut3=lm(wage~cut(age,3),data=Wage[train,])
fit.lm.cut4=lm(wage~cut(age,4),data=Wage[train,])
fit.lm.cut5=lm(wage~cut(age,5),data=Wage[train,])
# prediction
preds.lm.cut2=predict(fit.lm.cut2,newdata=Wage[test,])
preds.lm.cut3=predict(fit.lm.cut3,newdata=Wage[test,])
preds.lm.cut4=predict(fit.lm.cut4,newdata=Wage[test,])
preds.lm.cut5=predict(fit.lm.cut5,newdata=Wage[test,])
# MSE
mean((preds.lm.cut2-Wage[test,"wage"])^2)
```

```
## [1] 1732.256
```

```r
mean((preds.lm.cut3-Wage[test,"wage"])^2)
```

```
## [1] 1681.784
```

```r
mean((preds.lm.cut4-Wage[test,"wage"])^2)
```

```
## [1] 1635.811
```

```r
mean((preds.lm.cut5-Wage[test,"wage"])^2)
```

```
## [1] 1634.171
```

```r
# anova

# using boot(), cut 2 to 10
all.deltas = rep(NA, 9)
for (i in 2:10) {
  glm.fit = glm(wage~cut(age,i),data=Wage)
  all.deltas[i] = cv.glm(Wage, glm.fit, K=30)$delta[2]
} # does not work
```

```
## Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels): factor cut(age, i) has new levels (20.9,48], (48,75.1]
```

```r
for (i in 2:10) {
  Wage$cut2<-cut(age,i)
  glm.fit = glm(wage~cut2,data=Wage)
  all.deltas[i] = cv.glm(Wage, glm.fit, K=30)$delta[2]
} 
all.deltas
```

```
##  [1]       NA 1733.977 1682.508 1635.346 1631.821 1623.130 1611.142
##  [8] 1602.528 1609.915 1605.108
```

```r
library(ggplot2)
qplot(1:10,all.deltas,geom="line",xlab="degree")
```

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
# plot the fit
Wage$cut3<-cut(Wage$age,8)
glm.fit <- glm(wage~cut3,data=Wage)
Wage$predict.wage<-predict(glm.fit,cut3)
```

```
## Error in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type == : object 'cut3' not found
```

```r
ggplot(Wage,aes(x=age,y=wage)) + geom_point(alpha=0.1) + geom_step(aes(x=age,y=predict.wage,color=I("red")))
```

```
## Error in eval(expr, envir, enclos): object 'predict.wage' not found
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
detach()
```


```r
# 7. The Wage data set contains a number of other features not explored in this chapter, such as marital status (maritl), job class (jobclass), and others. Explore the relationships between some of these other predictors and wage, and use non-linear fitting techniques in order to fit flexible models to the data. Create plots of the results obtained, and write a summary of your findings.
attach(Wage)
```

```
## The following objects are masked from Wage (pos = 4):
## 
##     age, education, health, health_ins, jobclass, logwage, maritl,
##     race, region, wage, year
```

```
## The following objects are masked from Wage (pos = 5):
## 
##     age, education, health, health_ins, jobclass, logwage, maritl,
##     race, region, wage, year
```

```r
?pairs
pairs(Wage)
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
names(Wage)
```

```
##  [1] "year"       "age"        "maritl"     "race"       "education" 
##  [6] "region"     "jobclass"   "health"     "health_ins" "logwage"   
## [11] "wage"       "cut2"       "cut3"
```

```r
library(ggplot2)
ggplot(Wage,aes(as.factor(year),wage))+geom_violin() # no effects
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
ggplot(Wage,aes(maritl,wage))+geom_violin() # has effects
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-4-3.png)<!-- -->

```r
ggplot(Wage,aes(race,wage))+geom_violin() # has effects
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-4-4.png)<!-- -->

```r
ggplot(Wage,aes(education,wage))+geom_violin() # has strong effects
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-4-5.png)<!-- -->

```r
ggplot(Wage,aes(region,wage))+geom_violin() # only Middle Atlantic
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-4-6.png)<!-- -->

```r
ggplot(Wage,aes(jobclass,wage))+geom_violin() # has effects
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-4-7.png)<!-- -->

```r
ggplot(Wage,aes(health,wage))+geom_violin() # has effects
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-4-8.png)<!-- -->

```r
ggplot(Wage,aes(health_ins,wage))+geom_violin() # has strong effects
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-4-9.png)<!-- -->

```r
ggplot(Wage,aes(logwage,wage))+geom_point() # just transformation
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-4-10.png)<!-- -->

```r
# non-linear regression using poly() and estimate coefficient by ridge or lasso
# which degree I need to use?

detach()
```
# 8. Fit some of the non-linear models investigated in this chapter to the Auto data set. Is there evidence for non-linear relationships in this data set? Create some informative plots to justify your answer.


```r
Auto
```

```
##      mpg cylinders displacement horsepower weight acceleration year origin
## 1   18.0         8        307.0        130   3504         12.0   70      1
## 2   15.0         8        350.0        165   3693         11.5   70      1
## 3   18.0         8        318.0        150   3436         11.0   70      1
## 4   16.0         8        304.0        150   3433         12.0   70      1
## 5   17.0         8        302.0        140   3449         10.5   70      1
## 6   15.0         8        429.0        198   4341         10.0   70      1
## 7   14.0         8        454.0        220   4354          9.0   70      1
## 8   14.0         8        440.0        215   4312          8.5   70      1
## 9   14.0         8        455.0        225   4425         10.0   70      1
## 10  15.0         8        390.0        190   3850          8.5   70      1
## 11  15.0         8        383.0        170   3563         10.0   70      1
## 12  14.0         8        340.0        160   3609          8.0   70      1
## 13  15.0         8        400.0        150   3761          9.5   70      1
## 14  14.0         8        455.0        225   3086         10.0   70      1
## 15  24.0         4        113.0         95   2372         15.0   70      3
## 16  22.0         6        198.0         95   2833         15.5   70      1
## 17  18.0         6        199.0         97   2774         15.5   70      1
## 18  21.0         6        200.0         85   2587         16.0   70      1
## 19  27.0         4         97.0         88   2130         14.5   70      3
## 20  26.0         4         97.0         46   1835         20.5   70      2
## 21  25.0         4        110.0         87   2672         17.5   70      2
## 22  24.0         4        107.0         90   2430         14.5   70      2
## 23  25.0         4        104.0         95   2375         17.5   70      2
## 24  26.0         4        121.0        113   2234         12.5   70      2
## 25  21.0         6        199.0         90   2648         15.0   70      1
## 26  10.0         8        360.0        215   4615         14.0   70      1
## 27  10.0         8        307.0        200   4376         15.0   70      1
## 28  11.0         8        318.0        210   4382         13.5   70      1
## 29   9.0         8        304.0        193   4732         18.5   70      1
## 30  27.0         4         97.0         88   2130         14.5   71      3
## 31  28.0         4        140.0         90   2264         15.5   71      1
## 32  25.0         4        113.0         95   2228         14.0   71      3
## 34  19.0         6        232.0        100   2634         13.0   71      1
## 35  16.0         6        225.0        105   3439         15.5   71      1
## 36  17.0         6        250.0        100   3329         15.5   71      1
## 37  19.0         6        250.0         88   3302         15.5   71      1
## 38  18.0         6        232.0        100   3288         15.5   71      1
## 39  14.0         8        350.0        165   4209         12.0   71      1
## 40  14.0         8        400.0        175   4464         11.5   71      1
## 41  14.0         8        351.0        153   4154         13.5   71      1
## 42  14.0         8        318.0        150   4096         13.0   71      1
## 43  12.0         8        383.0        180   4955         11.5   71      1
## 44  13.0         8        400.0        170   4746         12.0   71      1
## 45  13.0         8        400.0        175   5140         12.0   71      1
## 46  18.0         6        258.0        110   2962         13.5   71      1
## 47  22.0         4        140.0         72   2408         19.0   71      1
## 48  19.0         6        250.0        100   3282         15.0   71      1
## 49  18.0         6        250.0         88   3139         14.5   71      1
## 50  23.0         4        122.0         86   2220         14.0   71      1
## 51  28.0         4        116.0         90   2123         14.0   71      2
## 52  30.0         4         79.0         70   2074         19.5   71      2
## 53  30.0         4         88.0         76   2065         14.5   71      2
## 54  31.0         4         71.0         65   1773         19.0   71      3
## 55  35.0         4         72.0         69   1613         18.0   71      3
## 56  27.0         4         97.0         60   1834         19.0   71      2
## 57  26.0         4         91.0         70   1955         20.5   71      1
## 58  24.0         4        113.0         95   2278         15.5   72      3
## 59  25.0         4         97.5         80   2126         17.0   72      1
## 60  23.0         4         97.0         54   2254         23.5   72      2
## 61  20.0         4        140.0         90   2408         19.5   72      1
## 62  21.0         4        122.0         86   2226         16.5   72      1
## 63  13.0         8        350.0        165   4274         12.0   72      1
## 64  14.0         8        400.0        175   4385         12.0   72      1
## 65  15.0         8        318.0        150   4135         13.5   72      1
## 66  14.0         8        351.0        153   4129         13.0   72      1
## 67  17.0         8        304.0        150   3672         11.5   72      1
## 68  11.0         8        429.0        208   4633         11.0   72      1
## 69  13.0         8        350.0        155   4502         13.5   72      1
## 70  12.0         8        350.0        160   4456         13.5   72      1
## 71  13.0         8        400.0        190   4422         12.5   72      1
## 72  19.0         3         70.0         97   2330         13.5   72      3
## 73  15.0         8        304.0        150   3892         12.5   72      1
## 74  13.0         8        307.0        130   4098         14.0   72      1
## 75  13.0         8        302.0        140   4294         16.0   72      1
## 76  14.0         8        318.0        150   4077         14.0   72      1
## 77  18.0         4        121.0        112   2933         14.5   72      2
## 78  22.0         4        121.0         76   2511         18.0   72      2
## 79  21.0         4        120.0         87   2979         19.5   72      2
## 80  26.0         4         96.0         69   2189         18.0   72      2
## 81  22.0         4        122.0         86   2395         16.0   72      1
## 82  28.0         4         97.0         92   2288         17.0   72      3
## 83  23.0         4        120.0         97   2506         14.5   72      3
## 84  28.0         4         98.0         80   2164         15.0   72      1
## 85  27.0         4         97.0         88   2100         16.5   72      3
## 86  13.0         8        350.0        175   4100         13.0   73      1
## 87  14.0         8        304.0        150   3672         11.5   73      1
## 88  13.0         8        350.0        145   3988         13.0   73      1
## 89  14.0         8        302.0        137   4042         14.5   73      1
## 90  15.0         8        318.0        150   3777         12.5   73      1
## 91  12.0         8        429.0        198   4952         11.5   73      1
## 92  13.0         8        400.0        150   4464         12.0   73      1
## 93  13.0         8        351.0        158   4363         13.0   73      1
## 94  14.0         8        318.0        150   4237         14.5   73      1
## 95  13.0         8        440.0        215   4735         11.0   73      1
## 96  12.0         8        455.0        225   4951         11.0   73      1
## 97  13.0         8        360.0        175   3821         11.0   73      1
## 98  18.0         6        225.0        105   3121         16.5   73      1
## 99  16.0         6        250.0        100   3278         18.0   73      1
## 100 18.0         6        232.0        100   2945         16.0   73      1
## 101 18.0         6        250.0         88   3021         16.5   73      1
## 102 23.0         6        198.0         95   2904         16.0   73      1
## 103 26.0         4         97.0         46   1950         21.0   73      2
## 104 11.0         8        400.0        150   4997         14.0   73      1
## 105 12.0         8        400.0        167   4906         12.5   73      1
## 106 13.0         8        360.0        170   4654         13.0   73      1
## 107 12.0         8        350.0        180   4499         12.5   73      1
## 108 18.0         6        232.0        100   2789         15.0   73      1
## 109 20.0         4         97.0         88   2279         19.0   73      3
## 110 21.0         4        140.0         72   2401         19.5   73      1
## 111 22.0         4        108.0         94   2379         16.5   73      3
## 112 18.0         3         70.0         90   2124         13.5   73      3
## 113 19.0         4        122.0         85   2310         18.5   73      1
## 114 21.0         6        155.0        107   2472         14.0   73      1
## 115 26.0         4         98.0         90   2265         15.5   73      2
## 116 15.0         8        350.0        145   4082         13.0   73      1
## 117 16.0         8        400.0        230   4278          9.5   73      1
## 118 29.0         4         68.0         49   1867         19.5   73      2
## 119 24.0         4        116.0         75   2158         15.5   73      2
## 120 20.0         4        114.0         91   2582         14.0   73      2
## 121 19.0         4        121.0        112   2868         15.5   73      2
## 122 15.0         8        318.0        150   3399         11.0   73      1
## 123 24.0         4        121.0        110   2660         14.0   73      2
## 124 20.0         6        156.0        122   2807         13.5   73      3
## 125 11.0         8        350.0        180   3664         11.0   73      1
## 126 20.0         6        198.0         95   3102         16.5   74      1
## 128 19.0         6        232.0        100   2901         16.0   74      1
## 129 15.0         6        250.0        100   3336         17.0   74      1
## 130 31.0         4         79.0         67   1950         19.0   74      3
## 131 26.0         4        122.0         80   2451         16.5   74      1
## 132 32.0         4         71.0         65   1836         21.0   74      3
## 133 25.0         4        140.0         75   2542         17.0   74      1
## 134 16.0         6        250.0        100   3781         17.0   74      1
## 135 16.0         6        258.0        110   3632         18.0   74      1
## 136 18.0         6        225.0        105   3613         16.5   74      1
## 137 16.0         8        302.0        140   4141         14.0   74      1
## 138 13.0         8        350.0        150   4699         14.5   74      1
## 139 14.0         8        318.0        150   4457         13.5   74      1
## 140 14.0         8        302.0        140   4638         16.0   74      1
## 141 14.0         8        304.0        150   4257         15.5   74      1
## 142 29.0         4         98.0         83   2219         16.5   74      2
## 143 26.0         4         79.0         67   1963         15.5   74      2
## 144 26.0         4         97.0         78   2300         14.5   74      2
## 145 31.0         4         76.0         52   1649         16.5   74      3
## 146 32.0         4         83.0         61   2003         19.0   74      3
## 147 28.0         4         90.0         75   2125         14.5   74      1
## 148 24.0         4         90.0         75   2108         15.5   74      2
## 149 26.0         4        116.0         75   2246         14.0   74      2
## 150 24.0         4        120.0         97   2489         15.0   74      3
## 151 26.0         4        108.0         93   2391         15.5   74      3
## 152 31.0         4         79.0         67   2000         16.0   74      2
## 153 19.0         6        225.0         95   3264         16.0   75      1
## 154 18.0         6        250.0        105   3459         16.0   75      1
## 155 15.0         6        250.0         72   3432         21.0   75      1
## 156 15.0         6        250.0         72   3158         19.5   75      1
## 157 16.0         8        400.0        170   4668         11.5   75      1
## 158 15.0         8        350.0        145   4440         14.0   75      1
## 159 16.0         8        318.0        150   4498         14.5   75      1
## 160 14.0         8        351.0        148   4657         13.5   75      1
## 161 17.0         6        231.0        110   3907         21.0   75      1
## 162 16.0         6        250.0        105   3897         18.5   75      1
## 163 15.0         6        258.0        110   3730         19.0   75      1
## 164 18.0         6        225.0         95   3785         19.0   75      1
## 165 21.0         6        231.0        110   3039         15.0   75      1
## 166 20.0         8        262.0        110   3221         13.5   75      1
## 167 13.0         8        302.0        129   3169         12.0   75      1
## 168 29.0         4         97.0         75   2171         16.0   75      3
## 169 23.0         4        140.0         83   2639         17.0   75      1
## 170 20.0         6        232.0        100   2914         16.0   75      1
## 171 23.0         4        140.0         78   2592         18.5   75      1
## 172 24.0         4        134.0         96   2702         13.5   75      3
## 173 25.0         4         90.0         71   2223         16.5   75      2
## 174 24.0         4        119.0         97   2545         17.0   75      3
## 175 18.0         6        171.0         97   2984         14.5   75      1
## 176 29.0         4         90.0         70   1937         14.0   75      2
## 177 19.0         6        232.0         90   3211         17.0   75      1
## 178 23.0         4        115.0         95   2694         15.0   75      2
## 179 23.0         4        120.0         88   2957         17.0   75      2
## 180 22.0         4        121.0         98   2945         14.5   75      2
## 181 25.0         4        121.0        115   2671         13.5   75      2
## 182 33.0         4         91.0         53   1795         17.5   75      3
## 183 28.0         4        107.0         86   2464         15.5   76      2
## 184 25.0         4        116.0         81   2220         16.9   76      2
## 185 25.0         4        140.0         92   2572         14.9   76      1
## 186 26.0         4         98.0         79   2255         17.7   76      1
## 187 27.0         4        101.0         83   2202         15.3   76      2
## 188 17.5         8        305.0        140   4215         13.0   76      1
## 189 16.0         8        318.0        150   4190         13.0   76      1
## 190 15.5         8        304.0        120   3962         13.9   76      1
## 191 14.5         8        351.0        152   4215         12.8   76      1
## 192 22.0         6        225.0        100   3233         15.4   76      1
## 193 22.0         6        250.0        105   3353         14.5   76      1
## 194 24.0         6        200.0         81   3012         17.6   76      1
## 195 22.5         6        232.0         90   3085         17.6   76      1
## 196 29.0         4         85.0         52   2035         22.2   76      1
## 197 24.5         4         98.0         60   2164         22.1   76      1
## 198 29.0         4         90.0         70   1937         14.2   76      2
## 199 33.0         4         91.0         53   1795         17.4   76      3
## 200 20.0         6        225.0        100   3651         17.7   76      1
## 201 18.0         6        250.0         78   3574         21.0   76      1
## 202 18.5         6        250.0        110   3645         16.2   76      1
## 203 17.5         6        258.0         95   3193         17.8   76      1
## 204 29.5         4         97.0         71   1825         12.2   76      2
## 205 32.0         4         85.0         70   1990         17.0   76      3
## 206 28.0         4         97.0         75   2155         16.4   76      3
## 207 26.5         4        140.0         72   2565         13.6   76      1
## 208 20.0         4        130.0        102   3150         15.7   76      2
## 209 13.0         8        318.0        150   3940         13.2   76      1
## 210 19.0         4        120.0         88   3270         21.9   76      2
## 211 19.0         6        156.0        108   2930         15.5   76      3
## 212 16.5         6        168.0        120   3820         16.7   76      2
## 213 16.5         8        350.0        180   4380         12.1   76      1
## 214 13.0         8        350.0        145   4055         12.0   76      1
## 215 13.0         8        302.0        130   3870         15.0   76      1
## 216 13.0         8        318.0        150   3755         14.0   76      1
## 217 31.5         4         98.0         68   2045         18.5   77      3
## 218 30.0         4        111.0         80   2155         14.8   77      1
## 219 36.0         4         79.0         58   1825         18.6   77      2
## 220 25.5         4        122.0         96   2300         15.5   77      1
## 221 33.5         4         85.0         70   1945         16.8   77      3
## 222 17.5         8        305.0        145   3880         12.5   77      1
## 223 17.0         8        260.0        110   4060         19.0   77      1
## 224 15.5         8        318.0        145   4140         13.7   77      1
## 225 15.0         8        302.0        130   4295         14.9   77      1
## 226 17.5         6        250.0        110   3520         16.4   77      1
## 227 20.5         6        231.0        105   3425         16.9   77      1
## 228 19.0         6        225.0        100   3630         17.7   77      1
## 229 18.5         6        250.0         98   3525         19.0   77      1
## 230 16.0         8        400.0        180   4220         11.1   77      1
## 231 15.5         8        350.0        170   4165         11.4   77      1
## 232 15.5         8        400.0        190   4325         12.2   77      1
## 233 16.0         8        351.0        149   4335         14.5   77      1
## 234 29.0         4         97.0         78   1940         14.5   77      2
## 235 24.5         4        151.0         88   2740         16.0   77      1
## 236 26.0         4         97.0         75   2265         18.2   77      3
## 237 25.5         4        140.0         89   2755         15.8   77      1
## 238 30.5         4         98.0         63   2051         17.0   77      1
## 239 33.5         4         98.0         83   2075         15.9   77      1
## 240 30.0         4         97.0         67   1985         16.4   77      3
## 241 30.5         4         97.0         78   2190         14.1   77      2
## 242 22.0         6        146.0         97   2815         14.5   77      3
## 243 21.5         4        121.0        110   2600         12.8   77      2
## 244 21.5         3         80.0        110   2720         13.5   77      3
## 245 43.1         4         90.0         48   1985         21.5   78      2
## 246 36.1         4         98.0         66   1800         14.4   78      1
## 247 32.8         4         78.0         52   1985         19.4   78      3
## 248 39.4         4         85.0         70   2070         18.6   78      3
## 249 36.1         4         91.0         60   1800         16.4   78      3
## 250 19.9         8        260.0        110   3365         15.5   78      1
## 251 19.4         8        318.0        140   3735         13.2   78      1
## 252 20.2         8        302.0        139   3570         12.8   78      1
## 253 19.2         6        231.0        105   3535         19.2   78      1
## 254 20.5         6        200.0         95   3155         18.2   78      1
## 255 20.2         6        200.0         85   2965         15.8   78      1
## 256 25.1         4        140.0         88   2720         15.4   78      1
## 257 20.5         6        225.0        100   3430         17.2   78      1
## 258 19.4         6        232.0         90   3210         17.2   78      1
## 259 20.6         6        231.0        105   3380         15.8   78      1
## 260 20.8         6        200.0         85   3070         16.7   78      1
## 261 18.6         6        225.0        110   3620         18.7   78      1
## 262 18.1         6        258.0        120   3410         15.1   78      1
## 263 19.2         8        305.0        145   3425         13.2   78      1
## 264 17.7         6        231.0        165   3445         13.4   78      1
## 265 18.1         8        302.0        139   3205         11.2   78      1
## 266 17.5         8        318.0        140   4080         13.7   78      1
## 267 30.0         4         98.0         68   2155         16.5   78      1
## 268 27.5         4        134.0         95   2560         14.2   78      3
## 269 27.2         4        119.0         97   2300         14.7   78      3
## 270 30.9         4        105.0         75   2230         14.5   78      1
## 271 21.1         4        134.0         95   2515         14.8   78      3
## 272 23.2         4        156.0        105   2745         16.7   78      1
## 273 23.8         4        151.0         85   2855         17.6   78      1
## 274 23.9         4        119.0         97   2405         14.9   78      3
## 275 20.3         5        131.0        103   2830         15.9   78      2
## 276 17.0         6        163.0        125   3140         13.6   78      2
## 277 21.6         4        121.0        115   2795         15.7   78      2
## 278 16.2         6        163.0        133   3410         15.8   78      2
## 279 31.5         4         89.0         71   1990         14.9   78      2
## 280 29.5         4         98.0         68   2135         16.6   78      3
## 281 21.5         6        231.0        115   3245         15.4   79      1
## 282 19.8         6        200.0         85   2990         18.2   79      1
## 283 22.3         4        140.0         88   2890         17.3   79      1
## 284 20.2         6        232.0         90   3265         18.2   79      1
## 285 20.6         6        225.0        110   3360         16.6   79      1
## 286 17.0         8        305.0        130   3840         15.4   79      1
## 287 17.6         8        302.0        129   3725         13.4   79      1
## 288 16.5         8        351.0        138   3955         13.2   79      1
## 289 18.2         8        318.0        135   3830         15.2   79      1
## 290 16.9         8        350.0        155   4360         14.9   79      1
## 291 15.5         8        351.0        142   4054         14.3   79      1
## 292 19.2         8        267.0        125   3605         15.0   79      1
## 293 18.5         8        360.0        150   3940         13.0   79      1
## 294 31.9         4         89.0         71   1925         14.0   79      2
## 295 34.1         4         86.0         65   1975         15.2   79      3
## 296 35.7         4         98.0         80   1915         14.4   79      1
## 297 27.4         4        121.0         80   2670         15.0   79      1
## 298 25.4         5        183.0         77   3530         20.1   79      2
## 299 23.0         8        350.0        125   3900         17.4   79      1
## 300 27.2         4        141.0         71   3190         24.8   79      2
## 301 23.9         8        260.0         90   3420         22.2   79      1
## 302 34.2         4        105.0         70   2200         13.2   79      1
## 303 34.5         4        105.0         70   2150         14.9   79      1
## 304 31.8         4         85.0         65   2020         19.2   79      3
## 305 37.3         4         91.0         69   2130         14.7   79      2
## 306 28.4         4        151.0         90   2670         16.0   79      1
## 307 28.8         6        173.0        115   2595         11.3   79      1
## 308 26.8         6        173.0        115   2700         12.9   79      1
## 309 33.5         4        151.0         90   2556         13.2   79      1
## 310 41.5         4         98.0         76   2144         14.7   80      2
## 311 38.1         4         89.0         60   1968         18.8   80      3
## 312 32.1         4         98.0         70   2120         15.5   80      1
## 313 37.2         4         86.0         65   2019         16.4   80      3
## 314 28.0         4        151.0         90   2678         16.5   80      1
## 315 26.4         4        140.0         88   2870         18.1   80      1
## 316 24.3         4        151.0         90   3003         20.1   80      1
## 317 19.1         6        225.0         90   3381         18.7   80      1
## 318 34.3         4         97.0         78   2188         15.8   80      2
## 319 29.8         4        134.0         90   2711         15.5   80      3
## 320 31.3         4        120.0         75   2542         17.5   80      3
## 321 37.0         4        119.0         92   2434         15.0   80      3
## 322 32.2         4        108.0         75   2265         15.2   80      3
## 323 46.6         4         86.0         65   2110         17.9   80      3
## 324 27.9         4        156.0        105   2800         14.4   80      1
## 325 40.8         4         85.0         65   2110         19.2   80      3
## 326 44.3         4         90.0         48   2085         21.7   80      2
## 327 43.4         4         90.0         48   2335         23.7   80      2
## 328 36.4         5        121.0         67   2950         19.9   80      2
## 329 30.0         4        146.0         67   3250         21.8   80      2
## 330 44.6         4         91.0         67   1850         13.8   80      3
## 332 33.8         4         97.0         67   2145         18.0   80      3
## 333 29.8         4         89.0         62   1845         15.3   80      2
## 334 32.7         6        168.0        132   2910         11.4   80      3
## 335 23.7         3         70.0        100   2420         12.5   80      3
## 336 35.0         4        122.0         88   2500         15.1   80      2
## 338 32.4         4        107.0         72   2290         17.0   80      3
## 339 27.2         4        135.0         84   2490         15.7   81      1
## 340 26.6         4        151.0         84   2635         16.4   81      1
## 341 25.8         4        156.0         92   2620         14.4   81      1
## 342 23.5         6        173.0        110   2725         12.6   81      1
## 343 30.0         4        135.0         84   2385         12.9   81      1
## 344 39.1         4         79.0         58   1755         16.9   81      3
## 345 39.0         4         86.0         64   1875         16.4   81      1
## 346 35.1         4         81.0         60   1760         16.1   81      3
## 347 32.3         4         97.0         67   2065         17.8   81      3
## 348 37.0         4         85.0         65   1975         19.4   81      3
## 349 37.7         4         89.0         62   2050         17.3   81      3
## 350 34.1         4         91.0         68   1985         16.0   81      3
## 351 34.7         4        105.0         63   2215         14.9   81      1
## 352 34.4         4         98.0         65   2045         16.2   81      1
## 353 29.9         4         98.0         65   2380         20.7   81      1
## 354 33.0         4        105.0         74   2190         14.2   81      2
## 356 33.7         4        107.0         75   2210         14.4   81      3
## 357 32.4         4        108.0         75   2350         16.8   81      3
## 358 32.9         4        119.0        100   2615         14.8   81      3
## 359 31.6         4        120.0         74   2635         18.3   81      3
## 360 28.1         4        141.0         80   3230         20.4   81      2
## 361 30.7         6        145.0         76   3160         19.6   81      2
## 362 25.4         6        168.0        116   2900         12.6   81      3
## 363 24.2         6        146.0        120   2930         13.8   81      3
## 364 22.4         6        231.0        110   3415         15.8   81      1
## 365 26.6         8        350.0        105   3725         19.0   81      1
## 366 20.2         6        200.0         88   3060         17.1   81      1
## 367 17.6         6        225.0         85   3465         16.6   81      1
## 368 28.0         4        112.0         88   2605         19.6   82      1
## 369 27.0         4        112.0         88   2640         18.6   82      1
## 370 34.0         4        112.0         88   2395         18.0   82      1
## 371 31.0         4        112.0         85   2575         16.2   82      1
## 372 29.0         4        135.0         84   2525         16.0   82      1
## 373 27.0         4        151.0         90   2735         18.0   82      1
## 374 24.0         4        140.0         92   2865         16.4   82      1
## 375 36.0         4        105.0         74   1980         15.3   82      2
## 376 37.0         4         91.0         68   2025         18.2   82      3
## 377 31.0         4         91.0         68   1970         17.6   82      3
## 378 38.0         4        105.0         63   2125         14.7   82      1
## 379 36.0         4         98.0         70   2125         17.3   82      1
## 380 36.0         4        120.0         88   2160         14.5   82      3
## 381 36.0         4        107.0         75   2205         14.5   82      3
## 382 34.0         4        108.0         70   2245         16.9   82      3
## 383 38.0         4         91.0         67   1965         15.0   82      3
## 384 32.0         4         91.0         67   1965         15.7   82      3
## 385 38.0         4         91.0         67   1995         16.2   82      3
## 386 25.0         6        181.0        110   2945         16.4   82      1
## 387 38.0         6        262.0         85   3015         17.0   82      1
## 388 26.0         4        156.0         92   2585         14.5   82      1
## 389 22.0         6        232.0        112   2835         14.7   82      1
## 390 32.0         4        144.0         96   2665         13.9   82      3
## 391 36.0         4        135.0         84   2370         13.0   82      1
## 392 27.0         4        151.0         90   2950         17.3   82      1
## 393 27.0         4        140.0         86   2790         15.6   82      1
## 394 44.0         4         97.0         52   2130         24.6   82      2
## 395 32.0         4        135.0         84   2295         11.6   82      1
## 396 28.0         4        120.0         79   2625         18.6   82      1
## 397 31.0         4        119.0         82   2720         19.4   82      1
##                                     name
## 1              chevrolet chevelle malibu
## 2                      buick skylark 320
## 3                     plymouth satellite
## 4                          amc rebel sst
## 5                            ford torino
## 6                       ford galaxie 500
## 7                       chevrolet impala
## 8                      plymouth fury iii
## 9                       pontiac catalina
## 10                    amc ambassador dpl
## 11                   dodge challenger se
## 12                    plymouth 'cuda 340
## 13                 chevrolet monte carlo
## 14               buick estate wagon (sw)
## 15                 toyota corona mark ii
## 16                       plymouth duster
## 17                            amc hornet
## 18                         ford maverick
## 19                          datsun pl510
## 20          volkswagen 1131 deluxe sedan
## 21                           peugeot 504
## 22                           audi 100 ls
## 23                              saab 99e
## 24                              bmw 2002
## 25                           amc gremlin
## 26                             ford f250
## 27                             chevy c20
## 28                            dodge d200
## 29                              hi 1200d
## 30                          datsun pl510
## 31                   chevrolet vega 2300
## 32                         toyota corona
## 34                           amc gremlin
## 35             plymouth satellite custom
## 36             chevrolet chevelle malibu
## 37                       ford torino 500
## 38                           amc matador
## 39                      chevrolet impala
## 40             pontiac catalina brougham
## 41                      ford galaxie 500
## 42                     plymouth fury iii
## 43                     dodge monaco (sw)
## 44              ford country squire (sw)
## 45                   pontiac safari (sw)
## 46            amc hornet sportabout (sw)
## 47                   chevrolet vega (sw)
## 48                      pontiac firebird
## 49                          ford mustang
## 50                    mercury capri 2000
## 51                             opel 1900
## 52                           peugeot 304
## 53                             fiat 124b
## 54                   toyota corolla 1200
## 55                           datsun 1200
## 56                  volkswagen model 111
## 57                      plymouth cricket
## 58                 toyota corona hardtop
## 59                    dodge colt hardtop
## 60                     volkswagen type 3
## 61                        chevrolet vega
## 62                   ford pinto runabout
## 63                      chevrolet impala
## 64                      pontiac catalina
## 65                     plymouth fury iii
## 66                      ford galaxie 500
## 67                    amc ambassador sst
## 68                       mercury marquis
## 69                  buick lesabre custom
## 70            oldsmobile delta 88 royale
## 71                chrysler newport royal
## 72                       mazda rx2 coupe
## 73                      amc matador (sw)
## 74      chevrolet chevelle concours (sw)
## 75                 ford gran torino (sw)
## 76        plymouth satellite custom (sw)
## 77                       volvo 145e (sw)
## 78                   volkswagen 411 (sw)
## 79                      peugeot 504 (sw)
## 80                       renault 12 (sw)
## 81                       ford pinto (sw)
## 82                       datsun 510 (sw)
## 83           toyouta corona mark ii (sw)
## 84                       dodge colt (sw)
## 85              toyota corolla 1600 (sw)
## 86                     buick century 350
## 87                           amc matador
## 88                      chevrolet malibu
## 89                      ford gran torino
## 90                  dodge coronet custom
## 91              mercury marquis brougham
## 92             chevrolet caprice classic
## 93                              ford ltd
## 94              plymouth fury gran sedan
## 95          chrysler new yorker brougham
## 96              buick electra 225 custom
## 97               amc ambassador brougham
## 98                      plymouth valiant
## 99                 chevrolet nova custom
## 100                           amc hornet
## 101                        ford maverick
## 102                      plymouth duster
## 103              volkswagen super beetle
## 104                     chevrolet impala
## 105                         ford country
## 106               plymouth custom suburb
## 107             oldsmobile vista cruiser
## 108                          amc gremlin
## 109                        toyota carina
## 110                       chevrolet vega
## 111                           datsun 610
## 112                            maxda rx3
## 113                           ford pinto
## 114                     mercury capri v6
## 115                 fiat 124 sport coupe
## 116              chevrolet monte carlo s
## 117                   pontiac grand prix
## 118                             fiat 128
## 119                           opel manta
## 120                           audi 100ls
## 121                          volvo 144ea
## 122                    dodge dart custom
## 123                            saab 99le
## 124                       toyota mark ii
## 125                     oldsmobile omega
## 126                      plymouth duster
## 128                           amc hornet
## 129                       chevrolet nova
## 130                          datsun b210
## 131                           ford pinto
## 132                  toyota corolla 1200
## 133                       chevrolet vega
## 134    chevrolet chevelle malibu classic
## 135                          amc matador
## 136           plymouth satellite sebring
## 137                     ford gran torino
## 138             buick century luxus (sw)
## 139            dodge coronet custom (sw)
## 140                ford gran torino (sw)
## 141                     amc matador (sw)
## 142                             audi fox
## 143                    volkswagen dasher
## 144                           opel manta
## 145                        toyota corona
## 146                           datsun 710
## 147                           dodge colt
## 148                             fiat 128
## 149                          fiat 124 tc
## 150                          honda civic
## 151                               subaru
## 152                            fiat x1.9
## 153              plymouth valiant custom
## 154                       chevrolet nova
## 155                      mercury monarch
## 156                        ford maverick
## 157                     pontiac catalina
## 158                    chevrolet bel air
## 159                  plymouth grand fury
## 160                             ford ltd
## 161                        buick century
## 162            chevroelt chevelle malibu
## 163                          amc matador
## 164                        plymouth fury
## 165                        buick skyhawk
## 166                  chevrolet monza 2+2
## 167                      ford mustang ii
## 168                       toyota corolla
## 169                           ford pinto
## 170                          amc gremlin
## 171                        pontiac astro
## 172                        toyota corona
## 173                    volkswagen dasher
## 174                           datsun 710
## 175                           ford pinto
## 176                    volkswagen rabbit
## 177                            amc pacer
## 178                           audi 100ls
## 179                          peugeot 504
## 180                          volvo 244dl
## 181                            saab 99le
## 182                     honda civic cvcc
## 183                             fiat 131
## 184                            opel 1900
## 185                             capri ii
## 186                           dodge colt
## 187                         renault 12tl
## 188    chevrolet chevelle malibu classic
## 189               dodge coronet brougham
## 190                          amc matador
## 191                     ford gran torino
## 192                     plymouth valiant
## 193                       chevrolet nova
## 194                        ford maverick
## 195                           amc hornet
## 196                   chevrolet chevette
## 197                      chevrolet woody
## 198                            vw rabbit
## 199                          honda civic
## 200                       dodge aspen se
## 201                    ford granada ghia
## 202                   pontiac ventura sj
## 203                        amc pacer d/l
## 204                    volkswagen rabbit
## 205                         datsun b-210
## 206                       toyota corolla
## 207                           ford pinto
## 208                            volvo 245
## 209           plymouth volare premier v8
## 210                          peugeot 504
## 211                       toyota mark ii
## 212                   mercedes-benz 280s
## 213                     cadillac seville
## 214                            chevy c10
## 215                            ford f108
## 216                           dodge d100
## 217                    honda accord cvcc
## 218              buick opel isuzu deluxe
## 219                        renault 5 gtl
## 220                    plymouth arrow gs
## 221                datsun f-10 hatchback
## 222            chevrolet caprice classic
## 223           oldsmobile cutlass supreme
## 224                dodge monaco brougham
## 225              mercury cougar brougham
## 226                   chevrolet concours
## 227                        buick skylark
## 228               plymouth volare custom
## 229                         ford granada
## 230                pontiac grand prix lj
## 231         chevrolet monte carlo landau
## 232                     chrysler cordoba
## 233                     ford thunderbird
## 234             volkswagen rabbit custom
## 235                pontiac sunbird coupe
## 236              toyota corolla liftback
## 237                  ford mustang ii 2+2
## 238                   chevrolet chevette
## 239                       dodge colt m/m
## 240                            subaru dl
## 241                    volkswagen dasher
## 242                           datsun 810
## 243                             bmw 320i
## 244                           mazda rx-4
## 245      volkswagen rabbit custom diesel
## 246                          ford fiesta
## 247                     mazda glc deluxe
## 248                       datsun b210 gx
## 249                     honda civic cvcc
## 250    oldsmobile cutlass salon brougham
## 251                       dodge diplomat
## 252                 mercury monarch ghia
## 253                   pontiac phoenix lj
## 254                     chevrolet malibu
## 255                 ford fairmont (auto)
## 256                  ford fairmont (man)
## 257                      plymouth volare
## 258                          amc concord
## 259                buick century special
## 260                       mercury zephyr
## 261                          dodge aspen
## 262                      amc concord d/l
## 263         chevrolet monte carlo landau
## 264      buick regal sport coupe (turbo)
## 265                          ford futura
## 266                      dodge magnum xe
## 267                   chevrolet chevette
## 268                        toyota corona
## 269                           datsun 510
## 270                           dodge omni
## 271            toyota celica gt liftback
## 272                     plymouth sapporo
## 273               oldsmobile starfire sx
## 274                        datsun 200-sx
## 275                            audi 5000
## 276                          volvo 264gl
## 277                           saab 99gle
## 278                        peugeot 604sl
## 279                  volkswagen scirocco
## 280                      honda accord lx
## 281                    pontiac lemans v6
## 282                     mercury zephyr 6
## 283                      ford fairmont 4
## 284                     amc concord dl 6
## 285                        dodge aspen 6
## 286            chevrolet caprice classic
## 287                      ford ltd landau
## 288                mercury grand marquis
## 289                      dodge st. regis
## 290              buick estate wagon (sw)
## 291             ford country squire (sw)
## 292        chevrolet malibu classic (sw)
## 293 chrysler lebaron town @ country (sw)
## 294                     vw rabbit custom
## 295                     maxda glc deluxe
## 296          dodge colt hatchback custom
## 297                        amc spirit dl
## 298                   mercedes benz 300d
## 299                    cadillac eldorado
## 300                          peugeot 504
## 301    oldsmobile cutlass salon brougham
## 302                     plymouth horizon
## 303                 plymouth horizon tc3
## 304                           datsun 210
## 305                   fiat strada custom
## 306                buick skylark limited
## 307                   chevrolet citation
## 308            oldsmobile omega brougham
## 309                      pontiac phoenix
## 310                            vw rabbit
## 311                toyota corolla tercel
## 312                   chevrolet chevette
## 313                           datsun 310
## 314                   chevrolet citation
## 315                        ford fairmont
## 316                          amc concord
## 317                          dodge aspen
## 318                            audi 4000
## 319               toyota corona liftback
## 320                            mazda 626
## 321                 datsun 510 hatchback
## 322                       toyota corolla
## 323                            mazda glc
## 324                           dodge colt
## 325                           datsun 210
## 326                 vw rabbit c (diesel)
## 327                   vw dasher (diesel)
## 328                  audi 5000s (diesel)
## 329                   mercedes-benz 240d
## 330                  honda civic 1500 gl
## 332                            subaru dl
## 333                     vokswagen rabbit
## 334                        datsun 280-zx
## 335                        mazda rx-7 gs
## 336                    triumph tr7 coupe
## 338                         honda accord
## 339                     plymouth reliant
## 340                        buick skylark
## 341               dodge aries wagon (sw)
## 342                   chevrolet citation
## 343                     plymouth reliant
## 344                       toyota starlet
## 345                       plymouth champ
## 346                     honda civic 1300
## 347                               subaru
## 348                       datsun 210 mpg
## 349                        toyota tercel
## 350                          mazda glc 4
## 351                   plymouth horizon 4
## 352                       ford escort 4w
## 353                       ford escort 2h
## 354                     volkswagen jetta
## 356                        honda prelude
## 357                       toyota corolla
## 358                         datsun 200sx
## 359                            mazda 626
## 360            peugeot 505s turbo diesel
## 361                         volvo diesel
## 362                      toyota cressida
## 363                    datsun 810 maxima
## 364                        buick century
## 365                oldsmobile cutlass ls
## 366                      ford granada gl
## 367               chrysler lebaron salon
## 368                   chevrolet cavalier
## 369             chevrolet cavalier wagon
## 370            chevrolet cavalier 2-door
## 371           pontiac j2000 se hatchback
## 372                       dodge aries se
## 373                      pontiac phoenix
## 374                 ford fairmont futura
## 375                  volkswagen rabbit l
## 376                   mazda glc custom l
## 377                     mazda glc custom
## 378               plymouth horizon miser
## 379                       mercury lynx l
## 380                     nissan stanza xe
## 381                         honda accord
## 382                       toyota corolla
## 383                          honda civic
## 384                   honda civic (auto)
## 385                        datsun 310 gx
## 386                buick century limited
## 387    oldsmobile cutlass ciera (diesel)
## 388           chrysler lebaron medallion
## 389                       ford granada l
## 390                     toyota celica gt
## 391                    dodge charger 2.2
## 392                     chevrolet camaro
## 393                      ford mustang gl
## 394                            vw pickup
## 395                        dodge rampage
## 396                          ford ranger
## 397                           chevy s-10
```

```r
pairs(Auto)
```

![](March_21_2018_KN_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
# 9. This question uses the variables dis (the weighted mean of distances to five Boston employment centers) and nox (nitrogen oxides concen- tration in parts per 10 million) from the Boston data. We will treat dis as the predictor and nox as the response.


```r
## (a) Use the poly() function to fit a cubic polynomial regression to predict nox using dis. Report the regression output, and plot the resulting data and polynomial fits.
library(MASS);head(Boston)
```

```
##      crim zn indus chas   nox    rm  age    dis rad tax ptratio  black
## 1 0.00632 18  2.31    0 0.538 6.575 65.2 4.0900   1 296    15.3 396.90
## 2 0.02731  0  7.07    0 0.469 6.421 78.9 4.9671   2 242    17.8 396.90
## 3 0.02729  0  7.07    0 0.469 7.185 61.1 4.9671   2 242    17.8 392.83
## 4 0.03237  0  2.18    0 0.458 6.998 45.8 6.0622   3 222    18.7 394.63
## 5 0.06905  0  2.18    0 0.458 7.147 54.2 6.0622   3 222    18.7 396.90
## 6 0.02985  0  2.18    0 0.458 6.430 58.7 6.0622   3 222    18.7 394.12
##   lstat medv
## 1  4.98 24.0
## 2  9.14 21.6
## 3  4.03 34.7
## 4  2.94 33.4
## 5  5.33 36.2
## 6  5.21 28.7
```

```r
i<-2
fit.glm<-glm(nox~poly(dis,i),data=Boston)
predict.nox<-predict(fit.glm,newdata=Boston$dis) # does not work...
```

```
## Error in eval(predvars, data, env): numeric 'envir' arg not of length one
```

```r
ggplot(Boston,aes(nox,dis))+geom_point(alpha=0.1) + geom_line(nox,predict.nox)
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```

```r
##  (b) Plot the polynomial fits for a range of different polynomial degrees (say, from 1 to 10), and report the associated residual sum of squares.


## (c) Perform cross-validation or another approach to select the opti- mal degree for the polynomial, and explain your results.

## (d) Use the bs() function to fit a regression spline to predict nox using dis. Report the output for the fit using four degrees of freedom. How did you choose the knots? Plot the resulting fit.

## (e) Now fit a regression spline for a range of degrees of freedom, and plot the resulting fits and report the resulting RSS. Describe the results obtained.

## (f) Perform cross-validation or another approach in order to select the best degrees of freedom for a regression spline on this data. Describe your results.
```