library("readr")
data <- read_csv("http://www.stats.ox.ac.uk/~laws/SB1/data/swim.csv")

# Relationship of time with different factors
par(mfrow=(c(2,3)))
pieces <- split(data,data$dist)
boxplot(time~sex, data=pieces[[1]])
boxplot(time~course,data=pieces[[1]])
boxplot(time~stroke,data=pieces[[1]])

boxplot(time~sex, data=pieces[[4]])
boxplot(time~course,data=pieces[[4]])
boxplot(time~stroke,data=pieces[[4]])

pieces2 <- split(data,data[,c('dist','stroke')])
count <- matrix(sapply(pieces2,nrow),ncol=5)
rownames(count) <- c('50m','100m','200m','400m')
colnames(count) <- c('Backstroke','Breaststroke','Butterfly','Freestyle','Medley')

## Balanced?
par(mfrow=c(1,1))

barplot(count,col=c('#606060','#808080','#A0A0A0','#C0C0C0','#E0E0E0'))
#title('Frequency of Different Stroke and Distance')
legend("topright",legend = c('50m','100m','200m','400m'),
       fill=c('#606060','#808080','#A0A0A0','#C0C0C0','#E0E0E0'))

### Box-Cox Transformation

par(mfrow=c(1,2))
library('MASS')
boxcox(time~dist+sex+stroke+course,data=data)
bc <- boxcox(time~dist+sex+stroke+course,data=data,lambda=seq(0.8,1,by=0.02))
lambda <- bc$x[which.max(bc$y)]

### Hypothesis Test

full <- lm(I(time**lambda)~1+dist+stroke+sex+course,data=data)
summary(full)

full_stroke <- lm(I(time**lambda)~1+dist+sex+course,data=data)
anova(full,full_stroke)

full_course <- lm(I(time**lambda)~1+dist+sex+stroke,data=data)
anova(full,full_course)

full_sex <- lm(I(time**lambda)~1+dist+course+stroke,data=data)
anova(full,full_sex)

full_dist <- lm(I(time**lambda)~1+sex+course+stroke,data=data)
anova(full,full_dist)

### Involve Interaction

inter <- lm(I(time**lambda)~1+dist*(stroke+sex+course),data=data)
inter_course <- lm(I(time**lambda)~1+dist*course+stroke+sex,data=data)
inter_stroke <- lm(I(time**lambda)~1+dist*stroke+sex+course,data=data)
inter_sex <- lm(I(time**lambda)~1+dist*sex+stroke+course,data=data)
anova(inter, inter_course)
anova(inter, inter_stroke)
anova(inter, inter_sex)

p <- length(inter$coefficients)
n <- nrow(data)
i <- cooks.distance(inter) > (8/(n - 2*p))

par(mfrow=c(2,2))
qqnorm(rstudent(inter), main = NULL, pch = 1 + 15*i, col = 1 + i)
qqline(rstudent(inter))

plot(fitted.values(inter), rstudent(inter), pch = 1 + 15*i, col = 1 + i)

plot(hatvalues(inter), ylim=c(0,0.1), pch = 1 + 15*i, col = 1 + i)
abline(2*p/n, 0, lty = 2)

plot(cooks.distance(inter), pch = 1 + 15*i, col = 1 + i)
abline(8/(n - 1*p), 0, lty = 2)

d <- c(50,100,200,400)
vard <- matrix(nrow=1,ncol=4)
for (i in 1:4){
  vard[,i]=var(rstudent(inter)[data$dist==d[i]])
}
vard=data.frame(vard)
colnames(vard)=d
w <- array(dim=n)
for (j in 1:n){
  distj = data[j,]$dist
  w[j] = vard[[as.character(distj)]]
}

inter.reweight <- lm(I(time**lambda)~1+dist*(stroke+sex+course),data=data,weights=1/w)

iw <- cooks.distance(inter.reweight) > (8/(n - 2*p))

par(mfrow=c(2,2))
qqnorm(rstudent(inter.reweight), main = NULL, pch = 1 + 15*iw, col = 1 + iw)
qqline(rstudent(inter.reweight))

plot(fitted.values(inter.reweight), rstudent(inter.reweight), 
     pch = 1 + 15*iw, col = 1 + iw)

plot(hatvalues(inter.reweight), ylim=c(0,0.1), pch = 1 + 15*iw, col = 1 + iw)
abline(2*p/n, 0, lty = 2)

plot(cooks.distance(inter.reweight), pch = 1 + 15*iw, col = 1 + iw)
abline(8/(n - 1*p), 0, lty = 2)

any(hatvalues(inter.reweight)[which(iw)]>2*p/n)

pieces3 <- split(data,data[,c('sex','course','stroke')])
par(mfrow=c(1,3))

df1 <- pieces3[['F.Long.Backstroke']]
x1 <- seq(80,220,by=2)
data1 <- data.frame(dist=x1,sex='F',course='Long',stroke='Backstroke')
pred1 <- (predict.lm(inter.reweight,newdata=data1))**(1/lambda)
plot(x1,pred1)
points(df1$dist,df1$time,col=2,pch=16)

df2 <- pieces3[['M.Long.Backstroke']]
x2 <- seq(80,220,by=2)
data2 <- data.frame(dist=x1,sex='M',course='Long',stroke='Backstroke')
pred2 <- (predict.lm(inter.reweight,newdata=data2))**(1/lambda)
plot(x2,pred2)
points(df2$dist,df2$time,col=2,pch=16)

df3 <- pieces3[['F.Short.Breaststroke']]
x3 <- seq(40,220,by=2)
data3 <- data.frame(dist=x3,sex='F',course='Short',stroke='Breaststroke')
pred3 <- (predict.lm(inter.reweight,newdata=data3))**(1/lambda)
plot(x3,pred3)
points(df3$dist,df3$time,col=2,pch=16)

newdf <- data.frame(name=c('RaceA','RaceB','RaceC','RaceD'),
                    dist=c(400,50,100,100),
                    stroke=c('Freestyle','Backstroke','Butterfly','Medley'),
                    sex='F',
                    course='Long')
pred.times <- (predict.lm(inter.reweight,newdata=newdf))**(1/lambda)
pred.intervals <- (predict.lm(inter.reweight,newdata=newdf,interval='prediction'))**(1/lambda)

