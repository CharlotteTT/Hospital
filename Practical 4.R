## read data
dat <- read.csv("admissions.csv") 
head(dat)
dim(dat)
dat$admission<-as.factor(dat$admission)
levels(dat$admission)
dat$provnum<-as.factor(dat$provnum)
summary(dat)
##exploratory 
require(lattice)
# Pooled data
histogram(dat$provnum ~ admission|as.factor(dat$died))
# for the entire period
table(dat$admission)
#plot of pr differ by hospital
require(ggplot2)
fr<-table(dat$admission, dat$died)
p<-rep(c("Elective", "Emergency","Urgent"),each=ncol(fr))
fr<-data.frame(rep(colnames(fr),3), as.vector(t(fr)),p)
colnames(fr)<-c("Died","Frequency", "Admission")
fr
ggplot(fr, aes(x= Died, y = Frequency, fill=Admission))+
  geom_bar(stat="identity",position = position_stack(reverse = TRUE)) +
  geom_text(aes(y=Frequency, label=Frequency), vjust=1.2, color="white", size=3, position = position_stack(reverse = TRUE)) + 
  ylab("Frequency") 

#plot of pr differ by age
histogram(dat$provnum ~ admission|as.factor(dat$age80))
fr<-table(dat$admission, dat$age)
p<-rep(c("Elective", "Emergency","Urgent"),each=ncol(fr))
fr<-data.frame(rep(colnames(fr),3), as.vector(t(fr)),p)
colnames(fr)<-c("Age","Frequency", "Admission")
fr
ggplot(fr, aes(x= Age, y = Frequency, fill=Admission))+
  geom_bar(stat="identity",position = position_stack(reverse = TRUE)) +
  geom_text(aes(y=Frequency, label=Frequency), vjust=1.2, color="white", size=3, position = position_stack(reverse = TRUE)) + 
  ylab("Frequency") 

# Fitting models
require(nnet)
model<-multinom(admission~died+white+los+age80+age,dat=dat)
summary(model)
require(VGAM)
vglm.is<-vglm(admission~died+white+los+age80+age,family=multinomial,dat=dat)
summary(vglm.is)

## Model selection
require(car)
Anova(model)
require(MuMIn)
options(na.action="na.fail")
head(dredge(model))
step(model,direction="both")
model_final<-multinom(admission~died+white+los,dat=dat)
Anova(model_final)
summary(model_final)
confint(model_final)
qchisq(0.05,2,lower.tail=FALSE)
pchisq(19.519,2,lower.tail=FALSE)

## Interpretation
coef(model_final)
predict(model_final,newdata=data.frame(died=0,white=0,los=0),type="probs")
log(0.77476773/0.02285664)
require(MASS)
require(effects)
dat$died<-as.factor(dat$died)
dat$white<-as.factor(dat$white)
polrSimple<-MASS::polr(admission ~ died+white+los,dat=dat) 
plot(effect("died", polrSimple))
plot(effect("white", polrSimple))
plot(
  (effect("los", polrSimple)),
  xlab="Length of stay in hospital",
  ylab="Probability of admission",
  main="Effect plot of Length of stay")

## assumption check
par(mfrow=c(1,3))
# convert elective to 1, else other
dat$admissionel<-as.factor(ifelse(dat$admission=='Elective', 1, 0))
b1<-glm(admissionel ~ died+white+los, dat=dat, family=binomial)
plot(fitted(b1), residuals(b1, type='pearson'), main='Elective',xlab='fitted value elective', ylab='pearson residual')
abline(h=0)
dat$admissionem<-as.factor(ifelse(dat$admission=='Emergency', 1, 0))
b2<-glm(admissionem ~ died+white+los, dat=dat, family=binomial)
plot(fitted(b2), residuals(b2, type='pearson'), main='Emergency',xlab='fitted value emergency', ylab='pearson residual')
abline(h=0)
dat$admissionur<-as.factor(ifelse(dat$admission=='Urgent', 1, 0))
b3<-glm(admissionur ~ died+white+los, dat=dat, family=binomial)
plot(fitted(b3), residuals(b3, type='pearson'), main='Urgent',xlab='fitted value urgent', ylab='pearson residual')
abline(h=0)

par(mfrow=c(1,3))
plot(residuals(model_final)[1:100,2], type='l', ylab='raw residuals')

pred<-predict(model_final, newdata=dat, type='probs')
pred
# log odds is log(pj/p1) where p1 is elective
pEmgelec<-log(pred[,2]/pred[,1])
plot(dat$los, pEmgelec, xlab='Length of Stay (days)', ylab='Log Odds', main='Emergency vs Elective')
pUrgelec<-log(pred[,3]/pred[,1])
plot(dat$los, pUrgelec, xlab='Length of Stay (days)', ylab='Log Odds', main='Urgent vs Elective')

mn.sat<-multinom(admission~provnum,dat=dat)
dev<-deviance(model_final)-deviance(mn.sat)
dev
n<-dim(dat)[1]
n
vglm.is<-vglm(admission~died+white+los,family=multinomial,dat=dat)
vglm.null<-vglm(admission~1,family=multinomial,dat=dat)
1-logLik(vglm.is)/logLik(vglm.null)
