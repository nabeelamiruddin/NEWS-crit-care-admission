library(tidyr)
library(dplyr)
library(zoo)
library(caTools)
library(randomForest)
library(class)
library(ggplot2)
library(MASS)
library(glmnet)

setwd("~/Machine learning/eObs")

eobs <- read.csv('eobs.csv')
ICU <- read.csv('ICU_july.csv')
MHDU <- read.csv('MHDU_july.csv')
SHDU <- read.csv('SHDU_july.csv')

eobs$AdmissionDtm <- as.POSIXct(eobs$AdmissionDtm, format = '%d/%m/%Y %H:%M')
eobs$ObsRecordedDtm <- as.POSIXct(eobs$ObsRecordedDtm, format = '%d/%m/%Y %H:%M')
eobs$hours_from_adm <- as.integer(round(difftime(eobs$ObsRecordedDtm, eobs$AdmissionDtm, units = 'hours')))
eobs$VisitIDCode <- as.factor(eobs$VisitIDCode)
eobs$Hospital.admission.date <- as.Date(eobs$AdmissionDtm)
names(eobs)[1] <- 'Hosp.No'

ICU$Unit.adm.date.time <- paste(ICU$Unit.admission.date,ICU$Unit.admission.time)
ICU$Unit.adm.date.time <- as.POSIXlt(ICU$Unit.adm.date.time, format = '%d/%m/%Y %H:%M:%S')
ICU$Hospital.admission.date <- as.Date(ICU$Hospital.admission.date, format = '%d/%m/%Y')
names(ICU)[1] <- 'Hosp.No'
ICU$ICU_adm <- as.factor('Yes')
ICU$Hosp.No <- as.factor(ICU$Hosp.No)

MHDU$Unit.adm.date.time <- paste(MHDU$Unit.admission.date,MHDU$Unit.admission.time)
MHDU$Unit.adm.date.time <- as.POSIXlt(MHDU$Unit.adm.date.time, format = '%d/%m/%Y %H:%M:%S')
MHDU$Hospital.admission.date <- as.Date(MHDU$Hospital.admission.date, format = '%d/%m/%Y')
names(MHDU)[1] <- 'Hosp.No'
MHDU$MHDU_adm <- as.factor('Yes')
MHDU$Hosp.No <- as.factor(MHDU$Hosp.No)

SHDU$Unit.adm.date.time <- paste(SHDU$Unit.admission.date,SHDU$Unit.admission.time)
SHDU$Unit.adm.date.time <- as.POSIXlt(SHDU$Unit.adm.date.time, format = '%d/%m/%Y %H:%M:%S')
SHDU$Hospital.admission.date <- as.Date(SHDU$Hospital.admission.date, format = '%d/%m/%Y')
names(SHDU)[1] <- 'Hosp.No'
SHDU$SHDU_adm <- as.factor('Yes')
SHDU$Hosp.No <- as.factor(SHDU$Hosp.No)

eobs <- eobs %>%
  filter(location != 'Emergency Department') %>%
  filter(location != 'Admissions Lounge') %>%
  filter(location != 'Day Case Unit') %>%
  filter(location != 'Day Surg Unit (Treatment Room)') %>%
  filter(location != 'Critical Care - SHDU') %>%
  filter(location != 'Critical Care - ITU') %>%
  filter(location != 'Medical HDU') %>%
  filter(Obs == 'NEWS Score')
eobs <- eobs[, -c(4:8)]

hrs <- data.frame(hours_from_adm = rep(1:105,1293))
visitid <- data.frame(VisitIDCode = rep(unique(eobs$VisitIDCode),105))

visitid <- visitid %>%
  arrange(VisitIDCode)
for_merge <- cbind(hrs,visitid)

eobs <- eobs %>%
  arrange(VisitIDCode,hours_from_adm) %>%
  group_by(VisitIDCode) %>%
  distinct(hours_from_adm,ObsValue,AdmissionDtm,Hosp.No)

eobs <- left_join(for_merge,eobs,by = c('VisitIDCode', 'hours_from_adm'))

eobs <- eobs[!duplicated(eobs[c(1,2)]),]

eobs <- eobs %>%
  group_by(VisitIDCode) %>%
  spread(hours_from_adm, ObsValue)

eobs <- eobs[!is.na(eobs$Hosp.No),] 
eobs$Hospital.admission.date <- as.Date(eobs$AdmissionDtm)

eobs <- left_join(eobs, ICU, by = c('Hosp.No','Hospital.admission.date'))
eobs <- left_join(eobs, MHDU, by = c('Hosp.No','Hospital.admission.date'))
eobs <- left_join(eobs, SHDU, by = c('Hosp.No','Hospital.admission.date'))

eobs$Crit_care_adm <- NA
eobs$Crit_care_adm <- ifelse(eobs$ICU_adm == 'Yes'|eobs$MHDU_adm == 'Yes'|eobs$SHDU_adm == 'Yes',1,0)
eobs$Crit_care_adm <- as.factor(ifelse(is.na(eobs$Crit_care_adm),0,1))

eobs1 <- eobs[,c(3:108,122)]

eobs1[2:106] <- lapply(eobs1[2:106],as.character)
eobs1[2:106] <- lapply(eobs1[2:106],as.numeric)
eobs1[2:106] <- replace(eobs1[2:106],eobs1[2:106]>12,NA)
eobs1 <- eobs1[-1]

no_crit <- filter(eobs1, Crit_care_adm == 0)
crit <- filter(eobs1, Crit_care_adm == 1)

cor_obs <- cor(eobs1[1:105])

no_crit_mean <- no_crit[1:105] %>%
  summarise_each(funs(mean(.,na.rm = TRUE)))
no_crit_mean <- stack(no_crit_mean)
no_crit_mean$hrs_from_adm <- seq(1,105)
crit_mean <- crit[1:105] %>%
  summarise_each(funs(mean(.,na.rm = TRUE)))
crit_mean <- stack(crit_mean)
crit_mean$hrs_from_adm <- seq(1,105)

mean_eobs_scores <- data.frame(crit_mean$values, no_crit_mean$values, crit_mean$hrs_from_adm)

ggplot(mean_eobs_scores) +
  geom_point(aes(x = crit_mean.hrs_from_adm,y = crit_mean.values), colour = 'blue') +
  stat_smooth(aes(x = crit_mean.hrs_from_adm,y = crit_mean.values), colour = 'blue') +
  geom_point(aes(x = crit_mean.hrs_from_adm,y = no_crit_mean.values), colour = 'red') +
  geom_smooth(aes(x = crit_mean.hrs_from_adm,y = no_crit_mean.values), colour = 'red') +
  ylim(0,6) +
  ggtitle('Mean eObs Scores for Patients Admitted to Critical Care (Blue) and not Admitted (Red)') +
  xlab('Time since admission (hours)') +
  ylab('Mean eObs score')

eobs1 <- eobs1[,-c(25:105)]

set.seed(123)
split = sample.split(eobs1$Crit_care_adm, SplitRatio = 0.75)
train.eobs = subset(eobs1, split == TRUE)
test.eobs = subset(eobs1, split == FALSE)

glm.eobs <- glm(Crit_care_adm ~ ., 
                data = train.eobs, 
                family = binomial)
summary(glm.eobs)

lda.eobs <- lda(Crit_care_adm ~ `15`,
                data = train.eobs)


rf.eobs <- randomForest(Crit_care_adm ~ `12` + `14`,
                        data = train.eobs,
                        ntree = 5000,
                        importance = TRUE,
                        na.action = na.roughfix)
rf.eobs.pred <- predict(rf.eobs, newdata = test.eobs)
table(rf.eobs.pred)

train.eobs <- na.locf(train.eobs, fromLast = TRUE)
knn.eobs <- knn(train = train.eobs[,-106],
                test = test.eobs[,-106],
                cl = train.eobs[,106],
                k = 5)