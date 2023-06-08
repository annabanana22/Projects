library(rstudioapi)
library(readxl)
library(ggplot2)
library(tidyverse)


setwd("/Users/Anna/Dropbox/2. ACADEMIA/2.3. CCiL Master/2nd semester CCiL/Empirical Linguistics [EL]/Assignment 2 - Praat")
getwd()

# let's upload our data
classData <- read_excel("voice_features_results.xlsm")
Anna2Anna3_Data <- read_excel("Anna2_and_Anna3_excel.xlsm")

# let's convert our character values to factors
classData <- mutate_if(classData,is.character,as.factor)
Anna2Anna3_Data <- mutate_if(Anna2Anna3_Data,is.character,as.factor)

# Let's calculate the rounded mean of the age of the participants

mean_age = round(mean(classData$Age))
print(mean_age)

# CALCULATING THE MEANS OF EACH FEATURE:

### MPD (s)

mean_MPD = mean (classData$MPD)
sd_MPD = sd(classData$MPD)

print(mean_MPD)
print(sd_MPD)

### F0 tone (Hz) // FEMALES & MALES

mean_F0_F = mean(classData$F0_tone[classData$Sex=="Female"])
sd_F0_F = sd(classData$F0_tone[classData$Sex=="Female"])

print(mean_F0_F)
print(sd_F0_F)

mean_F0_M = mean(classData$F0_tone[classData$Sex=="Male"])
sd_F0_M = sd(classData$F0_tone[classData$Sex=="Male"])

print(mean_F0_M)
print(sd_F0_M)

### Shimmer (%) // FEMALES & MALES
# must multiply Praat values times 100 to obtain percentages

mean_Shimmer_F = mean(classData$Shimmer[classData$Sex=="Female"])*100
sd_Shimmer_F = sd(classData$Shimmer[classData$Sex=="Female"])*100

print(mean_Shimmer_F)
print(sd_Shimmer_F)

mean_Shimmer_M = mean(classData$Shimmer[classData$Sex=="Male"])*100
sd_Shimmer_M = sd(classData$Shimmer[classData$Sex=="Male"])*100

print(mean_Shimmer_M)
print(sd_Shimmer_M)

### Jitter (%) // FEMALES & MALES
# must multiply Praat values times 100 to obtain percentages

mean_Jitter_F = mean(classData$Jitter[classData$Sex=="Female"])*100
sd_Jitter_F = sd(classData$Jitter[classData$Sex=="Female"])*100

print(mean_Jitter_F)
print(sd_Jitter_F)

mean_Jitter_M = mean(classData$Jitter[classData$Sex=="Male"])*100
sd_Jitter_M = sd(classData$Jitter[classData$Sex=="Male"])*100

print(mean_Jitter_M)
print(sd_Jitter_M)

### HNR (dB)

mean_mHNR = mean (classData$Mean_HNR)
sd_mHNR = sd(classData$Mean_HNR)

print(mean_mHNR)
print(sd_mHNR)



### MAKING GRAPHS:

# DURATION boxplot

MPD_Anna1 = classData$MPD[13]
print(MPD_Anna1)

MPD_Anna2 = Anna2Anna3_Data$MPD[1]
print(MPD_Anna2)

MPD_Anna3 = Anna2Anna3_Data$MPD[2]
print(MPD_Anna3)

ggplot(classData, aes(y= MPD)) + 
  geom_boxplot(outlier.colour="black",
                outlier.size=1.5)+coord_flip()+
  labs(title = "Maximum Phonation Duration across participants",
       y = 'MPD (s)')+
  geom_point(aes(x=0, y=MPD_Anna1), size=1.5)+
  geom_point(aes(x=0, y=MPD_Anna2), size=1.5)+
  geom_point(aes(x=0, y=MPD_Anna3), size=1.5)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  annotate(geom="text", x=-0.06, y=8.9, label="AC1",
           color="red")+
  annotate(geom="text", x=0.06, y=9.46, label="AC2",
           color="red")+
  annotate(geom="text", x=-0.05, y=13.68, label="AC3",
           color="red")+
  stat_summary(geom = "point", mapping = aes(x=-0.5), fun = "mean", size = 2.0, color="red", shape=6)
  

# F0 TONE boxplot with two categorical classes, female and male

F0_Anna1 = classData$F0_tone[13]
print(F0_Anna1)

F0_Anna2 = Anna2Anna3_Data$F0_tone[1]
print(F0_Anna2)

F0_Anna3 = Anna2Anna3_Data$F0_tone[2]
print(F0_Anna3)

ggplot(classData, aes(x = Sex, y= F0_tone, color = Sex)) + 
  geom_boxplot(outlier.colour="black",
               outlier.size=1.5)+
  labs(title = "Fundamental frequency across participants",
     y = 'Frequency (Hz)', x = "Sex")+
  geom_point()+
  geom_point(aes(x=1.05, y=F0_Anna1), colour="black", size=1.5)+
  geom_point(aes(x=0.95, y=F0_Anna2), colour="black", size=1.5)+
  geom_point(aes(x=1, y=F0_Anna3), colour="black", size=1.5)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  annotate(geom="text", x=1.2, y=137, label="AC1",
           color="red")+
  annotate(geom="text", x=0.8, y=140, label="AC2",
           color="red")+
  annotate(geom="text", x=0.85, y=153, label="AC3",
           color="red")+
  stat_summary(geom = "point", mapping = aes(x=0.5), fun = "mean", size = 2.0, color="red", shape=6)



# SHIMMER (%!! multiplied by 100) according to males and females

### in Excel

# JITTER (%!! multiplied by 100) according to males and females

### in Excel

# HNR boxplot

HNR_Anna1 = classData$Mean_HNR[13]
print(HNR_Anna1)

HNR_Anna2 = Anna2Anna3_Data$Mean_HNR[1]
print(HNR_Anna2)

HNR_Anna3 = Anna2Anna3_Data$Mean_HNR[2]
print(HNR_Anna3)

ggplot(classData, aes(y= Mean_HNR)) + 
  geom_boxplot(outlier.colour="black",
               outlier.size=1.5)+coord_flip()+
  labs(title = "Mean Harmonics-To-Noise Ratio across participants",
       y = 'MHN (dB)')+
  geom_point(aes(x=0, y=HNR_Anna1), size=1.5)+
  geom_point(aes(x=0, y=HNR_Anna2), size=1.5)+
  geom_point(aes(x=0, y=HNR_Anna3), size=1.5)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  annotate(geom="text", x=-0.06, y=15.5, label="AC1",
           color="red")+
  annotate(geom="text", x=+0.06, y=16.8, label="AC2",
           color="red")+
  annotate(geom="text", x=-0.06, y=19.3, label="AC3",
           color="red")+
  stat_summary(geom = "point", mapping = aes(x=0.5), fun = "mean", size = 2.0, color="red", shape=6)



