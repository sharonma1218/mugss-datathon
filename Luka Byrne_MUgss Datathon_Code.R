library(tidyverse)
library(GGally)
df <- read.csv("/Users/lukabyrne/Downloads/mxmh_survey_results.csv")

#random EDA
ggpairs(df, columns = c(2,4,5,6,28:31), lower = list(continuous = wrap("points", size = 0.1)))
ggplot(df, aes(x = Foreign.languages, y = Anxiety)) + geom_boxplot()
plot(df$Hours.per.day, df$Anxiety)
abline(df$Hours.per.day, df$Anxiety)
class(df$BPM)
plot(df$BPM, df$Anxiety)
df1 <- df %>% filter(BPM < 500)
plot(df1$BPM, df1$Anxiety)
abline(df1$BPM, df1$Anxiety)
ggplot(df, aes(x = Instrumentalist, y = Anxiety)) + geom_boxplot()

#cleaning dataset
df1 <- df %>% filter(While.working == "No" | While.working == "Yes")

df1$While.working <- as.factor(df1$While.working)
class(df1$While.working)

df1 <- df1 %>% filter(Music.effects == "Improve" |
                        Music.effects == "No effect" |
                        Music.effects == "Worsen")

#construct average mental health indicator
df1$Average <- rowMeans(df1[,c("Anxiety", "Depression", "Insomnia", "OCD")])

#Boxplots
ggplot(df1, aes(x = While.working, y = Anxiety, fill = While.working)) +
  geom_boxplot() + theme(axis.title.y=element_text(size=20,face="bold"))
ggplot(df1, aes(x = While.working, y = Depression, fill = While.working)) +
  geom_boxplot() + theme(axis.title.y=element_text(size=20,face="bold"))
ggplot(df1, aes(x = While.working, y = Insomnia, fill = While.working)) +
  geom_boxplot() + theme(axis.title.y=element_text(size=20,face="bold"))
ggplot(df1, aes(x = While.working, y = OCD, fill = While.working)) +
  geom_boxplot() + theme(axis.title.y=element_text(size=20,face="bold"))

#regressions
library(stargazer)
mod1 <- lm(Anxiety ~ While.working + Age, data = df1)
mod2 <- lm(Depression ~ While.working + Age, data = df1)
mod3 <- lm(Insomnia ~ While.working + Age, data = df1)
mod4 <- lm(OCD ~ While.working + Age, data = df1)
mod5 <- lm(Average ~ While.working + Age, data = df1)

stargazer(mod1, mod2, mod3, mod4, mod5)



#failed pie chart
df %>%
  filter(While.working=="No" | While.working=="Yes")%>%
  mutate(n=n())%>%
  ggplot(aes(x="",y=n,fill=While.working))+
  geom_bar(stat="identity",width=1)+
  coord_polar("y",start=0)+
  theme_void()+
  geom_text(aes(y = mean(n), label = While.working), color = "white", size=6) +
  labs(title="Proportion who listen to music while working",
       fill="While.working")


#investigating exploratory variable
ggplot(df1, aes(x = Exploratory, y = Average, fill = Exploratory)) +
  geom_boxplot()
summary(lm(Average ~ Exploratory + Age, data = df1))

#another failed pie chart
df1 %>%
  mutate(n=n())%>%
  ggplot(aes(x="",y=n,fill=Music.effects))+
  geom_bar(stat="identity",width=1)+
  coord_polar("y",start=0)+
  theme_void()+
  geom_text(aes(y = mean(n), label = Music.effects), color = "white", size=6) + labs(title="Proportion who listen to music while working",
                                                                                     fill="While.working")
