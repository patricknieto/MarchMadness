

library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)

TourneySeeds <- read.csv("../Input/TourneySeeds.csv")
SampleSubmission <- read.csv("../Input/Sample_Submission.csv")
Seasons <- read.csv("../Input/Seasons.csv")
Teams <- read.csv("../Input/Teams.csv")
TourneySlots <- read.csv("../Input/TourneySlots.csv")
TourneyDetailedResults <- read.csv("../Input/TourneyDetailedResults.csv")
TourneyCompactResults <- read.csv("../Input/TourneyCompactResults.csv")


head(TourneySeeds)
ggplot(TourneySeeds) +
  geom_bar(aes(Seed))

unique(TourneySeeds$Seed)
TourneySeeds <- TourneySeeds %>% 
  mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed)) %>% select(Season, Team, SeedNum)

games.to.predict <- cbind(SampleSubmission$id,
                          colsplit(SampleSubmission$id,
                                   "_",
                                   names = c('season', 'team1', 'team2'))
                          )

temp <- left_join(games.to.predict, TourneySeeds, by=c("season"="Season", "team1"="Team"))
games.to.predict <- left_join(temp, TourneySeeds, by=c("season"="Season", "team2"="Team"))
colnames(games.to.predict)[c(1,5:6)] <- c("Id", "team1seed", "team2seed")
games.to.predict <- games.to.predict %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))



temp <- left_join(as.data.frame(TourneyCompactResults), TourneySeeds, by=c("Season", "Wteam"="Team"))
compact.results <- left_join(temp, TourneySeeds, by=c("Season", "Lteam"="Team"))
colnames(compact.results)[c(9:10)] <- c("team1seed", "team2seed")
head(compact.results)



set1 <- compact.results %>% select(team1seed, team2seed) %>% mutate(result=1)
set2 <- compact.results %>% select(team2seed, team1seed) %>% mutate(result=0)
colnames(set1) <- c("team1seed", "team2seed", "team1win")
colnames(set2) <- c("team1seed", "team2seed", "team1win")
full.set <- rbind(set1, set2)
full.set <- full.set %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))
head(full.set)



m.seed.diff <- lm(team1win ~ I(team2seed - team1seed), data=full.set)
summary(m.seed.diff)
games.to.predict$Pred <- predict(m.seed.diff, games.to.predict)


m.logistic.seed.diff <- glm(team1win~ I(team2seed-team1seed), data=full.set, family=binomial())
coef(m.logistic.seed.diff)
games.to.predict$Pred <- predict(m.logistic.seed.diff, games.to.predict, type="response")
write.csv(games.to.predict %>% select(Id, Pred), '../Output/logistic_seed_submission.csv', row.names=FALSE)
