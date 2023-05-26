library(tidyverse)
library(rpart)
library(rattle)
library(class)
library(dplyr)
library(randomForest)
library(caret)
library(caretEnsemble)
library(lattice)
library(stringr)
library(ipred)
library(MASS)
library(pls)
library(e1071)
library("readxl")
library(caTools)

soccer1<-read_excel("FIFA22.xlsx") 

str(soccer1)
summary(soccer1)
length(select_if(soccer1, is.numeric))
length(select_if(soccer1, is.character))

table(soccer1$player_positions)


soccer.new<-soccer1%>%
  mutate(gen.position= ifelse(player_positions == "GK",
                              "GK",
                              ifelse(player_positions == "RW"|player_positions=="RF"|player_positions == "LF"|player_positions == "CF"|player_positions== "ST"|player_positions=="LS"|player_positions=="RS"|player_positions=="LW",
                                     "FWD",
                                     ifelse(player_positions=="LM"|player_positions=="RCM"|player_positions=="CM"|player_positions=="CAM"|player_positions=="CDM"|player_positions=="RAM"|player_positions=="LAM"|player_positions=="RM"|player_positions=="LCM"|player_positions=="RDM"|player_positions=="LDM",
                                            "MID", 
                                            ifelse( player_positions=="LWB"|player_positions=="LB"|player_positions=="LCB"|player_positions=="CB"|player_positions=="RCB"|player_positions=="RB"|player_positions=="RWB",
                                                    "DEF",
                                                    "none"))
                              )))


table(soccer.new$gen.position)

# normalize the values with NA to 10 in columns 25 to 65
soccer.new[, 25:65][is.na(soccer.new[, 25:65])] <- 10
summary(soccer.new)

soccer.updated<-soccer.new
soccer.new<-head(soccer.updated, 100)

rows<- sample(100:nrow(soccer.updated),
              size = 400,
              replace= FALSE)

soccer.new<- rbind(soccer.new,soccer.updated[rows,])

# age distribution
ggplot(soccer.new,aes(age))+
  geom_histogram(color="black",aes(fill=age))+
  ggtitle("Distribution based on player age")

# Distribution based General Position
ggplot(soccer.new,aes(gen.position))+
  geom_bar(color="black",aes(fill=gen.position))+
  ggtitle("Distribution based on general position in sample")

# Distribution based General Position for all Players in data-set
ggplot(soccer.updated,aes(gen.position))+
  geom_bar(color="black",aes(fill=gen.position))+
  ggtitle("Distribution based on general position in full dataset")


# split training and testing data
seed <- 88
set.seed(seed)
split = sample.split(soccer.new$gen.position, SplitRatio = 0.8)
split

soccer.new.train = subset(soccer.new, split == TRUE)
soccer.new.test = subset(soccer.new, split == FALSE)

## KNN with caret
trainControl1 <- trainControl(classProbs = TRUE)

knn.caret.pos<-train(factor(gen.position) ~ age+ overall+ attacking_finishing+ dribbling+
                       attacking_crossing+attacking_heading_accuracy+attacking_short_passing+
                       attacking_volleys+skill_curve+
                       skill_fk_accuracy+skill_long_passing+skill_ball_control+pace+movement_agility+ 
                       movement_reactions+movement_balance+power_shot_power+physic+ 
                       power_long_shots+ mentality_interceptions+ 
                       mentality_positioning+mentality_vision+ mentality_penalties+ 
                       mentality_composure+defending_marking_awareness+ defending_standing_tackle+
                       defending_sliding_tackle+ goalkeeping_handling,
                     data = soccer.new.train,
                     method="knn", 
                     tuneLength = 12,
                     na.action=na.exclude)



##KNN function-
set.seed(73)
class(soccer.new$attacking_finishing)

summary(soccer.new$attacking_finishing)
summary(soccer.new$movement_agility)

#Let's look at the different ways in which combination variables play a specific role in a player's overall rating group.
Finishing.grid<- seq(from = 1, to= 100,by= 1 )
agility.grid<- seq(from= 1, to = 100, by= 1)

grid2<-expand.grid(Finishing.grid, agility.grid)
colnames(grid2)<- c("Finishing", "Agility")

predictions2<- knn(soccer.new.test%>%dplyr::select(attacking_finishing,movement_agility),
                   grid2, soccer.new.test$gen.position,k=13)

grid.data2<-data.frame(grid2,predictions2)

##Graph 1 of knn
grid.data2%>%
  ggplot(aes(x=Finishing, y=Agility)) + 
  geom_point(aes(color = factor(predictions2)),
             size=2,
             alpha=0.1,
             inherit.aes = TRUE) + 
  geom_point(data = soccer.new,
             mapping = aes(x=attacking_finishing,
                           y=movement_agility,
                           color=gen.position),
             size=2) + 
  guides(color=guide_legend(title= "Position")) + 
  ggtitle("KNN Actual and Predicted Positions for K = 13") + 
  xlab("Finishing Rating") + 
  ylab("Agility Rating") 


#Confusion Matrix
confusionMatrix(knn.caret.pos, mode = "everything")


# LDA function
lda1.pos <- lda(gen.position~ age+ overall+ attacking_finishing+ dribbling+
                  attacking_crossing+attacking_heading_accuracy+attacking_short_passing+
                  attacking_volleys+skill_curve+
                  skill_fk_accuracy+skill_long_passing+skill_ball_control+pace+movement_agility+ 
                  movement_reactions+movement_balance+power_shot_power+physic+ 
                  power_long_shots+ mentality_interceptions+ 
                  mentality_positioning+mentality_vision+ mentality_penalties+ 
                  mentality_composure+defending_marking_awareness+ defending_standing_tackle+
                  defending_sliding_tackle+ goalkeeping_handling,
                  data=soccer.new)


#Make some predictions and visualize their classification
predictions3 <- predict(lda1.pos)$class

new.default <- data.frame(soccer.new, predictions3)

new.default %>%
  ggplot(aes(x = mentality_interceptions,
             y = attacking_finishing)) + 
  geom_point(aes(color = predictions3))

lda1.pos$prior 

# create training and testing sample set
indexes<-sample(nrow(soccer.new),size = nrow(soccer.new)*.8)
soccer.train<-soccer.new[indexes,]
soccer.test <-soccer.new[-indexes,]

lda.caret<-train(factor(gen.position) ~ age+ overall+ attacking_finishing+ dribbling+
                   attacking_crossing+attacking_heading_accuracy+attacking_short_passing+
                   attacking_volleys+skill_curve+
                   skill_fk_accuracy+skill_long_passing+skill_ball_control+pace+movement_agility+ 
                   movement_reactions+movement_balance+power_shot_power+physic+ 
                   power_long_shots+ mentality_interceptions+ 
                   mentality_positioning+mentality_vision+ mentality_penalties+ 
                   mentality_composure+defending_marking_awareness+ defending_standing_tackle+
                   defending_sliding_tackle+ goalkeeping_handling, 
                 data = soccer.train,
                 method = "lda")




lda.soccer.model<-lda(gen.position~ age+ overall+ attacking_finishing+ dribbling+
                        attacking_crossing+attacking_heading_accuracy+attacking_short_passing+
                        attacking_volleys+skill_curve+
                        skill_fk_accuracy+skill_long_passing+skill_ball_control+
                        pace+movement_agility+ 
                        movement_reactions+movement_balance+power_shot_power+physic+ 
                        power_long_shots+ mentality_interceptions+ 
                        mentality_positioning+mentality_vision+ mentality_penalties+ 
                        mentality_composure+defending_marking_awareness+ defending_standing_tackle+
                        defending_sliding_tackle+ goalkeeping_handling,
                      data=soccer.train)



pred.lda <- predict(lda.soccer.model, soccer.test)$class
#pred.lda <- predict(lda.soccer.model)$class


confusionMatrix(lda.caret, mode = "everything")
cor.test(soccer.new$movement_agility, soccer.new$attacking_finishing)
cor.test(soccer.new$mentality_interceptions, soccer.new$attacking_finishing)

# making predictions
model.data<-data.frame(pred.lda,
                       obs=1:length(pred.lda))
colors <- colorRampPalette(c("blue", "green", "yellow", "red"))

gathered.model.data <- model.data %>%
  gather(key = "model",
         value = "pred",
         -obs)

gathered.model.data %>%
  filter(obs <= 50) %>%
  ggplot(aes(x = model,
             y = obs)) +
  geom_tile(aes(fill = factor(pred)),
            color = "black")+scale_color_brewer(palette="Dark2")


# the accuracy of model
true.vals <-soccer.train$gen.position 
model.data <- data.frame(lda= pred.lda == true.vals,
                         obs=1:length(pred.lda))

gathered.model.data1 <- model.data %>%
  gather(key = "model",
         value = "pred",
         -obs)

gathered.model.data1 %>%
  filter(obs <= 50) %>%
  ggplot(aes(x = model,
             y = obs)) +
  geom_tile(aes(fill = factor(pred)),
            color = "black")+scale_color_brewer(palette="Dark2")

lda.caret
