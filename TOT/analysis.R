#analysing aggregate file
#agg has following variables: meanacc, mean acc per condition
#we want to see main effects of prime type

#below is the lmer model for accuracy

h <- read.csv("CompleteData.csv", header=TRUE, sep=',')

for (i in 1:nrow(h)){
  if(h[i,5] == "R"){
    h[i,5] = "T"
  }
}
h <- within (h, { uid = factor(Subject) } )
h$Subject <- as.factor(h$Subject)
h$PrimeCondition <- as.factor(h$PrimeCondition)
h$PrimeFirstResp_ACC = as.factor(h$PrimeFirstResp_ACC)
h$TargetQuestion = as.factor(h$TargetQuestion)

library(lme4)

m1 <- glmer(TargetFirstResp_ACC ~ PrimeCondition*PrimeFirstResp_ACC + (1 | Subject) + (1| Stimuli2), data = h, family = binomial, control = glmerControl(optimizer = "Nelder_Mead"))
summary(m1)

m1 <- glmer(TargetFirstResp_ACC ~ PrimeCondition*PrimeFirstResp_ACC + (1 | uid), data = h, family = binomial, control = glmerControl(optimizer = "Nelder_Mead"))
summary(m1)

m2 <- glmer(TargetFirstResp_ACC ~ TargetQuestion*PrimeCondition + (1 | uid), data = h, family = binomial, control = glmerControl(optimizer = "Nelder_Mead"), nAGQ=10)
summary(m2)

#how to calculate anovas
#Must have one row per participant per condition

y <- read.csv("agg45.csv", header = TRUE, sep = ",")
colnames(y) = c("No", "Subject", "Gender", "Age", "totalquestions","correctquestions", "wrongquestions", 
                "meanacc", "correctprimequestions", "wrongprimequestions", "meanprimeacc", 
                "r_trials", "p_trials", "b_trials", "u_trials", 
                "r_correct_trials", "p_correct_trials", "b_correct_trials",
                "u_correct_trials", "r_prime_correct_trials", "p_prime_correct_trials", 
                "b_prime_correct_trials", "u_prime_correct_trials", "r_meanacc", "p_meanacc", "b_meanacc", "u_meanacc", 
                "r_prime_meanacc", "p_prime_meanacc", "b_prime_meanacc", "u_prime_meanacc", 
                "numknow", "numdontknow", "numOther", "numTOT", "prop_know", "prop_dontknow",
                "prop_Other", "prop_TOT", "p_retrieved_correct_trials", "p_notretrieved_correct_trials",
                "p_retrieved_meanacc", "p_not_retrieved_meanacc", "retrieved_r", "retrieved_p", "retrieved_b", 
                "retrieved_u", "not_retrieved_r", "not_retrieved_p", "not_retrieved_b", "not_retrieved_u", 
                "r_ret", "p_ret", "b_ret", "u_ret", 
                "r_notret","p_notret", "b_notret", "u_notret",
                "r_know", "r_dontknow", "r_other", "r_TOT",
                "p_know", "p_dontknow", "p_other", "p_TOT",
                "b_know", "b_dontknow", "b_other", "b_TOT",
                "u_know", "u_dontknow", "u_other", "u_TOT",
                "r_know_correct", "r_dontknow_correct", "r_other_correct", "r_TOT_correct",
                "p_know_correct", "p_dontknow_correct", "p_other_correct", "p_TOT_correct",
                "b_know_correct", "b_dontknow_correct", "b_other_correct", "b_TOT_correct",
                "u_know_correct", "u_dontknow_correct", "u_other_correct", "u_TOT_correct",
                "r_know_incorrect", "r_dontknow_incorrect", "r_other_incorrect", "r_TOT_incorrect",
                "p_know_incorrect", "p_dontknow_incorrect", "p_other_incorrect", "p_TOT_incorrect",
                "b_know_incorrect", "b_dontknow_incorrect", "b_other_incorrect", "b_TOT_incorrect",
                "u_know_incorrect", "u_dontknow_incorrect", "u_other_incorrect", "u_TOT_incorrect",
                "visible" )

y <- as.data.frame(y)
y <- subset(y, y$Subject!=10)
N = nrow(y)

Nforx <- N*4
x <- matrix( NA, nrow = Nforx, ncol = 3)
colnames(x) = c("Subject", "PrimeCondition", "MeanAcc")

PrimeConditionLabels = c("Sem", "Phon", "Both", "Unrelated")

j =1
for(i in 1:N){
  
  for(k in 1:4){
  
   x[j,1] = y[i,2]
   x[j,2] = PrimeConditionLabels[k]
   x[j,3] = y[i,23+k]
   
   j = j+1
  }
  
}

x <- as.data.frame(x)
x$MeanAcc = as.numeric(as.character(x$MeanAcc))
x$Subject <- as.factor(x$Subject)
prime_aov <- with(x, aov(MeanAcc ~ PrimeCondition + Error(Subject / PrimeCondition)))

summary(prime_aov)

#prime accuracy anova

p_anova <- matrix( NA, nrow = Nforx, ncol = 3)
colnames(p_anova) = c("Subject", "PrimeCondition", "MeanPrimeAcc")

PrimeConditionLabels = c("Sem", "Phon", "Both", "Unrelated")

j =1
for(i in 1:N){
  
  for(k in 1:4){
    
    p_anova[j,1] = y[i,2]
    p_anova[j,2] = PrimeConditionLabels[k]
    p_anova[j,3] = y[i,27+k]
    
    j = j+1
  }
  
}

p_anova <- as.data.frame(p_anova)
p_anova$MeanPrimeAcc = as.numeric(as.character(p_anova$MeanPrimeAcc))
p_anova$Subject = as.factor(p_anova$Subject)
acc_prime_aov <- with(p_anova, aov(MeanPrimeAcc ~ PrimeCondition + Error(Subject / PrimeCondition)))

summary(acc_prime_aov)

#plotting target accuracy data
library(reshape2)

p1 <- subset(x, x$PrimeCondition == "Sem")
p2 <- subset(x, x$PrimeCondition == "Phon")
p3 <- subset(x, x$PrimeCondition == "Both")
p4 <- subset(x, x$PrimeCondition == "Unrelated")

sem <- mean(p1$MeanAcc)
phon <- mean(p2$MeanAcc)
both <- mean(p3$MeanAcc)
unrel <- mean(p4$MeanAcc)

accdata <- matrix(NA, nrow = 4, ncol = 5)
colnames(accdata) = c("PrimeCondition", "MeanAcc", "Error", "CI_lower", "CI_upper")
accdata[,1] = PrimeConditionLabels
accdata[,2] = c(sem, phon, both, unrel)
accdata <- as.data.frame(accdata)
accdata$MeanAcc <- as.numeric(as.character(accdata$MeanAcc))

accdata$Error = c(qnorm(0.975)*(sd(p1$MeanAcc))/sqrt(nrow(p1)),
                        qnorm(0.975)*(sd(p2$MeanAcc))/sqrt(nrow(p2)),
                        qnorm(0.975)*(sd(p3$MeanAcc))/sqrt(nrow(p3)),
                        qnorm(0.975)*(sd(p4$MeanAcc))/sqrt(nrow(p4)))
accdata$CI_lower = accdata$MeanAcc - accdata$Error
accdata$CI_upper = accdata$MeanAcc + accdata$Error



library(ggplot2)
library(ggthemes)

ggplot(accdata,aes(x = PrimeCondition ,y = MeanAcc, fill = PrimeCondition)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.5)+
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, fill = PrimeCondition), 
                width=.1, color = "gray26", 
                position = position_dodge(0.6))+
             theme_gdocs()+
  ylim(0, 0.7)+
  xlab("Prime Type") + ylab("Mean Acc") + 
  ggtitle("Mean Target Accuracy by \n Prime Type") 

#plotting prime accuracy data

p1_prime <- subset(p_anova, p_anova$PrimeCondition == "Sem")
p2_prime <- subset(p_anova, p_anova$PrimeCondition == "Phon")
p3_prime <- subset(p_anova, p_anova$PrimeCondition == "Both")
p4_prime <- subset(p_anova, p_anova$PrimeCondition == "Unrelated")

sem_prime <- mean(p1_prime$MeanPrimeAcc)
phon_prime <- mean(p2_prime$MeanPrimeAcc)
both_prime <- mean(p3_prime$MeanPrimeAcc)
unrel_prime <- mean(p4_prime$MeanPrimeAcc)


accdata_prime <- matrix(NA, nrow = 4, ncol = 5)
colnames(accdata_prime) = c("PrimeCondition", "MeanAcc", "Error", "CI_lower", "CI_upper")
accdata_prime[,1] = PrimeConditionLabels
accdata_prime[,2] = c(sem_prime, phon_prime, both_prime, unrel_prime)
accdata_prime <- as.data.frame(accdata_prime)
accdata_prime$MeanAcc <- as.numeric(as.character(accdata_prime$MeanAcc))

accdata_prime$Error = c(qnorm(0.975)*(sd(p1_prime$MeanPrimeAcc))/sqrt(nrow(p1_prime)),
                     qnorm(0.975)*(sd(p2_prime$MeanPrimeAcc))/sqrt(nrow(p2_prime)),
                    qnorm(0.975)*(sd(p3_prime$MeanPrimeAcc))/sqrt(nrow(p3_prime)),
                    qnorm(0.975)*(sd(p4_prime$MeanPrimeAcc))/sqrt(nrow(p4_prime)))
accdata_prime$CI_lower = accdata_prime$MeanAcc - accdata_prime$Error
accdata_prime$CI_upper = accdata_prime$MeanAcc + accdata_prime$Error


library(ggplot2)
library(ggthemes)

ggplot(accdata_prime,aes(x = PrimeCondition ,y = MeanAcc, fill = PrimeCondition)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.5)+
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, fill = PrimeCondition), 
                width=.1, color = "gray26", 
                position = position_dodge(0.6))+
  geom_point(color = "gray26", position = 
               position_dodge(0.6), size = 0.7)+
  theme_gdocs()+
  ylim(0, 1)+
  xlab("Prime Type") + ylab("Mean Acc") + 
  ggtitle("Mean Prime Accuracy by \n Prime Type") 

#getting retrieval states -- now we're viewing it by prime type
#for all trials

retrieval <- read.csv("agg45.csv")
retrieval <- subset(retrieval, retrieval$value.Subject!=10)

mean_r_know <- mean(retrieval$value.r_know)
mean_r_dontknow <- mean(retrieval$value.r_dontknow)
mean_r_other <- mean(retrieval$value.r_other)
mean_r_TOT <- mean(retrieval$value.r_TOT)

mean_p_know <- mean(retrieval$value.p_know)
mean_p_dontknow <- mean(retrieval$value.p_dontknow)
mean_p_other <- mean(retrieval$value.p_other)
mean_p_TOT <- mean(retrieval$value.p_TOT)

mean_b_know <- mean(retrieval$value.b_know)
mean_b_dontknow <- mean(retrieval$value.b_dontknow)
mean_b_other <- mean(retrieval$value.b_other)
mean_b_TOT <- mean(retrieval$value.b_TOT)

mean_u_know <- mean(retrieval$value.u_know)
mean_u_dontknow <- mean(retrieval$value.u_dontknow)
mean_u_other <- mean(retrieval$value.u_other)
mean_u_TOT <- mean(retrieval$value.u_TOT)

primebystate <- matrix(NA, nrow = 16, ncol = 6)
colnames(primebystate) = c("PrimeCondition", "State", "MeanProportion", "Error",
                           "CI_lower", "CI_upper")

primebystate[1:16, 1] = c("R", "R", "R", "R",
                          "P", "P", "P", "P",
                          "B", "B", "B", "B",
                          "U", "U", "U", "U")
primebystate[1:16, 2] = c("Know", "Dont Know", "Other", "TOT",
                          "Know", "Dont Know", "Other", "TOT",
                          "Know", "Dont Know", "Other", "TOT",
                          "Know", "Dont Know", "Other", "TOT")

primebystate[1:16, 3] = c(mean_r_know, mean_r_dontknow, mean_r_other, mean_r_TOT,
                          mean_p_know, mean_p_dontknow, mean_p_other, mean_p_TOT,
                          mean_b_know, mean_b_dontknow, mean_b_other, mean_b_TOT,
                          mean_u_know, mean_u_dontknow, mean_u_other, mean_u_TOT)

primebystate[1:16, 4] = c(qnorm(0.975)*(sd(retrieval$value.r_know)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.r_dontknow)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.r_other)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.r_TOT)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.p_know)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.p_dontknow)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.p_other)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.p_TOT)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.b_know)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.b_dontknow)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.b_other)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.b_TOT)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.u_know)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.u_dontknow)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.u_other)) /sqrt(nrow(retrieval)),
                          qnorm(0.975)*(sd(retrieval$value.u_TOT)) /sqrt(nrow(retrieval))
                          )


primebystate <- as.data.frame(primebystate)
primebystate$MeanProportion <- as.numeric(as.character(primebystate$MeanProportion))
primebystate$Error <- as.numeric(as.character(primebystate$Error))

primebystate$CI_lower = primebystate$MeanProportion - primebystate$Error
primebystate$CI_upper = primebystate$MeanProportion + primebystate$Error

ggplot(primebystate,aes(x = PrimeCondition ,y = MeanProportion, fill = as.factor(State))) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "chartreuse4", "#999999"), 
                    name = "State")+
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, fill = State), 
                width=.1, color = "gray26", 
                position = position_dodge(0.7))+
  theme_minimal()+
  xlab("Retrieval State") + ylab("Mean Proportion of Responses") + 
  ggtitle("Mean Proportion of Retrieval States by \n Prime Type for ALL trials")

#for anova, we need one row per participant

NforStateprime = N*16

Anova_State <- matrix(NA, nrow = NforStateprime, ncol = 4)
colnames(Anova_State) = c("Subject", "PrimeCondition", "State", "MeanProportion")
state <- c("1_Know", "2_DontKnow", "3_Other", "4_TOT")
prime <- c("R", "P", "B", "U")
j =1

for(i in 1:N){
  retrievalcounter = 1
  for(l in 1:4){
    
    for(k in 1:4){
      
      Anova_State[j,1] = retrieval[i,2]
      Anova_State[j,2] = prime[l]
      Anova_State[j,3] = state[k]
      index = 59 + retrievalcounter
      Anova_State[j,4] = retrieval[i, index]
      
      j = j+1
      retrievalcounter = retrievalcounter+1
    }
  }
}


#now we have one row per participant, per prime and state 
Anova_State <- as.data.frame(Anova_State)
Anova_State$MeanProportion <- as.numeric(as.character(Anova_State$MeanProportion))
Anova_State$Subject <- as.factor(Anova_State$Subject)

state_prime_aov <- with(Anova_State, aov(MeanProportion ~ PrimeCondition*State + Error(Subject/(PrimeCondition*State) )))
summary(state_prime_aov)

#for correct trials ONLY

retrieval_correct <- read.csv("aggremovedlowtargets2.csv")

mean_r_know_correct <- mean(retrieval_correct$value.r_know_correct, na.rm = TRUE)
mean_r_dontknow_correct <- mean(retrieval_correct$value.r_dontknow_correct, na.rm = TRUE)
mean_r_other_correct <- mean(retrieval_correct$value.r_other_correct, na.rm = TRUE)
mean_r_TOT_correct <- mean(retrieval_correct$value.r_TOT_correct, na.rm = TRUE)

mean_p_know_correct <- mean(retrieval_correct$value.p_know_correct, na.rm = TRUE)
mean_p_dontknow_correct <- mean(retrieval_correct$value.p_dontknow_correct, na.rm = TRUE)
mean_p_other_correct <- mean(retrieval_correct$value.p_other_correct, na.rm = TRUE)
mean_p_TOT_correct <- mean(retrieval_correct$value.p_TOT_correct, na.rm = TRUE)

mean_b_know_correct <- mean(retrieval_correct$value.b_know_correct, na.rm = TRUE)
mean_b_dontknow_correct <- mean(retrieval_correct$value.b_dontknow_correct, na.rm = TRUE)
mean_b_other_correct <- mean(retrieval_correct$value.b_other_correct, na.rm = TRUE)
mean_b_TOT_correct <- mean(retrieval_correct$value.b_TOT_correct, na.rm = TRUE)

mean_u_know_correct <- mean(retrieval_correct$value.u_know_correct, na.rm = TRUE)
mean_u_dontknow_correct <- mean(retrieval_correct$value.u_dontknow_correct, na.rm = TRUE)
mean_u_other_correct <- mean(retrieval_correct$value.u_other_correct, na.rm = TRUE)
mean_u_TOT_correct <- mean(retrieval_correct$value.u_TOT_correct, na.rm = TRUE)

primebystate_correct <- matrix(NA, nrow = 16, ncol = 6)
colnames(primebystate_correct) = c("PrimeCondition", "State", "MeanProportion", "Error",
                           "CI_lower", "CI_upper")

primebystate_correct[1:16, 1] = c("R", "R", "R", "R",
                          "P", "P", "P", "P",
                          "B", "B", "B", "B",
                          "U", "U", "U", "U")
primebystate_correct[1:16, 2] = c("Know", "Dont Know", "Other", "TOT",
                          "Know", "Dont Know", "Other", "TOT",
                          "Know", "Dont Know", "Other", "TOT",
                          "Know", "Dont Know", "Other", "TOT")

primebystate_correct[1:16, 3] = c(mean_r_know_correct, mean_r_dontknow_correct, 
                                  mean_r_other_correct, mean_r_TOT_correct,
                          mean_p_know_correct, mean_p_dontknow_correct, 
                          mean_p_other_correct, mean_p_TOT_correct,
                          mean_b_know_correct, mean_b_dontknow_correct, 
                          mean_b_other_correct, mean_b_TOT_correct,
                          mean_u_know_correct, mean_u_dontknow_correct, 
                          mean_u_other_correct, mean_u_TOT_correct)

primebystate_correct[1:16, 4] = c(qnorm(0.975)*(sd(retrieval_correct$value.r_know_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.r_dontknow_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.r_other_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.r_TOT_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.p_know_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.p_dontknow_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.p_other_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.p_TOT_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.b_know_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.b_dontknow_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.b_other_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.b_TOT_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.u_know_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.u_dontknow_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.u_other_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct)),
                          qnorm(0.975)*(sd(retrieval_correct$value.u_TOT_correct, na.rm = TRUE)) /sqrt(nrow(retrieval_correct))
)


primebystate_correct <- as.data.frame(primebystate_correct)
primebystate_correct$MeanProportion <- as.numeric(as.character(primebystate_correct$MeanProportion))
primebystate_correct$Error <- as.numeric(as.character(primebystate_correct$Error))

primebystate_correct$CI_lower = primebystate_correct$MeanProportion - primebystate_correct$Error
primebystate_correct$CI_upper = primebystate_correct$MeanProportion + primebystate_correct$Error

ggplot(primebystate_correct,aes(x = PrimeCondition ,y = MeanProportion, fill = as.factor(State))) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "chartreuse4", "#999999"), 
                    name = "State")+
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, fill = State), 
                width=.1, color = "gray26", 
                position = position_dodge(0.7))+
  theme_minimal()+
  xlab("Retrieval State") + ylab("Mean Proportion of Responses") + 
  ggtitle("Correct Trials: Mean Proportion \n of States by Prime Type")

#for anova, we need one row per participant

NforStateprime = N*16

Anova_State_correct <- matrix(NA, nrow = NforStateprime, ncol = 4)
colnames(Anova_State_correct) = c("Subject", "PrimeCondition", "State", "MeanProportion")
state <- c("1_Know", "2_DontKnow", "3_Other", "4_TOT")
prime <- c("R", "P", "B", "U")
j =1

for(i in 1:N){
  retrievalcounter = 1
  for(l in 1:4){
    
    for(k in 1:4){
      
      Anova_State_correct[j,1] = retrieval_correct[i,2]
      Anova_State_correct[j,2] = prime[l]
      Anova_State_correct[j,3] = state[k]
      index = 75 + retrievalcounter
      Anova_State_correct[j,4] = retrieval_correct[i, index]
      
      j = j+1
      retrievalcounter = retrievalcounter+1
    }
  }
}


#now we have one row per participant, per prime and state 
Anova_State_correct <- as.data.frame(Anova_State_correct)
Anova_State_correct$MeanProportion <- as.numeric(as.character(Anova_State_correct$MeanProportion))
Anova_State_correct$Subject <- as.factor(Anova_State_correct$Subject)

state_prime_aov_correct <- with(Anova_State_correct, aov(MeanProportion ~ PrimeCondition*State + Error(Subject/(PrimeCondition*State) )))
summary(state_prime_aov_correct)


## for incorrect trials

retrieval_incorrect <- read.csv("aggremovedlowtargets2.csv")

mean_r_know_incorrect <- mean(retrieval_incorrect$value.r_know_incorrect, na.rm = TRUE)
mean_r_dontknow_incorrect <- mean(retrieval_incorrect$value.r_dontknow_incorrect, na.rm = TRUE)
mean_r_other_incorrect <- mean(retrieval_incorrect$value.r_other_incorrect, na.rm = TRUE)
mean_r_TOT_incorrect <- mean(retrieval_incorrect$value.r_TOT_incorrect, na.rm = TRUE)

mean_p_know_incorrect <- mean(retrieval_incorrect$value.p_know_incorrect, na.rm = TRUE)
mean_p_dontknow_incorrect <- mean(retrieval_incorrect$value.p_dontknow_incorrect, na.rm = TRUE)
mean_p_other_incorrect <- mean(retrieval_incorrect$value.p_other_incorrect, na.rm = TRUE)
mean_p_TOT_incorrect <- mean(retrieval_incorrect$value.p_TOT_incorrect, na.rm = TRUE)

mean_b_know_incorrect <- mean(retrieval_incorrect$value.b_know_incorrect, na.rm = TRUE)
mean_b_dontknow_incorrect <- mean(retrieval_incorrect$value.b_dontknow_incorrect, na.rm = TRUE)
mean_b_other_incorrect <- mean(retrieval_incorrect$value.b_other_incorrect, na.rm = TRUE)
mean_b_TOT_incorrect <- mean(retrieval_incorrect$value.b_TOT_incorrect, na.rm = TRUE)

mean_u_know_incorrect <- mean(retrieval_incorrect$value.u_know_incorrect, na.rm = TRUE)
mean_u_dontknow_incorrect <- mean(retrieval_incorrect$value.u_dontknow_incorrect, na.rm = TRUE)
mean_u_other_incorrect <- mean(retrieval_incorrect$value.u_other_incorrect, na.rm = TRUE)
mean_u_TOT_incorrect <- mean(retrieval_incorrect$value.u_TOT_incorrect, na.rm = TRUE)

primebystate_incorrect <- matrix(NA, nrow = 16, ncol = 6)
colnames(primebystate_incorrect) = c("PrimeCondition", "State", "MeanProportion", "Error",
                                   "CI_lower", "CI_upper")

primebystate_incorrect[1:16, 1] = c("R", "R", "R", "R",
                                  "P", "P", "P", "P",
                                  "B", "B", "B", "B",
                                  "U", "U", "U", "U")
primebystate_incorrect[1:16, 2] = c("Know", "Dont Know", "Other", "TOT",
                                  "Know", "Dont Know", "Other", "TOT",
                                  "Know", "Dont Know", "Other", "TOT",
                                  "Know", "Dont Know", "Other", "TOT")

primebystate_incorrect[1:16, 3] = c(mean_r_know_incorrect, mean_r_dontknow_incorrect, 
                                  mean_r_other_incorrect, mean_r_TOT_incorrect,
                                  mean_p_know_incorrect, mean_p_dontknow_incorrect, 
                                  mean_p_other_incorrect, mean_p_TOT_incorrect,
                                  mean_b_know_incorrect, mean_b_dontknow_incorrect, 
                                  mean_b_other_incorrect, mean_b_TOT_incorrect,
                                  mean_u_know_incorrect, mean_u_dontknow_incorrect, 
                                  mean_u_other_incorrect, mean_u_TOT_incorrect)

primebystate_incorrect[1:16, 4] = c(qnorm(0.975)*(sd(retrieval_incorrect$value.r_know_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.r_dontknow_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.r_other_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.r_TOT_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.p_know_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.p_dontknow_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.p_other_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.p_TOT_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.b_know_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.b_dontknow_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.b_other_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.b_TOT_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.u_know_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.u_dontknow_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.u_other_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect)),
                                  qnorm(0.975)*(sd(retrieval_incorrect$value.u_TOT_incorrect, na.rm = TRUE)) /sqrt(nrow(retrieval_incorrect))
                              )


primebystate_incorrect <- as.data.frame(primebystate_incorrect)
primebystate_incorrect$MeanProportion <- as.numeric(as.character(primebystate_incorrect$MeanProportion))
primebystate_incorrect$Error <- as.numeric(as.character(primebystate_incorrect$Error))

primebystate_incorrect$CI_lower = primebystate_incorrect$MeanProportion - primebystate_incorrect$Error
primebystate_incorrect$CI_upper = primebystate_incorrect$MeanProportion + primebystate_incorrect$Error

ggplot(primebystate_incorrect,aes(x = PrimeCondition ,y = MeanProportion, fill = as.factor(State))) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "chartreuse4", "#999999"), 
                    name = "State")+
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, fill = State), 
                width=.1, color = "gray26", 
                position = position_dodge(0.7))+
  theme_minimal()+
  xlab("Retrieval State") + ylab("Mean Proportion of Responses") + 
  ggtitle("Wrong Trials: Mean Proportion \n of States by Prime Type")

#for anova, we need one row per participant

NforStateprime = N*16

Anova_State_incorrect <- matrix(NA, nrow = NforStateprime, ncol = 4)
colnames(Anova_State_incorrect) = c("Subject", "PrimeCondition", "State", "MeanProportion")
state <- c("1_Know", "2_DontKnow", "3_Other", "4_TOT")
prime <- c("R", "P", "B", "U")
j =1

for(i in 1:N){
  retrievalcounter = 1
  for(l in 1:4){
    
    for(k in 1:4){
      
      Anova_State_incorrect[j,1] = retrieval_incorrect[i,2]
      Anova_State_incorrect[j,2] = prime[l]
      Anova_State_incorrect[j,3] = state[k]
      index = 91 + retrievalcounter
      Anova_State_incorrect[j,4] = retrieval_incorrect[i, index]
      
      j = j+1
      retrievalcounter = retrievalcounter+1
    }
  }
}


#now we have one row per participant, per prime and state 
Anova_State_incorrect <- as.data.frame(Anova_State_incorrect)
Anova_State_incorrect$MeanProportion <- as.numeric(as.character(Anova_State_incorrect$MeanProportion))
Anova_State_incorrect$Subject <- as.factor(Anova_State_incorrect$Subject)

state_prime_aov_incorrect <- with(Anova_State_incorrect, aov(MeanProportion ~ PrimeCondition*State + Error(Subject/(PrimeCondition*State) )))
summary(state_prime_aov_incorrect)



#anova for accuracy for prime retrieved or not
  
  Nforretrieved = N*2
  pr <- matrix(NA, nrow = Nforretrieved, ncol = 3)
  colnames(pr) = c("Subject", "Retrieved", "MeanAcc")
  
  PrimeRetrievedLabels = c("Retrieved", "Not Retrieved")
  
  j =1
  for(i in 1:N){
    
    for(k in 1:2){
      
      pr[j,1] = y[i,2]
      pr[j,2] = PrimeRetrievedLabels[k]
      pr[j,3] = y[i,41+k]
      
      j = j+1
    }
    
  }
  
  pr <- as.data.frame(pr)
  pr$MeanAcc = as.numeric(as.character(pr$MeanAcc))
  
  prime_retrieved_aov <- with(pr, aov(MeanAcc ~ Retrieved + Error(Subject / Retrieved)))
  
  #this is significant i.e. more accurate when prime is retrieved at first attempt
  summary(prime_retrieved_aov)
  
  #plotting acc wrt whether prime was retrieved or not
  
  p_retrieved <- subset(pr, pr$Retrieved == "Retrieved")
  p_notretrieved <- subset(pr, pr$Retrieved == "Not Retrieved")
  
  
  p_r <- mean(p_retrieved$MeanAcc)
  p_notr <- mean(p_notretrieved$MeanAcc)
  
  pr_accdata <- matrix(NA, nrow = 2, ncol = 5)
  colnames(pr_accdata) = c("PrimeRetrieved", "MeanAcc", "Error", "CI_lower", "CI_upper")
  pr_accdata[,1] = PrimeRetrievedLabels
  pr_accdata[,2] = c(p_r, p_notr)
  
  
  pr_accdata <- as.data.frame(pr_accdata)
  pr_accdata$MeanAcc <- as.numeric(as.character(pr_accdata$MeanAcc))
  
  pr_accdata$Error = c(qnorm(0.975)*(sd(p_retrieved$MeanAcc))/sqrt(nrow(p_retrieved)),
                    qnorm(0.975)*(sd(p_notretrieved$MeanAcc))/sqrt(nrow(p_notretrieved)))
  pr_accdata$CI_lower = pr_accdata$MeanAcc - pr_accdata$Error
  pr_accdata$CI_upper = pr_accdata$MeanAcc + pr_accdata$Error
  
  library(ggplot2)
  library(ggthemes)
  
  ggplot(pr_accdata,aes(x = PrimeRetrieved ,y = MeanAcc, fill = PrimeRetrieved)) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.5)+
    geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                 width=.1,  position=position_dodge(width=.6))+
    ylim(0,1)+
    theme_gdocs()+
    xlab("Retrieval State") + ylab("Mean Acc") + 
    ggtitle("Mean Accuracy by \n Prime Retrieved or Not") 
  
  #accuracy as a function of prime retrieved as a function of prime condition
  
  Nforretanovastate <- N*8
  Anova_retrievedprime <- matrix(NA, nrow = Nforretanovastate, ncol = 4)
  colnames(Anova_retrievedprime) = c("Subject", "PrimeRetrieved","PrimeCondition", "MeanProportion")
  retrieved <- c("Retrieved", "Not Retrieved")
  prime <- c("R", "P", "B", "U")
  j =1
  
  for(i in 1:N){
    counterfory = 1
    for(l in 1:2){
      
      for(k in 1:4){
        
        Anova_retrievedprime[j,1] = y[i,2]
        Anova_retrievedprime[j,2] = retrieved[l]
        Anova_retrievedprime[j,3] = prime[k]
        index = 51 + counterfory
        Anova_retrievedprime[j,4] = y[i, index]
        
        j = j+1
        counterfory = counterfory + 1
      }
    }
  }
  
  
  #now we have one row per participant, per prime and state 
  Anova_retrievedprime <- as.data.frame(Anova_retrievedprime)
  Anova_retrievedprime$MeanProportion <- as.numeric(as.character(Anova_retrievedprime$MeanProportion))
  
  Anova_retrievedprime[c(42,46, 297, 301), 4] = c(0,0)
  #NOTE: S6 AND S41 had 0 retrieved in one prime condition, hence 
  #those proportions are NA, replaced by 0 above. 
  
  retrieved_prime_aov <- with(Anova_retrievedprime, 
                              aov(MeanProportion ~ PrimeCondition*PrimeRetrieved + 
                                    Error(Subject / (PrimeCondition*PrimeRetrieved) )))
  summary(retrieved_prime_aov)
  
  #plotting in a graph
  
  retrievedprimegraph <- matrix(NA, nrow = 8, ncol = 6)
  colnames(retrievedprimegraph) = c("PrimeCondition", "PrimeRetrieved", 
                                    "MeanProportion", "Error", "CI_lower", "CI_upper" )

  r <- subset(Anova_retrievedprime, Anova_retrievedprime$PrimeCondition == "R" )
  p <- subset(Anova_retrievedprime, Anova_retrievedprime$PrimeCondition == "P" )
  b <- subset(Anova_retrievedprime, Anova_retrievedprime$PrimeCondition == "B" )
  u <- subset(Anova_retrievedprime, Anova_retrievedprime$PrimeCondition == "U" )
  
  r_r <- subset(r, r$PrimeRetrieved == "Retrieved")
  r_nr <- subset(r, r$PrimeRetrieved == "Not Retrieved")
  
  p_r <- subset(p, p$PrimeRetrieved == "Retrieved")
  p_nr <- subset(p, p$PrimeRetrieved == "Not Retrieved")
  
  b_r <- subset(b, b$PrimeRetrieved == "Retrieved")
  b_nr <- subset(b, b$PrimeRetrieved == "Not Retrieved")
  
  u_r <- subset(u, u$PrimeRetrieved == "Retrieved")
  u_nr <- subset(u, u$PrimeRetrieved == "Not Retrieved")
  
  retrievedprimegraph[1:8, 1] = c("R", "R", "P", "P", "B", "B", "U", "U")
  retrievedprimegraph[1:8, 2] = c("Retrieved", "Not Retrieved", "Retrieved", "Not Retrieved",
                                  "Retrieved", "Not Retrieved", "Retrieved", "Not Retrieved")
  retrievedprimegraph[1:8, 3] = c(mean(r_r$MeanProportion), mean(r_nr$MeanProportion),
                                  mean(p_r$MeanProportion), mean(p_nr$MeanProportion),
                                  mean(b_r$MeanProportion), mean(b_nr$MeanProportion),
                                  mean(u_r$MeanProportion), mean(u_nr$MeanProportion))
  
  retrievedprimegraph[1:8, 4] = c(qnorm(0.975)*(sd(r_r$MeanProportion)) /sqrt(nrow(r_r)),
                                qnorm(0.975)*(sd(r_nr$MeanProportion))/sqrt(nrow(r_nr)),
                                qnorm(0.975)*(sd(p_r$MeanProportion)) /sqrt(nrow(p_r)),
                                qnorm(0.975)*(sd(p_nr$MeanProportion))/sqrt(nrow(p_nr)),
                                qnorm(0.975)*(sd(b_r$MeanProportion)) /sqrt(nrow(b_r)),
                                qnorm(0.975)*(sd(b_nr$MeanProportion))/sqrt(nrow(b_nr)),
                                qnorm(0.975)*(sd(u_r$MeanProportion)) /sqrt(nrow(u_r)),
                                qnorm(0.975)*(sd(u_nr$MeanProportion))/sqrt(nrow(u_nr)))
  
  retrievedprimegraph <- as.data.frame(retrievedprimegraph)
  colnames(retrievedprimegraph) = c("PrimeCondition", "PrimeRetrieved", 
                                    "MeanProportion", "Error", "CI_lower", "CI_upper" )
  retrievedprimegraph$MeanProportion <- as.numeric(as.character(retrievedprimegraph$MeanProportion))
  retrievedprimegraph$Error <- as.numeric(as.character(retrievedprimegraph$Error))
  
    
  retrievedprimegraph$CI_lower = retrievedprimegraph$MeanProportion - retrievedprimegraph$Error
  retrievedprimegraph$CI_upper = retrievedprimegraph$MeanProportion + retrievedprimegraph$Error
  

  ggplot(retrievedprimegraph,aes(x = PrimeCondition ,y = MeanProportion, fill = as.factor(PrimeRetrieved))) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values=c("#E69F00", "#56B4E9"), 
                      name = "Prime Retrieved")+
    geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, fill = PrimeRetrieved), 
                  width=.1, color = "gray26", 
                  position = position_dodge(0.6))+
        theme_minimal()+
    xlab("Prime Type") + ylab("Mean Proportion of Correct Trials") + 
    ggtitle("Mean Accuracy w.r.t. Prime Type \nand Prime Retrieved/Not")
  

  #PRIME RETRIEVED/NOT BY STATE
  
  mean_ret_know <- mean(retrieval$value.pretknow)
  mean_ret_dontknow <- mean(retrieval$value.pretdontknow)
  mean_ret_other <- mean(retrieval$value.pretother)
  mean_ret_TOT <- mean(retrieval$value.pretTOT)
  
  mean_notret_know <- mean(retrieval$value.pnotretknow)
  mean_notret_dontknow <- mean(retrieval$value.pnotretdontknow)
  mean_notret_other <- mean(retrieval$value.pnotretother)
  mean_notret_TOT <- mean(retrieval$value.pnotretTOT)
  
  Nforretrieved = N*8
  rs <- matrix(NA, nrow = Nforretrieved, ncol = 4)
  colnames(rs) = c("Subject", "Retrieved", "State", "Proportion")
  
  PrimeRetrievedLabels = c("Retrieved", "Not Retrieved")
  StateLabels = c("Know", "Dont Know", "Other", "TOT")
  
  j =1
  
  for(i in 1:N){
    counterforret = 1
    for(l in 1:2){
      
      for(k in 1:4){
        
        rs[j,1] = retrieval[i,2]
        rs[j,2] = PrimeRetrievedLabels[l]
        rs[j,3] = StateLabels[k]
        index = 117 + counterforret
        rs[j,4] = retrieval[i, index]
        
        j = j+1
        counterforret = counterforret + 1
      }
    }
  }
  
  
  rs <- as.data.frame(rs)
  rs$Proportion = as.numeric(as.character(rs$Proportion))
  
  prime_retrieved_state_aov <- with(rs, aov(Proportion ~ Retrieved*State + 
                                              Error(Subject / (Retrieved*State))))
  
  summary(prime_retrieved_state_aov)
  
  #plotting proportions per state wrt whether prime was retrieved or not
  
  ret <- subset(rs, rs$Retrieved == "Retrieved")
  notret <- subset(rs, rs$Retrieved == "Not Retrieved")
  
  ret_know = subset(ret, ret$State == "Know")
  ret_dontknow = subset(ret, ret$State == "Dont Know")
  ret_other = subset(ret, ret$State == "Other")
  ret_TOT = subset(ret, ret$State == "TOT")
  
  notret_know = subset(notret, notret$State == "Know")
  notret_dontknow = subset(notret, notret$State == "Dont Know")
  notret_other = subset(notret, notret$State == "Other")
  notret_TOT = subset(notret, notret$State == "TOT")
  
  rs_plotdata <- matrix(NA, nrow = 8, ncol = 6)
  colnames(rs_plotdata) = c("PrimeRetrieved", "State", "Proportion", 
                            "Error", "CI_lower", "CI_upper")
  
  rs_plotdata[1:8, 1] = c("Retrieved", "Retrieved", "Retrieved", "Retrieved",
                                  "Not Retrieved", "Not Retrieved", "Not Retrieved", "Not Retrieved")
  rs_plotdata[1:8, 2] = c("Know", "Dont Know", "Other", "TOT",
                                  "Know", "Dont Know", "Other", "TOT")
  
  rs_plotdata[1:8, 3] = c(mean(ret_know$Proportion), mean(ret_dontknow$Proportion),
                                  mean(ret_other$Proportion), mean(ret_TOT$Proportion),
                                  mean(notret_know$Proportion), mean(notret_dontknow$Proportion),
                                  mean(notret_other$Proportion), mean(notret_TOT$Proportion)
                                  )
  
  rs_plotdata[1:8, 4] = c(qnorm(0.975)*(sd(ret_know$Proportion)) /sqrt(nrow(ret_know)),
                                  qnorm(0.975)*(sd(ret_dontknow$Proportion))/sqrt(nrow(ret_dontknow)),
                                  qnorm(0.975)*(sd(ret_other$Proportion)) /sqrt(nrow(ret_other)),
                                  qnorm(0.975)*(sd(ret_TOT$Proportion))/sqrt(nrow(ret_TOT)),
                          qnorm(0.975)*(sd(notret_know$Proportion)) /sqrt(nrow(notret_know)),
                          qnorm(0.975)*(sd(notret_dontknow$Proportion))/sqrt(nrow(notret_dontknow)),
                          qnorm(0.975)*(sd(notret_other$Proportion)) /sqrt(nrow(notret_other)),
                          qnorm(0.975)*(sd(notret_TOT$Proportion))/sqrt(nrow(notret_TOT))       
                          )
  
  rs_plotdata <- as.data.frame(rs_plotdata)
  colnames(rs_plotdata) = c( "PrimeRetrieved", "State", 
                                    "Proportion", "Error", "CI_lower", "CI_upper" )
  rs_plotdata$Proportion <- as.numeric(as.character(rs_plotdata$Proportion))
  rs_plotdata$Error <- as.numeric(as.character(rs_plotdata$Error))
  
  
  rs_plotdata$CI_lower = rs_plotdata$Proportion - rs_plotdata$Error
  rs_plotdata$CI_upper = rs_plotdata$Proportion + rs_plotdata$Error
  
  
  ggplot(rs_plotdata,aes(x = PrimeRetrieved ,y = Proportion, fill = as.factor(State))) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values=c("#E69F00", "#56B4E9", "chartreuse4", "#999999"), 
                      name = "State")+
        geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, fill = State), 
                  width=.1, color = "gray26", 
                  position = position_dodge(0.6))+
    
    theme_minimal()+
    xlab("Prime Retrieved/Not") + ylab("Proportion of Trials") + 
    ggtitle("Proportion of States \n w.r.t. Prime Retrieved/Not")
  
  ######## STATE WRT PRIME RETRIEVED/NOT WRT PRIME TYPE #########
  
  threeway <- read.csv("agg45_withretstateprime.csv")
  
  #our data starts column 126 onwards
  
  Nforretrieved = N*32
  rps <- matrix(NA, nrow = Nforretrieved, ncol = 5)
  colnames(rps) = c("Subject", "Retrieved", "PrimeCondition", "State", "Proportion")
  
  PrimeRetrievedLabels = c("Retrieved", "Not Retrieved")
  PrimeConditionLabels = c("Related", "Phonological", "Both", "Unrelated")
  StateLabels = c("Know", "Dont Know", "Other", "TOT")
  
  j =1
  
  for(i in 1:N){
    counterforrps = 1

        for(r in 1:2){
      
      for(p in 1:4){
        
        for(s in 1:4){
        
        rps[j,1] = threeway[i,2]
        rps[j,2] = PrimeRetrievedLabels[r]
        rps[j,3] = PrimeConditionLabels[p]
        rps[j,4] = StateLabels[s]
        
        index = 125 + counterforrps
        rps[j,5] = threeway[i, index]
        
        j = j+1
        counterforrps = counterforrps + 1
        }
      }
    }
  }
  
  
  rps <- as.data.frame(rps)
  
  rps$Proportion = as.numeric(as.character(rps$Proportion))
  
  threeway_aov <- with(rps, aov(Proportion ~ Retrieved*PrimeCondition*State + 
                                              Error(Subject / (Retrieved*PrimeCondition*State))))
  
  summary(threeway_aov)
  

  #plotting proportions per prime condition per state wrt whether prime was retrieved or not
  
  #first for retrieved
  
  ret <- subset(rps, rps$Retrieved == "Retrieved")
  notret <- subset(rps, rps$Retrieved == "Not Retrieved")
  
  r_ret = subset(ret, ret$PrimeCondition == "Related")
  p_ret = subset(ret, ret$PrimeCondition == "Phonological")
  b_ret = subset(ret, ret$PrimeCondition == "Both")
  u_ret = subset(ret, ret$PrimeCondition == "Unrelated")
  
  r_ret_know = subset(r_ret, r_ret$State == "Know")
  r_ret_dontknow = subset(r_ret, r_ret$State == "Dont Know")
  r_ret_other = subset(r_ret, r_ret$State == "Other")
  r_ret_TOT = subset(r_ret, r_ret$State == "TOT")
  
  p_ret_know = subset(p_ret, p_ret$State == "Know")
  p_ret_dontknow = subset(p_ret, p_ret$State == "Dont Know")
  p_ret_other = subset(p_ret, p_ret$State == "Other")
  p_ret_TOT = subset(p_ret, p_ret$State == "TOT")
  
  b_ret_know = subset(b_ret, b_ret$State == "Know")
  b_ret_dontknow = subset(b_ret, b_ret$State == "Dont Know")
  b_ret_other = subset(b_ret, b_ret$State == "Other")
  b_ret_TOT = subset(b_ret, b_ret$State == "TOT")
  
  u_ret_know = subset(u_ret, u_ret$State == "Know")
  u_ret_dontknow = subset(u_ret, u_ret$State == "Dont Know")
  u_ret_other = subset(u_ret, u_ret$State == "Other")
  u_ret_TOT = subset(u_ret, u_ret$State == "TOT")
  
  #similarly for notret
  
  r_notret = subset(notret, notret$PrimeCondition == "Related")
  p_notret = subset(notret, notret$PrimeCondition == "Phonological")
  b_notret = subset(notret, notret$PrimeCondition == "Both")
  u_notret = subset(notret, notret$PrimeCondition == "Unrelated")
  
  r_notret_know = subset(r_notret, r_notret$State == "Know")
  r_notret_dontknow = subset(r_notret, r_notret$State == "Dont Know")
  r_notret_other = subset(r_notret, r_notret$State == "Other")
  r_notret_TOT = subset(r_notret, r_notret$State == "TOT")
  
  p_notret_know = subset(p_notret, p_notret$State == "Know")
  p_notret_dontknow = subset(p_notret, p_notret$State == "Dont Know")
  p_notret_other = subset(p_notret, p_notret$State == "Other")
  p_notret_TOT = subset(p_notret, p_notret$State == "TOT")
  
  b_notret_know = subset(b_notret, b_notret$State == "Know")
  b_notret_dontknow = subset(b_notret, b_notret$State == "Dont Know")
  b_notret_other = subset(b_notret, b_notret$State == "Other")
  b_notret_TOT = subset(b_notret, b_notret$State == "TOT")
  
  u_notret_know = subset(u_notret, u_notret$State == "Know")
  u_notret_dontknow = subset(u_notret, u_notret$State == "Dont Know")
  u_notret_other = subset(u_notret, u_notret$State == "Other")
  u_notret_TOT = subset(u_notret, u_notret$State == "TOT")
  
  
  
  rps_plotdata_ret <- matrix(NA, nrow = 16, ncol = 7)
  colnames(rps_plotdata_ret) = c("PrimeRetrieved", "PrimeCondition", "State", "Proportion", 
                            "Error", "CI_lower", "CI_upper")
  
  rps_plotdata_ret[1:16, 1] = c("Retrieved", "Retrieved", "Retrieved", "Retrieved",
                           "Retrieved", "Retrieved", "Retrieved", "Retrieved",
                           "Retrieved", "Retrieved", "Retrieved", "Retrieved",
                           "Retrieved", "Retrieved", "Retrieved", "Retrieved"
                           )
  rps_plotdata_ret[1:16, 2] = c("Related", "Related", "Related", "Related",
                                "Phonological", "Phonological", "Phonological","Phonological",
                                "Both", "Both", "Both", "Both",
                                "Unrelated", "Unrelated", "Unrelated", "Unrelated")
  rps_plotdata_ret[1:16, 3] = c("Know", "Dont Know", "Other", "TOT",
                          "Know", "Dont Know", "Other", "TOT",
                          "Know", "Dont Know", "Other", "TOT",
                          "Know", "Dont Know", "Other", "TOT")
  
  rps_plotdata_ret[1:16, 4] = c( mean(r_ret_know$Proportion), mean(r_ret_dontknow$Proportion),
                                 mean(r_ret_other$Proportion), mean(r_ret_TOT$Proportion),
                                 mean(p_ret_know$Proportion), mean(p_ret_dontknow$Proportion),
                                 mean(p_ret_other$Proportion), mean(p_ret_TOT$Proportion),
                                 mean(b_ret_know$Proportion), mean(b_ret_dontknow$Proportion),
                                 mean(b_ret_other$Proportion), mean(b_ret_TOT$Proportion),
                                 mean(u_ret_know$Proportion), mean(u_ret_dontknow$Proportion),
                                 mean(u_ret_other$Proportion), mean(u_ret_TOT$Proportion)
                                )
  
  rps_plotdata_ret[1:16, 5] = c(qnorm(0.975)*(sd(r_ret_know$Proportion)) /sqrt(nrow(r_ret_know)),
                          qnorm(0.975)*(sd(r_ret_dontknow$Proportion))/sqrt(nrow(r_ret_dontknow)),
                          qnorm(0.975)*(sd(r_ret_other$Proportion)) /sqrt(nrow(r_ret_other)),
                          qnorm(0.975)*(sd(r_ret_TOT$Proportion))/sqrt(nrow(r_ret_TOT)),
                          qnorm(0.975)*(sd(p_ret_know$Proportion)) /sqrt(nrow(p_ret_know)),
                          qnorm(0.975)*(sd(p_ret_dontknow$Proportion))/sqrt(nrow(p_ret_dontknow)),
                          qnorm(0.975)*(sd(p_ret_other$Proportion)) /sqrt(nrow(p_ret_other)),
                          qnorm(0.975)*(sd(p_ret_TOT$Proportion))/sqrt(nrow(p_ret_TOT)),
                          qnorm(0.975)*(sd(b_ret_know$Proportion)) /sqrt(nrow(b_ret_know)),
                          qnorm(0.975)*(sd(b_ret_dontknow$Proportion))/sqrt(nrow(b_ret_dontknow)),
                          qnorm(0.975)*(sd(b_ret_other$Proportion)) /sqrt(nrow(b_ret_other)),
                          qnorm(0.975)*(sd(b_ret_TOT$Proportion))/sqrt(nrow(b_ret_TOT)),
                          qnorm(0.975)*(sd(u_ret_know$Proportion)) /sqrt(nrow(u_ret_know)),
                          qnorm(0.975)*(sd(u_ret_dontknow$Proportion))/sqrt(nrow(u_ret_dontknow)),
                          qnorm(0.975)*(sd(u_ret_other$Proportion)) /sqrt(nrow(u_ret_other)),
                          qnorm(0.975)*(sd(u_ret_TOT$Proportion))/sqrt(nrow(u_ret_TOT))
                               
                          )
  
  rps_plotdata_ret <- as.data.frame(rps_plotdata_ret)
  colnames(rps_plotdata_ret) = c( "PrimeRetrieved", "PrimeCondition", "State", 
                             "Proportion", "Error", "CI_lower", "CI_upper" )
  rps_plotdata_ret$Proportion <- as.numeric(as.character(rps_plotdata_ret$Proportion))
  rps_plotdata_ret$Error <- as.numeric(as.character(rps_plotdata_ret$Error))
  
  
  rps_plotdata_ret$CI_lower = rps_plotdata_ret$Proportion - rps_plotdata_ret$Error
  rps_plotdata_ret$CI_upper = rps_plotdata_ret$Proportion + rps_plotdata_ret$Error
  
  
 retplot= ggplot(rps_plotdata_ret,aes(x = PrimeCondition ,y = Proportion, fill = as.factor(State))) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values=c("#E69F00", "#56B4E9", "chartreuse4", "#999999"), 
                      name = "State")+
    geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, fill = State), 
                  width=.1, color = "gray26", 
                  position = position_dodge(0.6))+
   ylim(0,1) +
    theme_minimal()+
    xlab("Prime Condition") + ylab("Proportion of Trials") + 
    ggtitle("Prime Retrieved: States \n w.r.t. Prime Condition")
  
  #similarly for not retrieved: then we can compare plots to see differences
  
  rps_plotdata_notret <- matrix(NA, nrow = 16, ncol = 7)
  colnames(rps_plotdata_notret) = c("PrimeRetrieved", "PrimeCondition", "State", "Proportion", 
                                 "Error", "CI_lower", "CI_upper")
  
  rps_plotdata_notret[1:16, 1] = c("Not Retrieved", "Not Retrieved","Not Retrieved","Not Retrieved",
                                   "Not Retrieved","Not Retrieved","Not Retrieved","Not Retrieved",
                                   "Not Retrieved","Not Retrieved","Not Retrieved","Not Retrieved",
                                   "Not Retrieved","Not Retrieved","Not Retrieved","Not Retrieved"
                                   )
  rps_plotdata_notret[1:16, 2] = c("Related", "Related", "Related", "Related",
                                "Phonological", "Phonological", "Phonological","Phonological",
                                "Both", "Both", "Both", "Both",
                                "Unrelated", "Unrelated", "Unrelated", "Unrelated")
  rps_plotdata_notret[1:16, 3] = c("Know", "Dont Know", "Other", "TOT",
                                "Know", "Dont Know", "Other", "TOT",
                                "Know", "Dont Know", "Other", "TOT",
                                "Know", "Dont Know", "Other", "TOT")
  
  rps_plotdata_notret[1:16, 4] = c( mean(r_notret_know$Proportion), mean(r_notret_dontknow$Proportion),
                                 mean(r_notret_other$Proportion), mean(r_notret_TOT$Proportion),
                                 mean(p_notret_know$Proportion), mean(p_notret_dontknow$Proportion),
                                 mean(p_notret_other$Proportion), mean(p_notret_TOT$Proportion),
                                 mean(b_notret_know$Proportion), mean(b_notret_dontknow$Proportion),
                                 mean(b_notret_other$Proportion), mean(b_notret_TOT$Proportion),
                                 mean(u_notret_know$Proportion), mean(u_notret_dontknow$Proportion),
                                 mean(u_notret_other$Proportion), mean(u_notret_TOT$Proportion)
  )
  
  rps_plotdata_notret[1:16, 5] = c(qnorm(0.975)*(sd(r_notret_know$Proportion)) /sqrt(nrow(r_notret_know)),
                                qnorm(0.975)*(sd(r_notret_dontknow$Proportion))/sqrt(nrow(r_notret_dontknow)),
                                qnorm(0.975)*(sd(r_notret_other$Proportion)) /sqrt(nrow(r_notret_other)),
                                qnorm(0.975)*(sd(r_notret_TOT$Proportion))/sqrt(nrow(r_notret_TOT)),
                                qnorm(0.975)*(sd(p_notret_know$Proportion)) /sqrt(nrow(p_notret_know)),
                                qnorm(0.975)*(sd(p_notret_dontknow$Proportion))/sqrt(nrow(p_notret_dontknow)),
                                qnorm(0.975)*(sd(p_notret_other$Proportion)) /sqrt(nrow(p_notret_other)),
                                qnorm(0.975)*(sd(p_notret_TOT$Proportion))/sqrt(nrow(p_notret_TOT)),
                                qnorm(0.975)*(sd(b_notret_know$Proportion)) /sqrt(nrow(b_notret_know)),
                                qnorm(0.975)*(sd(b_notret_dontknow$Proportion))/sqrt(nrow(b_notret_dontknow)),
                                qnorm(0.975)*(sd(b_notret_other$Proportion)) /sqrt(nrow(b_notret_other)),
                                qnorm(0.975)*(sd(b_notret_TOT$Proportion))/sqrt(nrow(b_notret_TOT)),
                                qnorm(0.975)*(sd(u_notret_know$Proportion)) /sqrt(nrow(u_notret_know)),
                                qnorm(0.975)*(sd(u_notret_dontknow$Proportion))/sqrt(nrow(u_notret_dontknow)),
                                qnorm(0.975)*(sd(u_notret_other$Proportion)) /sqrt(nrow(u_notret_other)),
                                qnorm(0.975)*(sd(u_notret_TOT$Proportion))/sqrt(nrow(u_notret_TOT))
                                
  )
  
  rps_plotdata_notret <- as.data.frame(rps_plotdata_notret)
  colnames(rps_plotdata_notret) = c( "PrimeRetrieved", "PrimeCondition", "State", 
                                  "Proportion", "Error", "CI_lower", "CI_upper" )
  rps_plotdata_notret$Proportion <- as.numeric(as.character(rps_plotdata_notret$Proportion))
  rps_plotdata_notret$Error <- as.numeric(as.character(rps_plotdata_notret$Error))
  
  
  rps_plotdata_notret$CI_lower = rps_plotdata_notret$Proportion - rps_plotdata_notret$Error
  rps_plotdata_notret$CI_upper = rps_plotdata_notret$Proportion + rps_plotdata_notret$Error
  
  
 notretplot =  ggplot(rps_plotdata_notret,aes(x = PrimeCondition ,y = Proportion, fill = as.factor(State))) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values=c("#E69F00", "#56B4E9", "chartreuse4", "#999999"), 
                      name = "State")+
    geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper, fill = State), 
                  width=.1, color = "gray26", 
                  position = position_dodge(0.6))+
   ylim(0,1) +
    theme_minimal()+
    xlab("Prime Condition") + ylab("") + 
    ggtitle("Prime NOT Retrieved: States \n w.r.t. Prime Condition")
  
  grid.arrange(retplot, notretplot, ncol = 2, nrow = 1)
  
  
  ## mean RT data (raw) -- filling in missing cells using triangulation
  
  RT_45 <- read.csv("agg45_withstateRT.csv", header = TRUE, sep=",")
  RT_45 <- subset(RT_45, RT_45$value.Subject!= 10) #REMOVING SLOW SUBJECT 10 completely. 
  N=nrow(RT_45)
  
  # we calculate means independent of condition
  mean_overall = mean(RT_45$value.overallstateRT)
  sd_overall = mean(RT_45$value.overallstateRTSD)
  
  #now mean per condition
  
  meanRT_know = mean(RT_45$value.targetQuesRT_know, na.rm = TRUE)
  meanRT_dontknow = mean(RT_45$value.targetQuesRT_dontknow, na.rm = TRUE)
  meanRT_Other = mean(RT_45$value.targetQuesRT_other, na.rm = TRUE)
  meanRT_TOT = mean(RT_45$value.targetQuesRT_TOT, na.rm = TRUE)

  sdRT_know = sd(RT_45$value.targetQuesRT_know, na.rm = TRUE)
  sdRT_dontknow = sd(RT_45$value.targetQuesRT_dontknow, na.rm = TRUE)
  sdRT_Other = sd(RT_45$value.targetQuesRT_other, na.rm = TRUE)
  sdRT_TOT = sd(RT_45$value.targetQuesRT_TOT, na.rm = TRUE)
  
  #at this point, we substitute missing RTs 
  #by mean for this condition + (diff b.w. overall state RT and overall subject RT
  # -- to account for whether this is a fast or slow subject)
  
  for(i in 1:N){
    
    if(is.na(RT_45[i,158])){
      RT_45[i,158] = meanRT_know +  RT_45[i,162]-mean_overall
    }
    
    if(is.na(RT_45[i,159])){
      RT_45[i,159] = meanRT_dontknow + RT_45[i,162]-mean_overall
    }
    
    if(is.na(RT_45[i,160])){
      RT_45[i,160] = meanRT_Other +  RT_45[i,162]-mean_overall
    }
    
    if(is.na(RT_45[i,161])){
      RT_45[i,161] = meanRT_TOT +  RT_45[i,162]-mean_overall
    }
    
  }
  
  #now each cell of each condition has a value. we do ANOVA on this. 
  
  subject_state_RT = matrix(NA, nrow = N*4, ncol = 4)
  subjects <- c(1:9, 11:45)
  StateLabels <- c("Know", "Dont Know", "Other", "TOT")
  
  j = 1
  for (i in 1:N){
    
    for(s in 1:4){
    
      
      subject_state_RT[j,1] = subjects[i]
      subject_state_RT[j,2] = StateLabels[s]
      subject_state_RT[j,3] = RT_45[i, 35+s]
      subject_state_RT[j,4] = RT_45[i,157+s]
      
      j = j+1
    }
  }
  
  subject_state_RT <- as.data.frame(subject_state_RT)
  colnames(subject_state_RT) = c("Subject", "State", "Proportion", "RT")
  
  subject_state_RT$Proportion <- as.numeric(as.character(subject_state_RT$Proportion))
  subject_state_RT$RT <- as.numeric(as.character(subject_state_RT$RT))
  
  sub_stateRT_anova <- with(subject_state_RT, aov(RT ~ State + Error(Subject/(State))))
  summary(sub_stateRT_anova)
  
  ## mean RT data for retrieving target (raw) -- filling in missing cells using triangulation
  
  RT_45 <- read.csv("agg45rawRTagg.csv", header = TRUE, sep=",")
  RT_45 <- subset(RT_45, RT_45$value.Subject!= 10) #REMOVING SLOW SUBJECT 10 completely. 
  N=nrow(RT_45)
  
  # we calculate means independent of condition
  mean_overall = mean(RT_45$value.overalltargetdefRT)
  sd_overall = mean(RT_45$value.overalltargetdefSD)
  
  #now mean per condition
  
  meanRT_retRT = mean(RT_45$value.retrieved_RT, na.rm = TRUE)
  meanRT_notretRT = mean(RT_45$value.notretrievedRT, na.rm = TRUE)
  
  #at this point, we substitute missing RTs 
  #by mean for this condition + (diff b.w. overall state RT and overall subject RT
  # -- to account for whether this is a fast or slow subject)
  
  for(i in 1:N){
    
    if(is.na(RT_45[i,164])){
      RT_45[i,164] = meanRT_retRT +  RT_45[i,174]-mean_overall
    }
    
    if(is.na(RT_45[i,165])){
      RT_45[i,165] = meanRT_notretRT + RT_45[i,174]-mean_overall
    }
    
 }
  
  #now each cell of each condition has a value. we do ANOVA on this. 
  
  ret_RT = matrix(NA, nrow = N*2, ncol = 3)
  subjects <- c(1:9, 11:45)
  RetrievedLabels <- c("Retrieved", "Not Retrieved")
  
  j = 1
  for (i in 1:N){
    
    for(s in 1:2){
      
      
      ret_RT[j,1] = subjects[i]
      ret_RT[j,2] = RetrievedLabels[s]
      ret_RT[j,3] = RT_45[i,163+s]
      
      j = j+1
    }
  }
  
  ret_RT <- as.data.frame(ret_RT)
  colnames(ret_RT) = c("Subject", "PrimeRetrieved", "RT")
  
  ret_RT$RT <- as.numeric(as.character(ret_RT$RT))
  
  retRT_anova <- with(ret_RT, aov(RT ~ PrimeRetrieved + Error(Subject/(PrimeRetrieved))))
  summary(retRT_anova) #not significant
  
  # we repeat for different prime types: this has major effects for accuracy data
  
  ## mean RT data for retrieving target (raw) -- filling in missing cells using triangulation
  
  RT_45 <- read.csv("agg45rawRTagg.csv", header = TRUE, sep=",")
  RT_45 <- subset(RT_45, RT_45$value.Subject!= 10) #REMOVING SLOW SUBJECT 10 completely. 
  N=nrow(RT_45)
  
  # we calculate means independent of condition
  mean_overall = mean(RT_45$value.overalltargetdefRT)
  sd_overall = mean(RT_45$value.overalltargetdefSD)
  
  #now mean per condition
  
  meanRT_r_retRT = mean(RT_45$value.r_ret_RT, na.rm = TRUE)
  meanRT_r_notretRT = mean(RT_45$value.r_notret_RT, na.rm = TRUE)
  
  meanRT_p_retRT = mean(RT_45$value.p_ret_RT, na.rm = TRUE)
  meanRT_p_notretRT = mean(RT_45$value.p_notret_RT, na.rm = TRUE)
  
  meanRT_b_retRT = mean(RT_45$value.b_ret_RT, na.rm = TRUE)
  meanRT_b_notretRT = mean(RT_45$value.b_notret_RT, na.rm = TRUE)
  
  meanRT_u_retRT = mean(RT_45$value.u_ret_RT, na.rm = TRUE)
  meanRT_u_notretRT = mean(RT_45$value.u_notret_RT, na.rm = TRUE)
  
  #at this point, we substitute missing RTs 
  #by mean for this condition + (diff b.w. overall state RT and overall subject RT
  # -- to account for whether this is a fast or slow subject)
  
  for(i in 1:N){
    
    if(is.na(RT_45[i,166])){
      RT_45[i,166] = meanRT_r_retRT +  RT_45[i,174]-mean_overall
    }
    
    if(is.na(RT_45[i,167])){
      RT_45[i,167] = meanRT_r_notretRT + RT_45[i,174]-mean_overall
    }
    
    if(is.na(RT_45[i,168])){
      RT_45[i,168] = meanRT_p_retRT +  RT_45[i,174]-mean_overall
    }
    
    if(is.na(RT_45[i,169])){
      RT_45[i,169] = meanRT_p_notretRT + RT_45[i,174]-mean_overall
    }
    
    if(is.na(RT_45[i,170])){
      RT_45[i,170] = meanRT_b_retRT +  RT_45[i,174]-mean_overall
    }
    
    if(is.na(RT_45[i,171])){
      RT_45[i,171] = meanRT_b_notretRT + RT_45[i,174]-mean_overall
    }
    
    if(is.na(RT_45[i,172])){
      RT_45[i,172] = meanRT_u_retRT +  RT_45[i,174]-mean_overall
    }
    
    if(is.na(RT_45[i,173])){
      RT_45[i,173] = meanRT_u_notretRT + RT_45[i,174]-mean_overall
    }
    
  }
  
  #now each cell of each condition has a value. we do ANOVA on this. 
  
  retprime_RT = matrix(NA, nrow = N*8, ncol = 4)
  subjects <- c(1:9, 11:45)
  RetrievedLabels <- c("Retrieved", "Not Retrieved")
  
  j = 1
  for (i in 1:N){
    
    counter = 1
    for(p in 1:4){
    
    for(s in 1:2){
      
      
      retprime_RT[j,1] = subjects[i]
      retprime_RT[j,2] = PrimeConditionLabels[p]
      retprime_RT[j,3] = RetrievedLabels[s]
      index = 165 + counter
      
      retprime_RT[j,4] = RT_45[i,index]
      
      j = j+1
      counter = counter+1
    }
    }
  }
  
  retprime_RT <- as.data.frame(retprime_RT)
  colnames(retprime_RT) = c("Subject", "PrimeCondition", "PrimeRetrieved", "RT")
  
  retprime_RT$RT <- as.numeric(as.character(retprime_RT$RT))
  
  retRTprime_anova <- with(retprime_RT, aov(RT ~ PrimeCondition*PrimeRetrieved + 
                                              Error(Subject/(PrimeCondition*PrimeRetrieved))))
  summary(retRTprime_anova) #not significant
  
  
  
  #let's run a test to see if proportion of states vary
  #as a function of subject: NOTE: this revealed S10 AND WE HAVE REMOVED S10 NOW
  
  subject_state = matrix(NA, nrow = N*4, ncol = 4)
  subjects <- c(1:9, 11:45)
  StateLabels <- c("Know", "Dont Know", "Other", "TOT")
  
  j = 1
  for(s in 1:4){
    
    for (i in 1:N){
    
    subject_state[j,1] = StateLabels[s]
    subject_state[j,2] = subjects[i]
    subject_state[j,3] = RT_45[i, 35+s]
    subject_state[j,4] = RT_45[i,157+s]
    
    j = j+1
    }
  }
    
  subject_state <- as.data.frame(subject_state)
  colnames(subject_state) = c("State", "Subject", "Proportion", "RT")
  
  subject_state$Proportion <- as.numeric(as.character(subject_state$Proportion))
  subject_state$RT <- as.numeric(as.character(subject_state$RT))
  
  subject_state = subset(subject_state, subject_state$Subject!=10)
  
  sub_state_aov <- with(subject_state, aov(Proportion ~ Subject + Error(State/(Subject))))
  summary(sub_state_aov)
  
  
  sub_stateRT_aov <- with(subject_state, aov(RT ~ Subject + Error(State/(Subject))))
  summary(sub_stateRT_aov) #significant
  #not significant without Subject 10
  
  substate_rg <- ref.grid(sub_stateRT_aov)
  
  sub_lsm <- lsmeans(substate_rg, "Subject")

  subeffect = cld(sub_lsm, alpha = 0.05, 
                  adjust = "tukey", details = TRUE)
  kable(subset(subeffect$comparisons, p.value < 0.05))
  
  #this shows Subject 10 is very slow in comparison to others, which is an issue.  
  
  #trying lsmeans
  
  options(contrasts = c('contr.sum', 'contr.poly'))
  
  library(lsmeans)
  library(multcomp)
  library(knitr)
  
  #for whether prime retrieved/not by prime type
  retrievedprime_rg <- ref.grid(retrieved_prime_aov)
  
  retrieved_lsm <- lsmeans(retrievedprime_rg, "PrimeRetrieved")
  prime_lsm <- lsmeans(retrievedprime_rg, "PrimeCondition")
  
  retrieved_x_prime_lsm <- lsmeans(retrievedprime_rg, 
                               c("PrimeRetrieved", "PrimeCondition"))
  
  reteffect = cld(retrieved_lsm, alpha = 0.05, 
                  adjust = "tukey", details = TRUE)
  kable(subset(reteffect$comparisons, 1<2))
  
  primeeffect = cld(prime_lsm, alpha = 0.05, 
                    adjust = "tukey", details = TRUE)
  kable(subset(primeeffect$comparisons, 1<2))
  
  
  interactionbyprime = cld(retrieved_x_prime_lsm, by = "PrimeCondition", alpha = 0.05, 
      adjust = "tukey", details = TRUE)
  kable(subset(interactionbyprime$comparisons, 1<2)[,c(1,2,3,7)])
  
  #state_prime_aov lsmeans
  
  StatePrime_rg <- ref.grid(state_prime_aov)
  
  state_lsm <- lsmeans(StatePrime_rg, "State")
  prime_lsm <- lsmeans(StatePrime_rg, "PrimeCondition")
  
  state_x_prime_lsm <- lsmeans(StatePrime_rg, 
                                   c("State", "PrimeCondition"))
  
  stateeffect = cld(state_lsm, alpha = 0.05, 
      adjust = "tukey", details = TRUE)
  kable(subset(stateeffect$comparisons, 1<2)[,c(1,2,3,6)])
  
  peffect = cld(prime_lsm, alpha = 0.05, 
                    adjust = "tukey", details = TRUE)
  kable(subset(peffect$comparisons, 1<2))
  
  interaction2 = cld(state_x_prime_lsm, by = "PrimeCondition", alpha = 0.05, 
                    adjust = "tukey", details = TRUE)
  kable(subset(interaction2$comparisons, 1<2)[,c(1,2,3,7)])
  
  interaction3 = cld(state_x_prime_lsm, by = "State", alpha = 0.05, 
                     adjust = "tukey", details = TRUE)
  kable(subset(interaction3$comparisons, 1<2)[,c(1,2,3,7)])
  
 
  #for correct trials
  StatePrime_rg_correct <- ref.grid(state_prime_aov_correct)
  
  state_lsm_correct <- lsmeans(StatePrime_rg_correct, "State")
  prime_lsm_correct <- lsmeans(StatePrime_rg_correct, "PrimeCondition")
  
  state_x_prime_lsm_correct <- lsmeans(StatePrime_rg_correct, 
                               c("State", "PrimeCondition"))
  
  stateeffect_correct = cld(state_lsm_correct, alpha = 0.05, 
                    adjust = "tukey", details = TRUE)
  kable(subset(stateeffect_correct$comparisons, 1<2)[,c(1,2,3,6)])
  
  peffect_correct = cld(prime_lsm_correct, alpha = 0.05, 
                adjust = "tukey", details = TRUE)
  kable(subset(peffect_correct$comparisons, 1<2))
  
  interaction2_correct = cld(state_x_prime_lsm_correct, by = "PrimeCondition", alpha = 0.05, 
                     adjust = "tukey", details = TRUE)
  kable(subset(interaction2_correct$comparisons, 1<2)[,c(1,2,3,7)])
  
  interaction3 = cld(state_x_prime_lsm_correct, by = "State", alpha = 0.05, 
                     adjust = "tukey", details = TRUE)
  kable(subset(interaction3$comparisons, 1<2)[,c(1,2,3,7)])

  #for incorrect trials
  
  StatePrime_rg_incorrect <- ref.grid(state_prime_aov_incorrect)
  
  state_lsm_incorrect <- lsmeans(StatePrime_rg_incorrect, "State")
  prime_lsm_incorrect <- lsmeans(StatePrime_rg_incorrect, "PrimeCondition")
  
  state_x_prime_lsm_incorrect <- lsmeans(StatePrime_rg_incorrect, 
                                       c("State", "PrimeCondition"))
  
  stateeffect_incorrect = cld(state_lsm_incorrect, alpha = 0.05, 
                            adjust = "tukey", details = TRUE)
  kable(subset(stateeffect_incorrect$comparisons, 1<2)[,c(1,2,3,6)])
  
  peffect_incorrect = cld(prime_lsm_incorrect, alpha = 0.05, 
                        adjust = "tukey", details = TRUE)
  kable(subset(peffect_incorrect$comparisons, 1<2))
  
  interaction2_incorrect = cld(state_x_prime_lsm_incorrect, by = "PrimeCondition", alpha = 0.05, 
                             adjust = "tukey", details = TRUE)
  kable(subset(interaction2_incorrect$comparisons, 1<2)[,c(1,2,3,7)])
  
  interaction3_incorrect = cld(state_x_prime_lsm_incorrect, by = "State", alpha = 0.05, 
                     adjust = "tukey", details = TRUE)
  kable(subset(interaction3_incorrect$comparisons, 1<2)[,c(1,2,3,7)])
  
  
  # STATE by prime retrieved/not 
  
  #for whether prime retrieved/not by prime type
  retrievedstate_rg <- ref.grid(prime_retrieved_state_aov)
  
  retrieved_lsm <- lsmeans(retrievedstate_rg, "Retrieved")
  state_lsm <- lsmeans(retrievedstate_rg, "State")
  
  retrieved_x_state_lsm <- lsmeans(retrievedstate_rg, 
                                   c("Retrieved", "State"))
  
  reteffect = cld(retrieved_lsm, alpha = 0.05, 
                  adjust = "tukey", details = TRUE)
  kable(subset(reteffect$comparisons, 1<2))
  
  stateeffect = cld(state_lsm, alpha = 0.05, 
                    adjust = "tukey", details = TRUE)
  kable(subset(stateeffect$comparisons, 1<2))
  
  
  interactionbyret = cld(retrieved_x_state_lsm, by = "Retrieved", alpha = 0.05, 
                           adjust = "tukey", details = TRUE)
  kable(subset(interactionbyret$comparisons, 1<2)[,c(1,2,3,7)])
  
  interactionbystate = cld(retrieved_x_state_lsm, by = "State", alpha = 0.05, 
                         adjust = "tukey", details = TRUE)
  kable(subset(interactionbystate$comparisons, 1<2)[,c(1,2,3,7)])
  
# THREEWAY INTERACTIONS
  
  #for whether prime retrieved/not by prime type
  threeway_rg <- ref.grid(threeway_aov)
  
  retrieved_lsm <- lsmeans(threeway_rg, "Retrieved")
  state_lsm <- lsmeans(threeway_rg, "State")
  prime_lsm <- lsmeans(threeway_rg, "PrimeCondition")
  
  threeway_int_lsm <- lsmeans(threeway_rg, 
                                   c("Retrieved", "PrimeCondition", "State" ))
  
  reteffect = cld(retrieved_lsm, alpha = 0.05, 
                  adjust = "tukey", details = TRUE)
  kable(subset(reteffect$comparisons, 1<2))
  
  stateeffect = cld(state_lsm, alpha = 0.05, 
                    adjust = "tukey", details = TRUE)
  kable(subset(stateeffect$comparisons, 1<2))
  
  
  interactionbyret = cld(threeway_int_lsm, by = c("Retrieved", "PrimeCondition"), alpha = 0.05, 
                         adjust = "tukey", details = TRUE)
  kable(subset(interactionbyret$comparisons, 1<2)[,c(1,2,3,8)])
  
  interactionbystate = cld(threeway_int_lsm, by = "State", alpha = 0.05, 
                           adjust = "tukey", details = TRUE)
  kable(subset(interactionbystate$comparisons, 1<2)[,c(1,2,3,7)])
  
  #plotting simple distributions
  
  #plotting target accuracy
  
  ggplot(y, aes(x = y$meanacc  )) + 
    geom_histogram(binwidth = 0.1, 
                   color = "gray", fill = "darkslateblue", size = 0.01, 
                   na.rm = TRUE) + 
    xlim(0,1)+
    theme_few()+
    xlab("Mean Target Accuracy") + ylab("Frequency") + 
    ggtitle("Distribution of Target Accuracy") 
  
  #prime accuracy
  
  ggplot(y, aes(x = y$meanprimeacc  )) + 
    geom_histogram(binwidth = 0.1, 
                   color = "gray", fill = "goldenrod1", size = 0.01, 
                   na.rm = TRUE) + 
    xlim(0,1)+
    theme_few()+
    xlab("Mean Prime Accuracy") + ylab("Frequency") + 
    ggtitle("Distribution of Prime Accuracy") 