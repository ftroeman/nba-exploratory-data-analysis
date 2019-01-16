---
  title: "Sports Betting Analysis of NBA Matches Since 2007"
author: "Peter Grantcharov, Fernando Troeman, Po-Chieh Liu"
date: "12/10/2018"
output:
  pdf_document: default
---
  
knitr::opts_chunk$set(echo = TRUE)
library(plyr)
library(tidyverse)
library(dygraphs)
library(xts)
library(GGally)
library(gridExtra)
library(knitr)
library(lubridate)
library(ggthemes)

combined = read_csv("Data/combined.csv")[-c(1)]
intermediate = read_csv("Data/intermediate.csv")
tidy = read_csv("Data/tidy.csv")



options(dplyr.width = Inf)
kable(combined[1:4,], caption = "Untidy Data")

kable(tidy[1:5, c(2, 4, 6, 7, 8, 9, 14, 16, 18, 22, 24)], caption = "Tidy Data")



kable((combined %>% filter(`2H` == 'pk'))[1:3,],
      caption = "Pick 'em Examples *Notice column '2H'")
kable(combined[c(1975,23521),], caption = "Faulty Over/Under Cases")

quarter_scores <- intermediate %>% 
  select(V1, V2, V3, V4, H1, H2, H3, H4)
quarter_scores <-gather(quarter_scores, key = "Quarter", value = "Score")

before <- ggplot(quarter_scores, aes(x = Quarter, y = Score)) +
  geom_boxplot(fill = "brown", color = "black") +
  ggtitle("Boxplots of Quarter Point Totals Before Adjustments") +
  labs(x = "Quarter",
       y = "Number of Points",
       caption = "V = visitor, H = home") +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=14)) +
  theme_bw()

quarter_scores <- tidy %>% 
  select(V1, V2, V3, V4, H1, H2, H3, H4)
quarter_scores <- gather(quarter_scores, key = "Quarter", value = "Score")

after <- ggplot(quarter_scores, aes(x = Quarter, y = Score)) +
  geom_boxplot(fill = "brown", color = "black") +
  ggtitle("Boxplots of Quarter Point Totals After Adjustments") +
  labs(x = "Quarter",
       y = "Number of Points",
       caption = "V = visitor, H = home") +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=14)) +
  theme_bw()

grid.arrange(before, after, ncol = 2, widths = c(9, 9))


OUs <- intermediate %>% select(OUOpen, OUClose)
OUs <- gather(OUs, key = "OpenClose", value = "Value")

before <- ggplot(OUs, aes(x = OpenClose, y = Value)) +
  geom_boxplot(fill = "brown", color = "black") +
  ggtitle("Boxplots of Over/Under Scores at the Open and Close Before") +
  labs(x = "Open/Close",
       y = "Over/Under for the Total Number of Points") +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=14)) +
  theme_bw()

OUs <- tidy %>% select(OUOpen, OUClose)
OUs <- gather(OUs, key = "OpenClose", value = "Value")

after <- ggplot(OUs, aes(x = OpenClose, y = Value)) +
  geom_boxplot(fill = "brown", color = "black") +
  ggtitle("Boxplots of Over/Under Scores at the Open and Close After") +
  labs(x = "Open/Close",
       y = "Over/Under for the Total Number of Points") +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=14)) +
  theme_bw()

grid.arrange(before, after, ncol = 2, widths = c(9, 9))


# Fix outliers for Over/Under scores
tidy.OUOpen = tidy.OUOpen[tidy.OUOpen > 100]


Tots <- intermediate %>% select(VF, HF)
Tots <- gather(Tots, key = "Team", value = "Value")

before <- ggplot(Tots, aes(x = Team, y = Value)) +
  geom_boxplot(fill = "brown", color = "black") +
  ggtitle("Boxplots of Game Point Totals Before Adjustments") +
  labs(x = "Visitor/Home Final Score",
       y = "Total Number of Points") +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=14)) +
  theme_bw()

Tots <- tidy %>% select(VF, HF)
Tots <- gather(Tots, key = "Team", value = "Value")

after <- ggplot(Tots, aes(x = Team, y = Value)) +
  geom_boxplot(fill = "brown", color = "black") +
  ggtitle("Boxplots of Game Point Totals After Adjustments") +
  labs(x = "Visitor/Home Final Score",
       y = "Total Number of Points") +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=14)) +
  theme_bw()

grid.arrange(before, after, ncol = 2, widths = c(9, 9))

tidy <- tidy %>% 
  select(Date, SZN, V, H, V1, V2, V3, V4, H1, H2, H3, H4,
         VF, HF, OUOpen, OUClose, OU2H, VMoney, HMoney) %>% 
  mutate(Total = VF+HF) %>% 
  mutate(Total_2H = V3+V4+H3+H4) 

df <- tidy %>% 
  select(OUOpen, OUClose) %>%
  mutate(Diff = OUClose - OUOpen)

histo <- ggplot(df, aes(x = Diff)) +
  geom_histogram(binwidth = 0.5, fill='brown', color='black') +
  xlab("Score Difference") + 
  ylab("Frequency Count") +
  ggtitle("Histogram of Changes in Over/Under Values") +
  theme_bw()

box <- ggplot(df, aes(y= Diff)) + 
  geom_boxplot(fill = "brown") +
  scale_x_discrete() + 
  xlab("(OUClose - OUOpen)") + 
  ylab("Score Difference") +
  ggtitle("Boxplot of Changes in Over/Under Values") +
  theme_bw()

qq <- ggplot(df, aes(sample = Diff)) +
  geom_qq() +
  stat_qq_line(distribution = qnorm, color= "brown", size = 2) +
  xlab("Theoratical") +
  ylab("Score Difference") +
  ggtitle("QQ-Plot of Changes in Over/Under Values") +
  theme_bw()

df <- tidy %>% select(OUClose, OU2H) %>% mutate(Ratio = OUClose / OU2H)

scat <- ggplot(df, aes(x = OUClose, y = OU2H)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = lm, se = FALSE, color = "brown", show.legend = TRUE, size = 2) + 
  scale_x_continuous("Over/Under Scores for Full Game",
                     breaks = c(170, 180, 190, 200, 210, 220, 230, 240),
                     labels = c("170", "180", "190", "200", "210", "220", "230", "240")) +
  scale_y_continuous("Over/Under Scores for 2nd Half",
                     breaks = c(85,90,95,100,105,110,115,120),
                     labels = c("85","90","95","100","105","110","115","120")) +
  ggtitle("Over/Under Scores for Full Game vs. 2nd Half") +
  theme_bw()

grid.arrange(histo, box, qq, scat, ncol = 2)




df1 <- tidy %>% 
  select(OUOpen, OUClose, Total, OU2H, Total_2H) %>%
  mutate(Diff = OUClose-OUOpen) %>% 
  mutate(Diff_dec_idx = 
           cut(Diff,
               breaks = c(-Inf, -5.49, -4.49, -3.49, -2.49, -1.49, -0.49,
                          0.49, 1.49, 2.49, 3.49, 4.49, 5.49, Inf), 
               labels = c("<= -6","-5","-4", "-3",
                          "-2", "-1", "skip",
                          "1", "2", "3",
                          "4", "5", ">= 6"))) %>%
  filter(!(Diff_dec_idx == "skip")) %>%
  mutate(earning = if_else( (Total_2H-OU2H)*(Diff)>0, 95,
                            if_else(Total_2H==OU2H, 0, -100) ) )

df2 <- df1 %>%  group_by(Diff_dec_idx) %>%
  summarise(profit = sum(earning)) %>%
  mutate( Gain = if_else(profit>0, "+", "-"))

ggplot(df2, aes(x=Diff_dec_idx, y = profit, fill = Gain)) +
  geom_bar(stat='identity', color = "black") +
  xlab("Open-to-Close Over/Under Change") +
  ylab("Profit ($)") +
  scale_fill_manual(values=c("brown", "darkgreen")) +
  geom_text(aes(label = paste(profit), vjust=if_else(profit>0,-0.5,1)), size = 3) +
  ggtitle("Profit by Betting System Based on Open-to-Close Score Change") +
  theme_bw() +
  theme(legend.position = "none")




df <- tidy  %>% select(Date, OUOpen, OUClose, OU2H, Total_2H) %>%
  mutate(Diff = OUClose-OUOpen)

df$Profit = 0
for (i in 2:length(df$Profit)){
  if (df$OU2H[i] > df$Total_2H[i]) {
    df$Profit[i] <- df$Profit[i - 1] + 95
  }
  else if (df$OU2H[i] < df$Total_2H[i]) {
    df$Profit[i] <- df$Profit[i - 1] - 100
  }
  else {
    df$Profit[i] <- df$Profit[i - 1]
  }
}

winner <- ggplot(df, aes(y = Profit/1000, x = Date, col = "Profit")) +
  geom_line(lwd = 0.1) +
  geom_ribbon(aes(x = Date, ymax = Profit/1000), ymin = 0, alpha=0.3,
              fill = "#009E73", color = "darkgreen") + 
  ggtitle("Profit Over Time by Betting 'Under' for 2nd Half Over/Under") +
  labs(x = "Date", y = "Running Profit ($, thousands)") +
  theme_bw()

winner + 
  geom_hline(aes(yintercept = min(Profit)/1000, colour = "Low"),
             alpha = 0.8, lwd = 1, linetype="dotdash") +
  geom_hline(aes(yintercept = Profit[14186]/1000, colour = "Finish"),
             alpha = 0.8, lwd = 1, linetype="dotdash") +
  scale_color_manual(values = c('Profit' = 'darkgreen', "Low" = "red",
                                "High" = "blue", "Finish" = "black")) +
  labs(x = "Date", y = "Running Profit ($, thousands)", color = "Legend:")



tidy <- read_csv("Data/tidy.csv")
tidy$HSpreadChange <- tidy$HSpreadClose - tidy$HSpreadOpen
tidy$VSpreadChange <- tidy$VSpreadClose - tidy$VSpreadOpen
tidy <- tidy %>% select(Date, VF, HF, HSpreadClose,
                        VSpreadClose, HSpreadChange, VSpreadChange)

spreadChange <- data_frame("Change" = c(-2.5, -2, -1.5, -1,
                                        -0.5, 0.5, 1, 1.5, 2, 2.5))
spreadChange$Profit <- 0

for (j in 1:length(spreadChange$Change)){ 
  tidy$Payout <- 0
  for (i in 1:length(tidy$Payout)) {
    if (spreadChange$Change[j] == tidy$HSpreadChange[i]) {  
      if (tidy$VF[i] - tidy$HF[i] < tidy$HSpreadClose[i]) {  
        tidy$Payout[i] <- 95
      } else if (tidy$VF[i] - tidy$HF[i] > tidy$HSpreadClose[i]){
        tidy$Payout[i] <- -100
      }
    } else if (spreadChange$Change[j] == tidy$VSpreadChange[i]) {
      if (tidy$HF[i] - tidy$VF[i] < tidy$VSpreadClose[i]) {  
        tidy$Payout[i] <- 95
      } else if (tidy$HF[i] - tidy$VF[i] > tidy$VSpreadClose[i]) {
        tidy$Payout[i] <- -100
      }
    }
  }
  spreadChange$Profit[j] = sum(tidy$Payout)
}
spreadChange <- spreadChange %>%  mutate( Gain = if_else(Profit>0, "+", "-"))
spreadChange$Change <- as.factor(spreadChange$Change)
com <- ggplot(spreadChange, aes(x=Change, y=Profit, fill  = Gain)) +
  geom_bar(stat='identity', color='black') +
  geom_text(aes(label = paste(Profit),
                vjust = ifelse(Profit >= 0, -0.5, 1.5)), size=3) +
  scale_y_continuous(limits = c(-15000,5000)) +
  scale_fill_manual(values=c("brown", "darkgreen")) +
  labs(x="Spread Change", y = "Profit ($)",
       title="Betting in Direction of Spread Change") +
  theme_bw() +
  theme(legend.position = "none")

com




for (j in 1:length(spreadChange$Change)){ 
  tidy$Payout <- 0
  for (i in 1:length(tidy$Payout)) {
    if (spreadChange$Change[j] == tidy$HSpreadChange[i]) {  
      if (tidy$VF[i] - tidy$HF[i] < tidy$HSpreadClose[i]) {  
        tidy$Payout[i] <- 100
      } else if (tidy$VF[i] - tidy$HF[i] > tidy$HSpreadClose[i]){
        tidy$Payout[i] <- -100
      }
    } else if (spreadChange$Change[j] == tidy$VSpreadChange[i]) {
      if (tidy$HF[i] - tidy$VF[i] < tidy$VSpreadClose[i]) {
        tidy$Payout[i] <- 100
      } else if (tidy$HF[i] - tidy$VF[i] > tidy$VSpreadClose[i]) {
        tidy$Payout[i] <- -100
      }
    }
  }
  spreadChange$Profit[j] = sum(tidy$Payout)
}

spreadChange <- spreadChange %>%  mutate( Gain = if_else(Profit>0, "+", "-"))
spreadChange$Change <- as.factor(spreadChange$Change)
nocom <- ggplot(spreadChange, aes(x=Change, y=Profit, fill = Gain)) +
  geom_bar(stat='identity', color='black') +
  geom_text(aes(label = paste(Profit),
                vjust = ifelse(Profit >= 0, -0.5, 1.5)), size=3) +
  scale_y_continuous(limits = c(-15000,5000)) +
  scale_fill_manual(values=c("brown", "darkgreen")) +
  labs(x="Spread Change", y = " ",
       title="Net Profit Without Commissions") +
  theme_bw() +
  theme(legend.position = "none")

com <- com + labs(title = "Net Profit With Commissions")
grid.arrange(com, nocom, ncol = 2)



tidy$Payout <- 0
tidy$Payout[1] <- 100
for (i in 2:length(tidy$Payout)) {
  if (tidy$HSpreadChange[i] > 0) {  
    if (tidy$VF[i] - tidy$HF[i] < tidy$HSpreadClose[i]) { 
      tidy$Payout[i] <- tidy$Payout[i - 1] + 100
    } else if (tidy$VF[i] - tidy$HF[i] > tidy$HSpreadClose[i]){
      tidy$Payout[i] <- tidy$Payout[i - 1] - 100
    } else {
      tidy$Payout[i] <- tidy$Payout[i - 1]
    }
  } else if (tidy$VSpreadChange[i] > 0) {
    if (tidy$HF[i] - tidy$VF[i] < tidy$VSpreadClose[i]) { 
      tidy$Payout[i] <- tidy$Payout[i - 1] + 100
    } else if (tidy$HF[i] - tidy$VF[i] > tidy$VSpreadClose[i]) {
      tidy$Payout[i] <- tidy$Payout[i - 1] - 100
    } else {
      tidy$Payout[i] <- tidy$Payout[i - 1]
    }
  } else {
    tidy$Payout[i] <- tidy$Payout[i - 1]
  }
}

ggplot(tidy, aes(y = Payout, x = Date, col = "Payout")) +
  geom_line(lwd = 0.1) +
  geom_ribbon(aes(x = Date, ymax = Payout), ymin = 0, alpha=0.3,
              fill = "#009E73", color = "darkgreen") + 
  geom_hline(aes(yintercept = max(Payout), colour = "High"),
             alpha = 0.8, lwd = 1, linetype="dotdash") +
  geom_hline(aes(yintercept = min(Payout), colour = "Low"), alpha = 0.8, lwd = 1,
             linetype="dotdash") +
  geom_hline(aes(yintercept = Payout[14186], colour = "Finish"), alpha = 0.8,
             lwd = 1, linetype="dotdash") +
  ggtitle("Running Profit When Betting Against Spread Movement") +
  scale_color_manual(values = c('Payout' = 'darkgreen', "Low" = "red",
                                "High" = "blue", "Finish" = "black")) +
  labs(x = "Date", y = "Running Profit ($)", color = "Legend:") +
  theme_bw()



tidy <- read_csv("Data/tidy.csv")
df2 <- tidy %>% 
  select(V, H, VF, HF, VMoney, HMoney) %>%
  mutate(Bet_V_win = if_else(VF>HF, if_else(VMoney>0, VMoney, -100/VMoney*100),
                             if_else(VF<HF, -100, 0))) %>%
  mutate(Bet_H_win = if_else(VF<HF, if_else(HMoney>0, HMoney, -100/HMoney*100),
                             if_else(VF>HF, -100, 0)))

df_V <- df2 %>%
  group_by(V) %>% summarise_at("Bet_V_win", sum) %>%
  rename(Team = V)

df_H <- df2 %>%
  group_by(H) %>% summarise_at("Bet_H_win", sum) %>%
  rename(Team = H)

df_Team <- df_V %>% 
  inner_join(df_H) %>%
  mutate(profit = Bet_V_win + Bet_H_win)

ggplot(df_Team, aes(x= profit, y= fct_reorder(Team, profit))) +
  geom_point(color = "brown", size = 2) +
  labs(x="Profit ($)", y="NBA Team") +
  ggtitle("Cleveland Dot Plot of Net Profit by Betting on Same Team") +
  theme_bw()

