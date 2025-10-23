#WATER TANK
library(randomForest)
library(mgcv)
library(e1071)
library(splines)
library(ggplot2)

#dataset 
water<-knowledge

#modification of data in one only matrix 24x365
stato<- as.tibble(t(water$s))
stato<-stato %>%
  gather(V1:V365, key="giorno", value="stato_corrente")
stato<- as.data.frame(stato)
azione<- as.tibble(t(water$a))
azione<-azione %>%
  gather(V1:V365, key="giorno", value="azione")
azione<- as.data.frame(azione)
stato_<- as.tibble(t(water$s_))
stato_succ<-stato_ %>%
  gather(V1:V365, key="giorno", value="stato_successivo")
stato_succ<- as.data.frame(stato_succ)
reward<- as.tibble(t(water$r))
reward<-reward %>%
  gather(V1:V365, key="giorno", value="reward")
reward<- as.data.frame(reward)
done<- rep(c(rep(0, 23) , 1),  365)

dati<-data.frame(stato=stato$stato_corrente, 
                 azione=azione$azione, 
                 stato_succ=stato_succ$stato_successivo, 
                 reward= - reward$reward,  #importo il costo direttamente con il
                 #meno perche devo massimizzare
                 done=done)
write.csv(dati, "water_tank.csv")


#EDA
dati<-read.csv("water_tank.csv") [,-1]
#plots 
library(ggplot2)
ggplot(dati, aes(x = azione, y = reward_new)) +
  geom_point(color = "aquamarine4", size = 1, alpha = 0.7) +
  # geom_smooth(method = "loess", se = FALSE, color = "darkgreen", linewidth = 1) +
  theme_light() +
  theme(
    #plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size=10),
    panel.grid.minor = element_blank()
  ) +
  labs(
    #title = "Reward in funzione dello stato",
    x = "Azione corrente",
    y = "Reward"
  )




# FQI 
# Pre processing data 
dati <- read.csv("water_tank.csv")[,-1]
#definition of reward
lambda <-0.2
dati$reward_new <- dati$reward - lambda*(dati$stato_succ - dati$stato)^2

# training test division

index <- 1:7200 #300 days
training <- dati[index,]

test <- dati[-index,]

# Hyper parameters
gamma <- 0.99
azioni_possibili <- seq(0, 10, by = 0.5)
n_iter <- 100

q_mod <- NULL
mse<-c()

media_reward<-c()
std_reward<-c()
std_costo<-c()
media_costo<-c()

prices = c(rep(1,6),rep(3,2),rep(2,10),rep(3,2),rep(2,2),rep(1,2)) # energy price tariff
eta = 0.85
gruppi <- rep(1:(nrow(training)/24), each = 24)

#FQI algorithm
set.seed(123)
for (k in 1:n_iter) {
  print(k)
  
  target_q <- training$reward_new
  
  if (!is.null(q_mod)) {
    # Q-values 
    stati_unici <- unique(training$stato_succ)
    q_next <-  sapply(stati_unici, function(s) {
      new_data <- data.frame(stato = s, azione = azioni_possibili)
      p<-predict(q_mod, new_data)
      out<-c(max(p),azioni_possibili[which.max(p)])
      return (out)
    }) 
    
    idx <- match(training$stato_succ, stati_unici)
    temp<-data.frame(stato=training$stato_succ, azione=q_next[2,][idx])
    
    #cost check
    costo_temp<- -prices[rep(1:24, 300)] * (1/eta) * (temp$azione^(1/3))
    costo_daily <- tapply(costo_temp, gruppi, sum)
    media_costo[k]<-mean(costo_daily)
    std_costo[k]<-sd(costo_daily)
    #reward check
    reward_temp<--prices[rep(1:24, 300)] * (1/eta) * (temp$azione^(1/3)) - lambda*(training$stato_succ - training$stato)^2
    reward_daily <- tapply(reward_temp, gruppi, sum)
    media_reward[k]<-mean(reward_daily)
    std_reward[k]<-sd(reward_daily)
    

    target_q[training$done != 1] <- target_q[training$done != 1] + gamma * q_next[1,][idx][training$done != 1]
  }
  
  #q_mod<-lm(target_q~ stato + azione, data=training)
  #SPLINES CUBICHE
  #q_mod<-mgcv::gam(target_q~s(stato, bs="cr", k=16) +s(azione, bs="cr", k=12)+ stato*azione, data=training)
  # q_mod<-mgcv::gam(target_q~s(stato)+ s(azione)+ azione*stato,  data=training, method="REML")
  
  #MARs
  #q_mod<-earth::earth(target_q~stato+ azione, data = training, degree = 2)
  
  #SPLINES SMOOTHING
  #q_mod<-lm(target_q~ +stato*azione, data=training)
  
  #RANDOM FOREST
  q_mod <- randomForest::randomForest(target_q ~ stato + azione, data = training, ntree = 200, nodesize = 5)
  
  
  #SVM 
  # q_mod <- e1071::svm(target_q ~ stato + azione, data = training, kernel="radial",  type="eps-regression", gamma=0.1 )
  
  mse[k] <- mean((predict(q_mod) - target_q)^2)
}

#optimal action function
optimal_action<-function(state){
  prediction_grid <- expand.grid(
    stato = unique(state),
    azione = azioni_possibili
  )
  q_vals<-predict(q_mod, prediction_grid)
  azioni_possibili[which.max(q_vals)]
}

#update state, action , tank level
domanda<- test$stato + test$azione- test$stato_succ

stato_nuovo<-numeric(nrow(test))
stato_nuovo[1]<- test$stato[1]
azione_ottima<-c()
done<-test$done
stato_oss<-test$stato

for (i in 1:nrow(test)){
  if (done[i]==0){
    azione_ottima[i]<-optimal_action(stato_nuovo[i])
    stato_nuovo[i+1]<-stato_nuovo[i]+ azione_ottima[i] -domanda[i]}
  else {
    azione_ottima[i]<-optimal_action(stato_nuovo[i])
    stato_nuovo[i+1]<-stato_oss[i+1]
  }
}
stato_nuovo<-stato_nuovo[-1561]
table(azione_ottima)
summary(stato_nuovo) #livello tank


sum(stato_nuovo<=5)
sum(stato_nuovo>=50)


#Costs under new target policy
costo <- prices[rep(1:24, 65)] * (1/eta) * (azione_ottima^(1/3))
sum(costo- -test$reward)

fresh<-data.frame(stato=stato_nuovo, azione=azione_ottima, costo=costo)
write.csv(fresh, "azioni_stato_rflambda02.csv")

#Daily reward
dati<-read.csv("dati_reward.csv")
library(ggplot2)
ggplot_with_ci <- function(media, se, 
                           xlab = "Iterazioni", 
                           ylab = "Valore", 
                           level = 0.95, range_y) {
  n <- length(media)
  z_value <- qnorm(1 - (1 - level)/2)
  df <- data.frame(
    iterazione = 1:n,
    media = media,
    lower = media - z_value * se,
    upper = media + z_value * se
  )
  
  ggplot(df, aes(x = iterazione, y = media)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), 
                fill="lightgreen" ,alpha = 0.2) +
    geom_line(color = "aquamarine4", size = 1) +
    labs(x = xlab, y = ylab) +
    theme_light()+ 
    coord_cartesian(ylim=range_y)
}


ggplot_with_ci(media_costo, std_costo,
               ylab = "Costo medio giornaliero",
               title = "Andamento del costo durante l'addestramento")

ggplot_with_ci(media_reward, std_reward,
               ylab = "Reward media giornaliera", range_y=c(-135,-95))

ggplot_with_ci(dati$media_reward, dati$std_reward,
               ylab = "Reward media giornaliera", range_y=c(-135,-95))

#Complete dataset
domanda<- dati$stato + dati$azione- dati$stato_succ
stato_nuovo<-numeric(nrow(dati))
stato_nuovo[1]<- dati$stato[1]
azione_ottima<-c()
done<-dati$done
stato_oss<-dati$stato
for (i in 1:nrow(dati)){
  if (done[i]==0){
    azione_ottima[i]<-optimal_action(stato_nuovo[i])
    stato_nuovo[i+1]<-stato_nuovo[i]+ azione_ottima[i] -domanda[i]}
  else {
    azione_ottima[i]<-optimal_action(stato_nuovo[i])
    stato_nuovo[i+1]<-stato_oss[i+1]
  }
}

stato_nuovo<-stato_nuovo[-8761]
table(azione_ottima)
summary(stato_nuovo)
sum(stato_nuovo<=5)
sum(stato_nuovo>=50)

#Grafici
dati<-read.csv("dati_azione_nnet.csv")
gruppi <- rep(1:(length(test$reward)/24), each = 24)
costi_oss <- tapply(-test$reward, gruppi, sum)
#new policy
gruppi <- rep(1:(length(dati$costo)/24), each = 24)
costi_new <- tapply(dati$costo, gruppi, sum)
#grafico 
df <- data.frame(
  valore = c(costi_oss, costi_new),
  gruppo = factor(rep(c("Costi policy safe", "Costi nuova policy"), 
                      times = c(length(costi_oss), length(costi_new))))
)

# Calcolo delle mediane
medie <- aggregate(valore ~ gruppo, df, median)

p <- ggplot(df, aes(x = valore, fill = gruppo, color = gruppo)) +
  geom_density(alpha = 0.25, linewidth = 1) +   # densità con più trasparenza
  geom_vline(data = medie, 
             aes(xintercept = valore, color = gruppo),
             linetype = "dashed", linewidth = 0.9, show.legend = FALSE) +
  
  # Titoli e assi
  labs(x = "Valori",
       y = "Densità",
       fill = "Gruppi",
       color = "Gruppi") +
  
  scale_fill_manual(values = c("lightgreen", "aquamarine4")) +
  scale_color_manual(values = c("lightgreen", "aquamarine4")) +
  theme_light() +
  theme(legend.text = element_text(size = 9)
  )

print(p)


#plotting data
dati_grafico <- data.frame(
  tempo = 1:length(stato_nuovo),
  stato = stato_nuovo
)

#plot
ggplot(dati_grafico, aes(x = tempo, y = stato)) +
  geom_line(color = "blue", linewidth=0.8) +
  geom_hline(yintercept = 5, color = "aquamarine3", linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = 50, color = "aquamarine3", linetype = "dashed", linewidth = 0.8) +
  labs(title = "Andamento del next state nel tempo",
       subtitle = "Con vincoli di sicurezza (5 mc3- 50 mc3 )",
       x = "Tempo (ogni ora) ",
       y = "Next state (Livello serbatioio)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_y_continuous(
    breaks = sort(unique(c(seq(floor(min(dati_grafico$stato)),
                               ceiling(max(dati_grafico$stato)),
                               by = 5),
                           5, 50)))
  )


