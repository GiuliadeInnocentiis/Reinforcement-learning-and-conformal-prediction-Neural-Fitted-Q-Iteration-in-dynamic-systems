#Conformal prediction
dati <- read.csv("water_tank.csv")[,-1]
lambda <-0.2
dati$reward_new <- dati$reward - lambda*(dati$stato_succ - dati$stato)^2
tempo<- rep(seq(1:24), 365)
dati$tempo<-rep(seq(1:24), 365)

#trainin and test division
index <- 1:7200 #300 days
training <- dati[index,]
test <- dati[-index,]

#calibration division
#255 training days, 45 calibration days 
idx<-1:6120
train<-training[idx, ]
cal<-training[-idx,]

#training the model
# hyoerparameters
gamma <- 0.99
azioni_possibili <- seq(0, 10, by = 0.5)
n_iter <- 50

q_mod <- NULL
mse<-c()

media_reward<-c()
std_reward<-c()
std_costo<-c()
media_costo<-c()

prices = c(rep(1,6),rep(3,2),rep(2,10),rep(3,2),rep(2,2),rep(1,2)) # energy price tariff
eta = 0.85
gruppi <- rep(1:(nrow(training)/24), each = 24)


#without the introduction of a temporal indicator
set.seed(123)
 for (k in 1:n_iter) {
 print(k)
 target_q <- training$reward_new

   if (!is.null(q_mod)) {
     # Pre-calcolo tutti i Q-values per stati unici
     stati_unici <- training$stato_succ
     t<-training$
     q_next <-  sapply(stati_unici, function(s) {
       new_data <- data.frame(stato = s, azione = azioni_possibili)
       p<-predict(q_mod, new_data)
       out<-c(max(p),azioni_possibili[which.max(p)])
       return (out)
     }) #mi crea una matrice con 2 righe e 6996 colonne

     idx <- match(training$stato_succ, stati_unici)
     temp<-data.frame(stato=training$stato_succ, azione=q_next[2,][idx])
     #reward check
     reward_temp<--prices[rep(1:24, 300)] * (1/eta) * (temp$azione^(1/3)) - lambda*(training$stato_succ - training$stato)^2
     reward_daily <- tapply(reward_temp, gruppi, sum)
     media_reward[k]<-mean(reward_daily)
     std_reward[k]<-sd(reward_daily)

     # Mappatura veloce
     target_q[training$done != 1] <- target_q[training$done != 1] + gamma * q_next[1,][idx][training$done != 1]
   }

   #SPLINES CUBICHE
   # q_mod<-mgcv::gam(target_q~s(stato, bs="cr", k=16) +s(azione, bs="cr", k=12)+ stato*azione, data=training, method="REML")
   # q_mod<-mgcv::gam(target_q~s(stato)+ s(azione)+ azione*stato,  data=training, method="REML")

   #MARs
   #q_mod<-earth::earth(target_q~stato+ azione, data = training, degree = 2)

   #SPLINES SMOOTHING
   #q_mod<-lm(target_q~ +stato*azione, data=training)

   #RANDOM FOREST
   q_mod <- randomForest::randomForest(target_q ~ stato + azione + tempo, data = training, ntree = 200, nodesize = 5)

      #SVM
   # q_mod <- e1071::svm(target_q ~ stato + azione, data = training, kernel="radial",  type="eps-regression", gamma=0.1 )

   mse[k] <- mean((predict(q_mod) - target_q)^2)
 }

#introducing temporal indicator
set.seed(123)
for (k in 1:n_iter) {
  print(k)
  
  target_q <- training$reward_new
  
  if (!is.null(q_mod)) {
    # Pre-calcolo tutti i Q-values per stati unici 
    stati_unici <- training$stato_succ
    tempo<-training$tempo
    q_next<-matrix(0, nrow=nrow(training), ncol=2)
    for (i in 1:nrow(training)){
      new_data <- data.frame(stato = training$stato[i], tempo=training$tempo[i],  azione = azioni_possibili)
      p<-predict(q_mod, new_data)
      out<-c(max(p),azioni_possibili[which.max(p)])
      q_next[i,]<-out
    }
    
    idx <- match(training$stato_succ, stati_unici)
    temp<-data.frame(stato=training$stato_succ, azione=q_next[2,][i])
    

    #che per la reward
    reward_temp<--prices[rep(1:24, 300)] * (1/eta) * (temp$azione^(1/3)) - lambda*(training$stato_succ - training$stato)^2
    reward_daily <- tapply(reward_temp, gruppi, sum)
    media_reward[k]<-mean(reward_daily)
    std_reward[k]<-sd(reward_daily)
    
    target_q[training$done != 1] <- target_q[training$done != 1] + gamma * q_next[,1][training$done != 1]
  }
  
  #q_mod<-lm(target_q ~ stato + azione + tempo, data = training)
  #SPLINES CUBICHE
  #q_mod<-mgcv::gam(target_q~s(stato, bs="cr", k=16) +s(azione, bs="cr", k=12)+ stato*azione + tempo, data=training, method="REML")
  # q_mod<-mgcv::gam(target_q~s(stato)+ s(azione)+ azione*stato,  data=training, method="REML")
  
  #MARs
  #q_mod<-earth::earth(target_q~stato+ azione, data = training, degree = 2)
  
  #SPLINES SMOOTHING
  #q_mod<-lm(target_q~ +stato*azione, data=training)
  
  #RANDOM FOREST
  #q_mod <- randomForest::randomForest(target_q ~ stato + azione + tempo, data = training, ntree = 200, nodesize = 5)
  
  
  #SVM 
  # q_mod <- e1071::svm(target_q ~ stato + azione, data = training, kernel="radial",  type="eps-regression", gamma=0.1 )
  
  mse[k] <- mean((predict(q_mod) - target_q)^2)
}



#creation of dataset with observed and predicted action 
indice_oss<-1:480

azione_behav<-train$azione[indice_oss]
stato_behav<-train$stato[indice_oss]
statosucc_behav<-train$stato_succ[indice_oss]
done_behav<-train$done[indice_oss]
time_b<-train$tempo[indice_oss]

#optimal action
optimal_action<-function(state, time){
  prediction_grid <- expand.grid(
    stato = unique(state),
    tempo=time,
    azione = azioni_possibili
  )
  q_vals<-predict(q_mod, prediction_grid)
  azioni_possibili[which.max(q_vals)]
}

#update state, action and tank level with the new target policy
domanda<- stato_behav+azione_behav-statosucc_behav

stato_nuovo_b<-numeric(length(stato_behav))
stato_nuovo_b[1]<- stato_behav[1]
azione_ottima_t<-c()
done<-done_behav
stato_oss<-stato_behav

for (i in 1:length(stato_behav)){
  if (done[i]==0){
    azione_ottima_t[i]<-optimal_action(stato_nuovo_b[i], time_b[i])
    stato_nuovo_b[i+1]<-stato_nuovo_b[i]+ azione_ottima_t[i] -domanda[i]}
  else {
    azione_ottima_t[i]<-optimal_action(stato_nuovo_b[i], time_b[i])
    stato_nuovo_b[i+1]<-stato_oss[i+1]
  }
}


dd<-data.frame(y=c(rep(1,480),rep(0,480)) , azioni= c(azione_behav, azione_ottima_t))
dd$azioni<-round(dd$azioni*2)/2

#logistic regression to calculate weights
mod_log<-glm(y~azioni,data=dd, family="binomial")

#calibration
#modify optimal cal to get also the max and not only the action associated to the max
optimal_action_cal<-function(state, time){
  prediction_grid <- expand.grid(
    stato = unique(state),
    tempo=time,
    azione = azioni_possibili
  )
  q_vals<-predict(q_mod, prediction_grid)
  out<-c(max(q_vals), azioni_possibili[which.max(q_vals)])
  out
}


#Calibration update
domanda<- cal$stato+cal$azione-cal$stato_succ
time_c<-cal$tempo
stato_nuovo_c<-numeric(nrow(cal))
stato_nuovo_c[1]<- cal$stato[1]
azione_ottima_c<-c()
done_c<-cal$done
stato_oss_c<-cal$stato
q_val_cal<-c()

for (i in 1:nrow(cal)){
  if (done_c[i]==0){
    azione_ottima_c[i]<-optimal_action_cal(stato_nuovo_c[i], time_c[i])[2]
    q_val_cal[i]<-optimal_action_cal(stato_nuovo_c[i], time_c[i])[1]
    stato_nuovo_c[i+1]<-stato_nuovo_c[i]+ azione_ottima_c[i] -domanda[i]}
  else {
    azione_ottima_c[i]<-optimal_action_cal(stato_nuovo_c[i], time_c[i])[2]
    q_val_cal[i]<-optimal_action_cal(stato_nuovo_c[i], time_c[i])[1]
    stato_nuovo_c[i+1]<-stato_oss_c[i+1]
  }
}

#Weights
pesi<-predict.glm(mod_log, newdata=data.frame(azioni=azione_ottima_c), type="response")
pesi_norm<-pesi/(1-pesi)
#Score function
tempo<-rep(1:24,45)
giorno<-rep(1:45, each=24)
cal<-cbind(cal, tempo, giorno)
q_val_cal_m<-matrix(q_val_cal, nrow=45, ncol=24, byrow=T)
score<-matrix(NA, nrow=45, ncol=24)
for (g in giorno)  {
  dati<-cal[cal$giorno==g,]
  for (t in tempo){
    score[g,t]<-sum(dati$reward_new[t:24]) - q_val_cal_m[g,t]
  }
}

score<-as.vector(t(score))
score_pesati<-score*pesi_norm
ord_score<-sort(score_pesati)
n<-nrow(cal)
q<-ceiling((n+1)*(1-0.1))
eq<-ord_score[q] #quantile of interest

#Test update
optimal_action_test<-function(state,time){
  prediction_grid <- expand.grid(
    stato = unique(state),
    tempo=time,
    azione = azioni_possibili
  )
  q_vals<-predict(q_mod, prediction_grid)
  out<-c(max(q_vals), azioni_possibili[which.max(q_vals)])
  out
}

domanda<- test$stato + test$azione- test$stato_succ
time_t<-test$tempo
stato_nuovo<-numeric(nrow(test))
stato_nuovo[1]<- test$stato[1]
azione_ottima<-c()
done<-test$done
stato_oss<-test$stato
q_val_test<-c()

for (i in 1:nrow(test)){
  if (done[i]==0){
    azione_ottima[i]<-optimal_action_test(stato_nuovo[i], time_t[i])[2]
    q_val_test[i]<-optimal_action_test(stato_nuovo[i], time_t[i])[1]
    stato_nuovo[i+1]<-stato_nuovo[i]+ azione_ottima[i] -domanda[i]}
  else {
    azione_ottima[i]<-optimal_action_test(stato_nuovo[i], time_t[i])[2]
    q_val_test[i]<-optimal_action_test(stato_nuovo[i], time_t[i])[1]
    stato_nuovo[i+1]<-stato_oss[i+1]
  }
}


#Prediction set
PI<-data.frame(q_values=q_val_test, PI_lower=q_val_test-eq, PI_upper=q_val_test+eq)

