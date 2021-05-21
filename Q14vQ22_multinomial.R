require("nnet")

logmult<- (D%>%
  mutate(Q22=NA)%>% #preallocate
  mutate(Q22 = ifelse(Q22_1_0 == 1, "Much better", Q22))%>%
  mutate(Q22 = ifelse(Q22_1_1 == 1, "Somewhat better", Q22))%>%
  mutate(Q22 = ifelse(Q22_1_2== 1, "No change", Q22))%>%
  mutate(Q22 = ifelse(Q22_1_3 == 1, "Somewhat worse", Q22))%>%
  mutate(Q22 = ifelse(Q22_1_4 == 1, "Much worse", Q22))%>%
  mutate(Q22=factor(Q22, levels = c("Much better","Somewhat better", "No change", "Somewhat worse", "Much worse")))%>%
  mutate(Q22=factor(Q22, levels = c("Much better","Somewhat better", "No change", "Somewhat worse", "Much worse")))%>%
  mutate(Q14f=NA)%>% #preallocate 
  mutate(Q14f= ifelse(Q14_a == 1 | Q14_a == 2 | Q14_a == 3 | Q14_a == 4 | Q14_a == 7| Q14_a == 6 | Q14_a == 5 , "Less than 24 weeks", Q14f))%>%
  mutate(Q14f= ifelse(Q14_a == 8  , "Over 24 weeks", Q14f)))


logmult$Q22<- relevel(logmult$Q22, ref = "No change")
logmult$Q14f<- as.factor(logmult$Q14f)
logmult$Q14f<- relevel(logmult$Q14f, ref = "Over 24 weeks")


test0<- multinom(Q22 ~ 1, data=logmult) #intercept model
test <- multinom(Q22 ~ Q14f, data = logmult)

anova(test0, test)

summary(test)
# Call:
#   multinom(formula = Q22 ~ Q14f, data = logmult)
# 
# Coefficients:
#   (Intercept) Q14fLess than 24 weeks
# Much better     -10.806767714             7.84926222
# Somewhat better  -0.153885001            -0.93157066
# Somewhat worse   -0.000137053             0.08768349
# Much worse       -0.441825980            -0.68294467
# 
# Std. Errors:
#   (Intercept) Q14fLess than 24 weeks
# Much better      59.3755068             59.3777219
# Somewhat better   0.3933762              0.4540891
# Somewhat worse    0.3779837              0.4095894
# Much worse        0.4272528              0.4853238
# 
# Residual Deviance: 701.7658 
# AIC: 717.7658 
z <- summary(test)$coefficients/summary(test)$standard.errors
z
# (Intercept) Q14fLess than 24 weeks
# Much better     -0.1820071658              0.1321920
# Somewhat better -0.3911903797             -2.0515152
# Somewhat worse  -0.0003625896              0.2140766
# Much worse      -1.0341090620             -1.4071938
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
# (Intercept) Q14fLess than 24 weeks
# Much better       0.8555771              0.8948324
# Somewhat better   0.6956565              0.0402168
# Somewhat worse    0.9997107              0.8304873
# Much worse        0.3010852              0.1593699

  
library(jmv) #very nice descriptives table!
descriptives(logmult, vars= vars(Q22, Q14f), freq=TRUE)


frequency<-table(logmult$Q14f, logmult$Q22) #to calculate chisqared difference
chisq.test(frequency)
# 
# Pearson's Chi-squared test
# 
# data:  frequency
# X-squared = 8.3953, df = 4, p-value = 0.07813
# 
# Warning message:
# In chisq.test(frequency) : Chi-squared approximation may be incorrect

#Not really significant.
