#02 basic general changes
wtf<-d%>%
  mutate(gender=ifelse(Q9==1, "Female", ifelse(Q9==2, "Male", NA)))%>%
  mutate(stage=ifelse(Q6==1, "PhD student",
                      ifelse(Q6 %in% c(2,3), "Postdoc","Other")))