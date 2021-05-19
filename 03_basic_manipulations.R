#02 basic general changes
d<-D%>%
  mutate(gender=ifelse(Q9==1, "Female", ifelse(Q9==2, "Male", NA)))%>%
  mutate(stage=ifelse(Q6==1, "PhD student",
                      ifelse(Q6 %in% c(2,3), "Postdoc","Other"))) #Postdoc & ECR considered the same group