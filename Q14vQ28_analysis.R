# [TEST/CREATE FIGURE FOR Q14vQ28]
# Q14	Have you been prevented from accessing your usual place(s) of work due to Covid-19 measures?
# 0	Yes
# 1	No
# Q14_a	How many weeks were you prevented from accessing your usual place(s) of work?
# 0	1-3 weeks
# 1	3-6 weeks
# 2	6-9 weeks
# 3	9-12 weeks
# 4	12-15 weeks
# 5	15-18 weeks
# 6	18-21 weeks
# 7	21-24 weeks
# 8	24 weeks or more
# Q14_b	Which of these do you have for your home working set up?
# 0	Own Desk
# 1	Separate room
# 2	Extra screen
# 3	Office chair
# 4	Own computer
# 5	Stable internet connection
# 6	Privacy
# 7	Quiet
# Q14_c	How adequate is your home working set up?
# Q14_c_1	Please choose one
# 0	Very adequate
# 1	Fairly adequate
# 2	Neither adequate or inadequate
# 3	Fairly inadequate
# 4	Very inadequate
# Q14_d	Is there anything that you need that you do not have for your home-working set up?
#   0	Own Desk
# 1	Separate room
# 2	Extra screen
# 3	Office chair
# 4	Own computer
# 5	Stable internet connection
# 6	Privacy
# 7	Quiet
# 8	Other
# Q14_d_i	If you selected Other, please specify:
  
  # Q28	Overall, how would you rate the effect of Covid-19 and the related measures on your career in the long term?
  # Q28_1	Please choose one
  # 0	Very negative
  # 1	Somewhat negative
  # 2	No effect
  # 3	Somewhat positive
  # 4	Very positive
  # Q28_a	Why?

#relationship between Q14_a	weeks out of work and Q28 long-term effect
leftplot<-(D%>%
  mutate(Q28=NA)%>% #preallocate
  mutate(Q28 = ifelse(Q28_1_0 == 1, "Very negative", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_1 == 1, "Somewhat negative", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_2== 1, "No effect", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_3 == 1, "Somewhat positive", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_4 == 1, "Very positive", Q28))%>%
  mutate(Q28=factor(Q28, levels = c("Very negative","Somewhat negative", "No effect", "Somewhat positive", "Very positive")))%>%
  mutate(Q28=factor(Q28, levels = c("Very negative","Somewhat negative", "No effect", "Somewhat positive", "Very positive")))%>%
  filter(!is.na(gender))%>%
  ggplot(., aes(Q28, Q14_a))+
  geom_violin()+
  geom_jitter(aes(colour=Q28))+
  # facet_grid(stage ~ gender)+
  scale_color_brewer(type = "qual", palette = "Dark2")+
  labs(x="long-term effect of Covid-19", y="weeks out of work")+
  scale_y_continuous(breaks = c(0, 4, 8), labels=c("1-3 weeks", "12-15 weeks", "more than 24 weeks"))+
  OYSTERtheme+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=35)))


#relationship between Q14_a	weeks out of work and Q28 long-term effect
rightplot<-(D%>%
  mutate(Q28=NA)%>% #preallocate
  mutate(Q28 = ifelse(Q28_1_0 == 1, "Very negative", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_1 == 1, "Somewhat negative", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_2== 1, "No effect", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_3 == 1, "Somewhat positive", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_4 == 1, "Very positive", Q28))%>%
  mutate(Q28=factor(Q28, levels = c("Very negative","Somewhat negative", "No effect", "Somewhat positive", "Very positive")))%>%
  mutate(Q28=factor(Q28, levels = c("Very negative","Somewhat negative", "No effect", "Somewhat positive", "Very positive")))%>%
  filter(!is.na(gender))%>%
  ggplot(., aes(Q28, Q14_a))+
  geom_violin()+
  geom_jitter(aes(colour=Q28))+
  facet_grid(stage ~ gender)+
  scale_color_brewer(type = "qual", palette = "Dark2")+
  labs(x="long-term effect of Covid-19", y="")+
  scale_y_continuous(breaks = c(0, 4, 8), labels=c("1-3 weeks", "12-15 weeks", "more than 24 weeks"))+
  OYSTERtheme+
    theme_bw()+
    theme(axis.text.x = element_text(angle=35),
          legend.title = element_blank()))

leftplot + rightplot