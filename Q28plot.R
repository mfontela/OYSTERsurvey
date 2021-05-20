# [FIGURE FOR Q28]
# Q28	Overall, how would you rate the effect of Covid-19 and the related measures on your career in the long term?
# Q28_1	Please choose one
# 0	Very negative
# 1	Somewhat negative
# 2	No effect
# 3	Somewhat positive
# 4	Very positive
# Q28_a	Why?

#Somehow the Q28 has a different coding than others... 
D%>%
  mutate(Q28=NA)%>% #preallocate
  mutate(Q28 = ifelse(Q28_1_0 == 1, "Very negative", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_1 == 1, "Somewhat negative", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_2== 1, "No effect", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_3 == 1, "Somewhat positive", Q28))%>%
  mutate(Q28 = ifelse(Q28_1_4 == 1, "Very positive", Q28))%>%
  mutate(Q28=factor(Q28, levels = c("Very negative","Somewhat negative", "No effect", "Somewhat positive", "Very positive")))%>%
  count(Q28)%>%
  mutate(percent=(n/sum(n))*100)%>%
  ggplot(., aes(x="", y=-percent, fill=Q28))+
  geom_bar(stat = "identity", position="stack")+
  ggrepel::geom_text_repel(aes(label=n), size=4, colour="black",position = position_stack(vjust = 0.5))+
  # geom_text(aes(label=n), size=4, colour="black", position = position_stack(vjust = 0.5))+
  labs(y="% Percentage",x="", title="how would you rate the effect of Covid-19 and the related measures on your career in the long term?")+
  scale_fill_brewer(type = "div", palette = "RdBu")+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), breaks = c(-100, -75, -50, -25, 0), labels = c(breaks = c(0, 25, 50, 75, 100)))+
  coord_flip()+
  OYSTERtheme+
  theme(legend.title = element_blank(),
        plot.title = element_text(size=11),
        legend.text = element_text(size=12))
