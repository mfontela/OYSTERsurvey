#[DESCRIPTIVE STAT FIGURE FOR Q16]
# Q16	Has your funding been extended?
#   0	Yes
# 1	No
# 2	Don't know
D%>%
  mutate(Q16=as.character(Q16))%>%
  count(Q16, sort = TRUE)%>%
  mutate(percent=(n/sum(n))*100)%>%
  mutate(Q16 = replace(Q16, Q16 == 0, "Yes"))%>%
  mutate(Q16 = replace(Q16, Q16 == 1, "No"))%>%
  mutate(Q16 = replace(Q16, Q16 == 2, "Don't known"))%>%
  ggplot(., aes(x="", y=percent, fill=factor(Q16, levels=c("Yes","Don't known", "No"))))+
  geom_bar(stat = "identity", position="stack")+
  geom_text(aes(label=n), size=7, colour=OYSTERcolor, position = position_stack(vjust = 0.5))+
  labs(y="% Percentage",x="", title="Has your funding been extended?")+
  scale_fill_brewer(palette="Accent")+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  coord_flip()+
  OYSTERtheme+
  theme(legend.title = element_blank())

