# [FIGURE FOR Q20]
# Q20	Would you prefer to work from home more often than you did before Covid-19 measures were introduced?
# 0	Yes, sometimes
# 1	Yes, part time
# 2	Yes, full time
# 3	No

D%>%
  mutate(Q20=as.character(Q20))%>%
  count(Q20)%>%
  mutate(percent=(n/sum(n))*100)%>%
  mutate(Q20 = replace(Q20, Q20 == 0, "Yes, sometimes"))%>%
  mutate(Q20 = replace(Q20, Q20 == 1, "Yes, part time"))%>%
  mutate(Q20 = replace(Q20, Q20 == 2, "Yes, full time"))%>%
  mutate(Q20 = replace(Q20, Q20 == 3, "No"))%>%
  mutate(Q20=as.factor(Q20))%>%
  mutate(Q20=factor(Q20, levels = c("No","Yes, sometimes", "Yes, part time", "Yes, full time")))%>%
  ggplot(., aes(x="", y=-percent, fill=Q20))+
  geom_bar(stat = "identity", position="stack")+
  geom_text(aes(label=n), size=4, colour="white", position = position_stack(vjust = 0.5))+
  labs(y="% Percentage",x="", title="Would you prefer to work from home more often than you did before Covid-19?")+
  scale_fill_manual(values=c("firebrick1", "darkgreen", "limegreen", "darkolivegreen3"))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), breaks = c(-100, -75, -50, -25, 0), labels = c(breaks = c(0, 25, 50, 75, 100)))+
  coord_flip()+
  OYSTERtheme+
  theme(legend.title = element_blank(),
        plot.title = element_text(size=11),
        legend.text = element_text(size=12))
