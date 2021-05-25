#Q24 & Q25 plot
#What has your supervisor/employer done well/poorly during Covid-19?
#Data in "Q24_25_Supervisors_wellVSbad.xlsx" (local copy)

Supervisor = factor(c(1,2,1,2,1,2,1,2,1,2),labels=c("Well", "Poorly"))
Keyword = factor(c(1,1,2,2,3,3,4,4,5,5),labels=c("Communication","Support","Flexibility", "Management","Availability"))
number_answers = c(148,85,59,41,45,26,41,24,46,41)
df=data.frame(Supervisor,Keyword,number_answers) 

library(tidyverse)


df%>%group_by(Supervisor)%>%
  mutate(perc=number_answers/sum(number_answers))%>%
  ggplot(., aes(x=Supervisor, y=-perc*100, fill=Keyword)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=number_answers), position = position_stack(vjust = 0.5))+
  scale_y_continuous(breaks = c(-100, -75, -50, -25, 0), labels= c("100", "75", "50", "25", "0"), expand = c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  scale_fill_brewer(palette="Accent")+
  labs(y="%", x="", title="What has your supervisor/employer done well/poorly during Covid-19?")+
  theme_minimal()+
  OYSTERtheme+
  theme(legend.position="top",
        legend.title = element_blank(),
        axis.text.x = element_text(colour = OYSTERcolor))


ggsave("Q2425.jpg", dpi=300, width = 7, height = 5.5)