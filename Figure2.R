# Q19_1	Please choose one
# 0	Very negative impact
# 1	Fairly negative impact
# 2	No change
# 3	Fairly positive impacts
# 4	Very positive impact


D$Q19_1=NA
D$Q19_1[D$Q19_1_0==1]="Very negative"
D$Q19_1[D$Q19_1_1==1]="Fairly negative"
D$Q19_1[D$Q19_1_2==1]="No change"
D$Q19_1[D$Q19_1_3==1]="Fairly positive"
D$Q19_1[D$Q19_1_4==1]="Very positive"
D$Q19_1=as.factor(D$Q19_1)
D$Q19_1=factor(D$Q19_1, levels=c(
  "Very negative",
  "Fairly negative",
  "No change",
  "Fairly positive",
  "Very positive"))


d<-D %>% 
  count(stage, Q19_1) #dummy variable
  
d$perc=NA
d$perc[d$stage=="PhD student"]=d$n[d$stage=="PhD student"] / sum(d$n[d$stage=="PhD student"])
d$perc[d$stage=="Postdoctoral researcher"]=d$n [d$stage=="Postdoctoral researcher"] / sum(d$n[d$stage=="Postdoctoral researcher"])
d$perc[d$stage=="Research assistant/Researcher/other ECR"]=d$n[d$stage=="Research assistant/Researcher/other ECR"] / sum(d$n[d$stage=="Research assistant/Researcher/other ECR"])

print(
  ggplot(d, aes(x = Q19_1, y = perc, fill=stage)) +
    geom_point(aes(colour=stage, shape = stage), size=5)+
    OYSTERtheme+
  scale_fill_manual(values=OYSTERpalette, name="Career stage")+
    scale_colour_manual(values=OYSTERpalette, name="Career stage")+
    labs(y="Respondents by career stage group (%)", 
         x="Home-working effect", shape="Career stage")+
    scale_y_continuous(breaks=seq(0,0.5,0.1), labels = c("0", "10", "20", "30", "40", "50"))+
    theme(axis.text.x = element_text(angle=35, vjust = 0.7, hjust=0.6))+
    theme(axis.line = element_line(colour=OYSTERcolor, size=1.1))
)

  