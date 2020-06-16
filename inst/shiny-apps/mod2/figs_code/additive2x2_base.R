ggplot(myData2, aes(x=factora, y=y, fill=factorb)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  labs(x= "Factor A", y= "Mean DV Score")+
  theme_classic() +
  theme(text = element_text(size=16)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_line(colour="black", size=.7),
        axis.line.x = element_line(colour="black", size=.7),
        plot.title = element_text(hjust=.5),
        legend.title=element_blank())+
  coord_cartesian(ylim=c(0, 40)) +  scale_y_continuous(expand = c(0,0)) +
  theme(legend.justification=c(0,0), legend.position=c(.05,.8)) +
  scale_fill_manual(values=c('slategray4','slategray3')) +
  geom_errorbar(aes(ymin=y-sem, ymax=y+sem), width=.2,
                position=position_dodge(.9))

