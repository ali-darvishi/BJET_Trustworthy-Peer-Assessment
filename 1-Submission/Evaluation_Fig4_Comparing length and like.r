library(readr)
library(dplyr)
library(ggplot2)
library("cowplot")

user <- read_csv('Users.csv')
glimpse(user)

table(user$ group)

library(rlang)
myplot <- function(mydf, myxcol, myycol, ymin=0,ymax=1) {
   ggplot2::ggplot(data = mydf, aes(x=reorder({{ myxcol }}, 
      {{ myycol }}), y= {{ myycol }},col  = factor(group))) +
        geom_jitter(height = 0.1, width = 0.5,alpha = .08, shape = 16,aes(colour = factor(group)))+
        geom_violin(trim=TRUE,alpha=0.35, position = position_dodge(width = 1),colour=NA,aes( fill  = factor(group)))+
    geom_boxplot(notch = FALSE,width=.4,  outlier.size = -1, lwd=.6,outlier.shape = NA,fill = NA,
                 col=c('steelblue4','deeppink4'),lty=1)+ #,'',''
    ylim(ymin,ymax)+
    scale_x_discrete(limits=c('NP',"CP"),label=c('Control', 'Experiment' ))+ coord_flip()+ 
         stat_summary(fun.y=mean, geom="point", shape=18, size=4)+# color=c('orange4','darkolivegreen4','purple4','forestgreen'), fill=c('orange4','darkolivegreen4','purple4','forestgreen')) +
#     scale_colour_manual(limits=c('TR','AI',"NR", "SR",'SAI'),values = aes(colour = factor(Condition)))#c('darkkhaki','deeppink4','snow4','blue4','orchid4')) +
#     scale_fill_manual(limits=c('TR','AI',"NR", "SR",'SAI'),values = c('olivedrab4','maroon4','navajowhite4','steelblue4','violetred4')) +
#     scale_linetype_manual(limits=c('AI',"NR", "SR",'SAI'),values = c(1,1,1,1)) +
    theme(plot.title = element_text(hjust = 0.5),plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),axis.title.x=element_blank(), axis.title.y=element_blank(), 
    panel.grid=element_line(colour="lightgrey", size = (.05))  ,panel.background =element_blank() ,legend.position="none")

}

bx1=myplot(user, group, words,0,max(user$words))
bx1

pdf("words.pdf", width=4, height=3)
bx1
dev.off()


bx1=myplot(user, group, likeRate   ,0,max(user$likeRate   ))
bx1

pdf("likeRate.pdf", width=4, height=3)
bx1
dev.off()


bx1=myplot(user, group, Decision      ,min(user$Decision      ),max(user$Decision      ))
bx1

pdf("Decision.pdf", width=4, height=3)
bx1
dev.off()


bx1=myplot(user, group, Confidence       ,min(user$Confidence       ),max(user$Confidence       ))
bx1

pdf("Confidence.pdf", width=4, height=3)
bx1
dev.off()

