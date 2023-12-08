library(shiny)
library(ggplot2)
# needs version 2023.9.12 version of dabestr
library(dabestr)
library(Rmisc)

function(input, output, session) {
    makeinfo1 <- reactive({
        #######################################
        # set up basic quantities for CI 
        civalueone <- as.numeric(input$cilevelone)
        cvtone <- qt((1-civalueone)/2, df=14, lower.tail=F)
        #cvtone
        
        pcivalue1 <- paste(civalueone*100,"% Confidence Interval\n Overlap of null value and\n Relationship to p-value")
        #pcivalue1
        
        ## work to establish means to produce exact pvals and other means
        t05 <- qt(.025, df=14, lower.tail=F)
        t01 <- qt(.005, df=14, lower.tail=F)
        #t05
        #t01
        # reverse engineer sample mean from t val
        stderrone <- 15/(15^.5)
        #stderrone
        
        mean05 <- 100+t05*stderrone
        #mean05
        mean01 <- 100+t01*stderrone
        #mean01
        return(list(civalueone=civalueone, cvtone=cvtone,pcivalue1=pcivalue1,
                    t05=t05,t0=t01, stderrone=stderrone))
    })
    makedata1 <-reactive({
        #######################################
        # establish the random sample of data
        # and scale to an exact mean and sd (100 and 15)
        # N is 15

        stderrone <- 15/(15^.5)
        t05 <- qt(.025, df=14, lower.tail=F)
        t01 <- qt(.005, df=14, lower.tail=F)
        
        set.seed(12222)
        d1 <- rnorm(15,mean=100, sd=15)
        #mean(d1)
        #sd(d1)
        sdratd1 <- (sd(d1))/15
        #sdratd1
        d1a <- d1/sdratd1
        #sd(d1a)
        diffd1 <- (mean(d1a))-100
        #diffd1
        d1b <- d1a-diffd1
        #mean(d1b)
        #sd(d1b)
        
        
        # create mean difference from null and put in data frame
        
        #nulldiff <- 12
        if (input$samplemeanval==1){
            nulldiff <- 12
        }
        if (input$samplemeanval==2){
            nulldiff <- 9
        }
        if (input$samplemeanval==3){
            nulldiff <- 5
        }
        if (input$samplemeanval==4){
            nulldiff<- t05*stderrone
        }
        if (input$samplemeanval==5){
            nulldiff<- t01*stderrone
        }
        samplemean <- round(100+nulldiff,2)
        psamplemean <- paste("Sample\n Mean =\n", samplemean)
        #psamplemean
        
        dv1 <- d1b+nulldiff
        Sample <- as.numeric(rep("1",times=15))
        dataone <- as.data.frame(cbind(Sample,dv1))

        return(list(dataone=dataone, samplemean=samplemean,psamplemean=psamplemean,
                    nulldiff=nulldiff))
    })


    output$plot1 <- renderPlot({
        infolist1 <- makeinfo1()
        civalueone <- infolist1$civalueone
        cvtone <-infolist1$cvtone
        pcivalue1 <- infolist1$pcivalue1
        stderrone <- infolist1$stderrone
        t05 <- infolist1$t05
        t01 <- infolist1$t01
        stderrone <- infolist1$stderrone
        dataonea <- makedata1()
        nulldiff <- dataonea$nulldiff
        dataone <- dataonea$dataone
        samplemean <- dataonea$samplemean
        psamplemean <- dataonea$psamplemean
        
        statstab <- Rmisc::summarySE(data=dataone, 
                                     measurevar="dv1", 
                                     groupvars="Sample", 
                                     conf.interval=civalueone)
        ### t-test
        onesampt <- t.test(dataone$dv1, mu=100)
        #onesampt
        #str(onesampt)
        pval1 <- round(onesampt$p.value,5)
        pval1b <- paste("Two-sided \n t-test p value =\n",pval1)
        #pval1b

        p <- ggplot(dataone, aes(x=Sample,y=dv1)) +
            geom_jitter(size=3, alpha=.2,
                        width=.01, height=.01) +
            scale_y_continuous(name="DV Value",limits=c(75,140), breaks=seq(0, 140, 10)) +
            scale_x_continuous(name="scale",limits=c(.6,1.4)) +
            geom_point(data=statstab, aes(y=dv1), stat="identity", size=4, colour="black") +
            geom_point(data=statstab, aes(y=dv1), stat="identity", size=3, colour="dodgerblue") +
            geom_hline(yintercept=100,linetype=2, colour="black", size=.7)+
            geom_errorbar(data=statstab, aes(ymin = dv1 - ci, ymax = dv1+ ci),
                          width = .07,size=.75,
                          # Width of the error bars
                          position = position_dodge(.9)) +
            theme_classic() + theme(text=element_text(size=12)) +
            ggtitle(pcivalue1)+
            theme(plot.title=element_text(hjust=0.5))+
            theme(aspect.ratio=4/4) +
            theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
                  plot.background = element_rect(
                      fill = "grey90",
                      colour = "black",
                      size = 1
                  ))
        #p
        if (input$descrone){
        p <- p +
             annotate(geom="text", x=.75, y=statstab[1,3]+2,
                      label=psamplemean,color="dodgerblue3", size=4.5) + 
             annotate(geom="text", x=1.25, y=statstab[1,3]+ statstab[1,6] + 7,
                      label=pval1b,color="dodgerblue3", size=4.5) +
             annotate(geom="text", x=.75, y=92,
                      label="null value = \n 100",color="dodgerblue3", size=4.5)
        }
        p
        
    })
    
    output$table1 <- renderTable({
        datalist <- makedata1()
        dataone <- datalist$dataone[,2]}
    )   


##############################################################################
# begin two-sample setup
    
    makeinfo2 <- reactive({
        #######################################
        # set up basic quantities for CI plots
        civalue <- as.numeric(input$cilevel1)
        #civalue <- .8304815 #for zero overlap with pval =.05
        cvt <- qt((1-civalue)/2, df=14, lower.tail=F)
        #cvt
        cvt2 <- qt((1-civalue)/2, df=28, lower.tail=F)
        #cvt2
        #cvt05 <- qt(.025, df=28, lower.tail=F)
        #cvt01 <- qt(.005, df=28, lower.tail=F)
        
        civalue2 <- paste(civalue*100,"% Confidence Intervals - Overlap and Relationship to p-values")
        #civalue2
        cistem2 <- cvt*(15/(15^.5)) # cv times sd/sqrtN
        #cistem2
        #cistem2*2
        
        #######################################
        sediff <- (15^2*(30/225))^.5
        #sediff
        threshdiff <- (cvt2*sediff) # cv times std err diff
        #threshdiff
        # establish sample mean difference values based on above
        meandiff3=11.21959 # this is for exactly p=.05
        meandiff6=15.13501 # this is for exactly p=.01
        meandiff4=16.61345 # just touching 95%CI bars, no overlap
        meandiff5 <- 8.306723 #95%ci bars just touch mean
        meandiff1 <- 10
        meandiff2 <- 15 # gives cohen's d of 1.0
        
        # establish diff for hedges g value of 1.0
        adjust1 <- 1/(1-(3/((4*28)-1)))
        meandiff_hedges1 <- adjust1*15
        #meandiff_hedges1
        meandiff7 <- meandiff_hedges1
        #hedgescorrected                      
        #meandiff/sediff  # gives observed t value
        
        ##############
        #set meandiff
        #meandiff <- meandiff5
        if (input$meandifference ==1){
            meandiff <-  meandiff1
        }
        if (input$meandifference ==2){
            meandiff <- meandiff2
        }
        if (input$meandifference ==3){
            meandiff <- meandiff3
        }
        if (input$meandifference ==4){
            meandiff <- meandiff4
        }
        if (input$meandifference ==5){
            meandiff <- meandiff5
        }
        if (input$meandifference ==6){
            meandiff <- meandiff6
        }
        if (input$meandifference ==7){
            meandiff <- meandiff7
        }
        
        # set values for pasting text on CI fig
        meandiffpaste <- round(meandiff,5)
        pmeandiff <- paste("Sample Mean Difference=\n", meandiffpaste)
        #pmeandiff
        cohend <- meandiff/15
        hedgescorrected <- cohend*(1-(3/((4*28)-1))) # hedgesg with correction
        cohend2 <- round(cohend,3)
        hedges2 <- round(hedgescorrected,3)
        pcohend <- paste("Cohen's d=", cohend2)
        phedgesg <- paste("Hedges g=", hedges2)
        
        return(list(meandiff=meandiff,
                    pmeandiff=pmeandiff,
                    pcohend=pcohend, phedgesg=phedgesg,
                    civalue=civalue,
                    cvt=cvt, cvt2=cvt2,
                    civalue2=civalue2,
                    cistem2=cistem2,
                    sediff=sediff))
    })

    makedata2 <- reactive({
        #######################################
        # establish the two random samples of data
        # and scale to have same sample mean and sd, initially
        # then create group mean differences according to choices above.
        infolist2 <- makeinfo2()
        sediff <- infolist2$sediff
        meandiff <- infolist2$meandiff
        cvt <- infolist2$cvt
        cvt2 <- infolist2$cvt2
        civalue <- infolist2$civalue

        
        
        set.seed(12222)
        g1 <- rnorm(15,mean=40, sd=15)
        #mean(g1)
        #sd(g1)
        sdrat1 <- sd(g1)/15
        #sdrat1
        g1a <- (g1)/sdrat1
        #sd(g1a)
        diff1 <- mean(g1a)-40
        #diff1
        g1b <- g1a-diff1
        #mean(g1b)
        #sd(g1b)
        
        g2 <- rnorm(15,mean=40, sd=15)
        #mean(g2)
        #sd(g2)
        sdrat2 <- sd(g2)/15
        #sdrat2
        g2a <- (g2)/sdrat2
        #sd(g2a)
        diff2 <- mean(g2a)-40
        #diff2
        g2b1 <- g2a-diff2
        g2b <- g2b1+meandiff
        #mean(g2b)
        #sd(g2b)
        
        ##############################################
        #  finish creating the data sets
        # establish the group vector for the CI plots
        grp1 <- rep("A",times=15)
        grp2 <- rep("B",times=15)
        grp3 <- rep("Difference",times=1)
        
        Group <- c(grp1,grp2,grp3)
        #Group
        
        
        # establish the DV vector for CI plots
        dv <- as.numeric(c(g1b,g2b, NA))
        #dv
        # 
        # establish the data frame for CI plots
        #data1 <- cbind(dv,Group)
        data1 <- data.frame(dv,Group)
        data1$Group <- as.factor(data1$Group)
        data1$dv <- as.numeric(data1$dv)
        #data1
        #str(data1)
        #data1 <- read.csv("data1.csv")
        ################################################
        
        # perform the independent samples t-test and extract the pvalue for use on the graph
        indept <- t.test(dv~Group,data=data1[1:30,], var.equal=T)
        #indept
        #str(indept)
        pval <- round(indept$p.value,5)
        pval2 <- paste("t-test p value =\n",pval)
        #pval2
        
        ################################################
        
        # create the summary data frame for use in ggplot2
        summ <- Rmisc::summarySE(data=data1, measurevar="dv", groupvars="Group", conf.interval=civalue)
        #summ
        #str(summ)
        # add in the values for the "third group", the difference value.
        summ[3,3] <- mean(g2b)
        #summ
        summ[3,6] <- cvt2*(meandiff/(abs(indept$statistic)))
        #summ
        
        ################################################
        # establish the Conf Interval half length and CI overlap percentage
        cistem <- summ[1,5]*cvt
        overlap1 <- round(((((mean(g1b)+cistem)-(mean(g2b)-cistem))/cistem))*100,4)
        overlap2 <- paste("CI overlap % =\n",overlap1)
        
        return(list(data1=data1,summ=summ, g1b=g1b,g2b=g2b,
                    cistem=cistem,
                    overlap2=overlap2,pval2=pval2))
    })

#    datalist3 <- reactive({makedata2()})
#    print(datalist3)
    
    output$table2 <- renderTable({
        datalist2 <- makedata2()
        data1 <- datalist2$data1[1:30,]}
    )   
    
    
    output$plot2 <- renderPlot({
        #read from earlier functions:
        infolist2 <- makeinfo2()
        sediff <- infolist2$sediff
        meandiff <- infolist2$meandiff
        cvt <- infolist2$cvt
        cvt2 <- infolist2$cvt2
        civalue <- infolist2$civalue
        civalue2 <- infolist2$civalue2
        pcohend <- infolist2$pcohend
        phedgesg <- infolist2$phedgesg
        pmeandiff <- infolist2$pmeandiff
        
        makedata2list <- makedata2()
        data1 <- makedata2list$data1
        summ <- makedata2list$summ
        g1b <- makedata2list$g1b
        g2b <- makedata2list$g2b
        cistem <- makedata2list$cistem
        overlap2 <- makedata2list$overlap2
        pval2 <- makedata2list$pval2
        
        #data2 <- data1[1:30,]
        #################################################
        # draw the graph
        p1 <- ggplot(data=summ, aes(x=Group,y=dv)) +
            scale_y_continuous(name= "Dependent Variable", limits=c(10,89),
                               sec.axis = sec_axis(trans=~.-mean(g1b),
                                                   name="Difference Scale")) +
            geom_point(size=4, colour="black") +
            geom_point(size=3, colour="dodgerblue") +
            #geom_hline(yintercept=0,linetype=2, colour="black", size=.7)+
            geom_vline(xintercept=2.5,linetype="solid", colour="darkgray", size=1)+
            geom_errorbar(aes(ymin = dv - ci, ymax = dv+ ci),
                          width = .1,size=.8,
                          # Width of the error bars
                          position = position_dodge(.9)) +
            geom_jitter(data=data1, aes(x=Group, y=dv), size=3, alpha=.2,
                        width=.05, height=0) +
            theme_classic() + theme(text=element_text(size=16)) +
            theme(axis.title.x = element_text(hjust=.33),
                  plot.background = element_rect(
                      fill = "grey90",
                      colour = "black",
                      size = 1
                  ))+
            ggtitle(civalue2)
        p1
        if (input$groupamean){
        p1 <- p1 +
            geom_hline(yintercept=40,linetype=3, colour="black", size=1)
        }
        if (input$edges){
        p1 <- p1 +
            geom_segment(aes(x=min(.4),xend=max(2.5),y=mean(g2b)-cistem,yend=mean(g2b)-cistem),
                                           linetype="dashed", colour="dodgerblue3", size=1) +
            geom_segment(aes(x=min(.4),xend=max(2.5),y=mean(g1b)+cistem, yend=mean(g1b)+cistem),
                         linetype="dashed", colour="dodgerblue3", size=1)
        }
        if (input$descr){
        p1 <- p1 +
            #geom_hline(yintercept=mean(g2b)-cistem,linetype="dashed", colour="dodgerblue3", size=1)+
            #geom_hline(yintercept=mean(g1b)+cistem,linetype="dashed", colour="dodgerblue3", size=1)+
            annotate(geom="text", x=3, y=summ[3,3]+ summ[3,6] +12,
                     label=pmeandiff,color="dodgerblue3", size=5) +
            annotate(geom="text", x=3, y=mean(g1b)-1.5*(summ[3,6]),
                     label=pcohend,color="dodgerblue3", size=5) +
            annotate(geom="text", x=3, y=mean(g1b)-2.25*(summ[3,6]),
                     label=phedgesg,color="dodgerblue3", size=5) +
            annotate(geom="text", x=1.4, y=(mean(g2b)+cistem + 13), label=pval2,
                     color="dodgerblue3", size=5) +
            annotate(geom="text", x=1.4, y=(mean(g1b)+cistem + 10), label=overlap2,
                     color="dodgerblue3", size=5)
        }
        p1
    }) # finish renderplot for plot2
    
    output$plot3 <- renderPlot({
        ##set up basics
        meandiff3ga=11.21959 # this is for exactly p=.05
        meandiff6ga=15.13501 # this is for exactly p=.01
        meandiff4ga=16.61345 # just touching 95%CI bars, no overlap
        meandiff5ga <- 8.306723 #95%ci bars just touch mean
        meandiff1ga <- 10
        meandiff2ga <- 15 # gives cohen's d of 1.0
        
        # establish diff for hedges g value of 1.0
        adjust1ga <- 1/(1-(3/((4*28)-1)))
        meandiff_hedges1ga <- adjust1ga*15
        #meandiff_hedges1
        meandiff7ga <- meandiff_hedges1ga
        #hedgescorrected                      
        #meandiff/sediff  # gives observed t value
        
        ##############
        #set meandiff


        if (input$meandifferencega ==1){
            meandiffga <-meandiff1ga
        }
        if (input$meandifferencega ==2){
            meandiffga <-meandiff2ga
        }
        if (input$meandifferencega ==3){
            meandiffga <-meandiff3ga
        }
        if (input$meandifferencega ==4){
            meandiffga <-meandiff4ga
        }
        if (input$meandifferencega ==5){
            meandiffga <-meandiff5ga
        }
        if (input$meandifferencega ==6){
            meandiffga <-meandiff6ga
        }
        if (input$meandifferencega ==7){
            meandiffga <- meandiff7ga
        }

        #######################################
        # establish the two random samples of data
        # and scale to have same sample mean and sd, initially
        # then create group mean differences according to choices above.
        
        set.seed(12222)
        g1ga <- rnorm(15,mean=40, sd=15)
        mean(g1ga)
        sdrat1ga <- sd(g1ga)/15
        g1aga <- (g1ga)/sdrat1ga
        diff1ga <- mean(g1aga)-40
        g1bga <- g1aga-diff1ga

        g2ga <- rnorm(15,mean=40, sd=15)
        sdrat2ga <- sd(g2ga)/15
        g2aga <- (g2ga)/sdrat2ga
        diff2ga <- mean(g2aga)-40
        g2b1ga <- g2aga-diff2ga
        g2bga <- g2b1ga+meandiffga

        ##############################################
        #  finish creating the data sets
        # establish the group vector for the GA plots
        grp1ga <- rep("A",times=15)
        grp2ga <- rep("B",times=15)

        
        group <- c(grp1ga,grp2ga)

        # establish the DV vector for CI plots
        DV <- as.numeric(c(g1bga,g2bga))
        DV
        
        # establish the data frame for the GA plots
        data2 <- as.data.frame(cbind(group,DV))
        data2$group <- as.factor(data2$group)
        data2$DV <- as.numeric(data2$DV)
### Attempted edit to update to the newer dabestr package
### Works but may need updated dplyr package too
### Left old code for dabestr version 0.3.0, commented.
### note that the load function is from dabestr, not base!!!
         # draw plots
        if (input$ptype==1){
        # two.group.unpaired1 <-
        #     dabest(.data=data2, group, DV,
        #            idx=c("A","B"),
        #            paired=FALSE) %>% mean_diff
        # plot(two.group.unpaired1, color.column = group)
          #print(names(data2))
          two.group.unpaired <-
            dabestr::load(data2, x=group, y=DV,
                 idx=c("A","B"))
          db1 <- mean_diff(two.group.unpaired)
          dabest_plot(db1,TRUE)
        }
        else if (input$ptype==2){
        # two.group.unpaired2 <-
        #     dabest(.data=data2, group, DV,
        #            idx=c("A","B"),
        #            paired=FALSE) %>% cohens_d
        # plot(two.group.unpaired2, color.column = group)
          two.group.unpaired <-
            dabestr::load(data2, x=group, y=DV,
                 idx=c("A","B"))
          db2 <- cohens_d(two.group.unpaired)
          dabest_plot(db2,TRUE)
        }
        else if (input$ptype==3){
        # two.group.unpaired3 <-
        #     dabest(.data=data2, group, DV,
        #            idx=c("A","B"),
        #            paired=FALSE) %>% hedges_g()
        # plot(two.group.unpaired3, color.column = group)
          two.group.unpaired <-
            dabestr::load(data2, x=group, y=DV,
                 idx=c("A","B"))
          db3 <- hedges_g(two.group.unpaired)
          dabest_plot(db3,TRUE)
        }
    }) # finisher renderplot for plot3
}

