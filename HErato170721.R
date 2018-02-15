#Briner, binary color choice (butterflies)
#Last update: 170720
#Data: H erato cohorts 2, 3, 4, 5, 6.
  #H erato Cohort2 (380+), Cohort3 (380+) Cohort4 (390+), Cohort5 (390+), Cohort6 (390+), Cohort7 160823 (390+)
#-------------------------------------------------------------------------------------



rm(list=ls()) # clears workspace
setwd("~/Dropbox/School/UCI/Briscoe/Experiments/Behavior/ColorDiscrimination/UV/BehaviorData/GoodCohorts_HerHdor_clean") #file path for home desktop
setwd("~/Dropbox/School/UCI/Briscoe/Projects/Wolbachia_Herato/Behavior/BehaviorData")

#load in behavior data
  Herato380 <-read.csv("Herato380data.csv", header=TRUE) 
  Herato390 <-read.csv("Herato390data.csv", header=TRUE) 
    as.character(Herato380$LightSetting) 
    as.character(Herato390$LightSetting)

#Optional: take a look
  head(Herato380)
  head(Herato390)
    summary(Herato380)
    summary(Herato390)
      
#-----------------------------------------------                        
#Subsets
#----------------------------------------------- 
    
  ##subsetting by sex
          #380+ 
            Herato380_fem <-Herato380[ which(Herato380$Sex=="female"), ]
            Herato380_male <-Herato380[ which(Herato380$Sex=="male"), ]
          #390+ 
            Herato390_fem <-Herato390[ which(Herato390$Sex=="female"), ]
            Herato390_male <-Herato390[ which(Herato390$Sex=="male"), ]
          
  ##subsetting by light setting within sex
        #380+ fem, H erato
          Herato380_fem_hard <-Herato380_fem[ which(Herato380_fem$LightSetting=="a 1:15 (hard)"), ]
          Herato380_fem_equal <-Herato380_fem[ which(Herato380_fem$LightSetting=="b 1:1 (equal)"), ]
          Herato380_fem_easy <-Herato380_fem[ which(Herato380_fem$LightSetting=="c 15:1 (easy)"), ]
        #380+ male, H erato
          Herato380_male_hard <-Herato380_male[ which(Herato380_male$LightSetting=="a 1:15 (hard)"), ]
          Herato380_male_equal <-Herato380_male[ which(Herato380_male$LightSetting=="b 1:1 (equal)"), ]
          Herato380_male_easy <-Herato380_male[ which(Herato380_male$LightSetting=="c 15:1 (easy)"), ]
        #390+ fem, H erato
          Herato390_fem_hard <-Herato390_fem[ which(Herato390_fem$LightSetting=="a 1:15 (hard)"), ]
          Herato390_fem_equal <-Herato390_fem[ which(Herato390_fem$LightSetting=="b 1:1 (equal)"), ]
          Herato390_fem_easy <-Herato390_fem[ which(Herato390_fem$LightSetting=="c 15:1 (easy)"), ]
        #390+ male, H erato
          Herato390_male_hard <-Herato390_male[ which(Herato390_male$LightSetting=="a 1:15 (hard)"), ]
          Herato390_male_equal <-Herato390_male[ which(Herato390_male$LightSetting=="b 1:1 (equal)"), ]
          Herato390_male_easy <-Herato390_male[ which(Herato390_male$LightSetting=="c 15:1 (easy)"), ]

          
#-----------------------------------------------                        
#Flexible subtitles (sample output: "N = 12 ,    no. choices:    1:15 hard = 138     1:1 equal = 174     15:1 easy = 126")
#-----------------------------------------------              

      #What's my N? (Run subsets first)
          N_Herato380_fem <-length( unique(Herato380_fem$Individual) ) #380+ fem, H erato
          N_Herato380_male <-length( unique(Herato380_male$Individual) ) #380+ male, H erato
          N_Herato390_fem <-length( unique(Herato390_fem$Individual) ) #390+ fem, H erato
          N_Herato390_male <-length( unique(Herato390_male$Individual) ) #390+ male, H erato
          
      #And what are my counts within each sex for each category of light choice?
            #380+ fem
            N_Her_380Fhard <-sum(Herato380_fem_hard$total)
            N_Her_380Fequal <-sum(Herato380_fem_equal$total)
            N_Her_380Feasy <-sum(Herato380_fem_easy$total)
            #380+ male
            N_Her_380Mhard <-sum(Herato380_male_hard$total)
            N_Her_380Mequal <-sum(Herato380_male_equal$total)
            N_Her_380Measy <-sum(Herato380_male_easy$total)
            #390+ fem
            N_Her_390Fhard <-sum(Herato390_fem_hard$total)
            N_Her_390Fequal <-sum(Herato390_fem_equal$total)
            N_Her_390Feasy <-sum(Herato390_fem_easy$total)
            #390+ male
            N_Her_390Mhard <-sum(Herato390_male_hard$total)
            N_Her_390Mequal <-sum(Herato390_male_equal$total)
            N_Her_390Measy <-sum(Herato390_male_easy$total)

            
#Subtitles
    sub_Her_380F <-paste("N =", N_Herato380_fem, ",    no. choices:    1:15 hard =", N_Her_380Fhard,  "    1:1 equal =", N_Her_380Fequal, "    15:1 easy =", N_Her_380Feasy)
    sub_Her_380M <-paste("N =", N_Herato380_male, ",    no. choices:    1:15 hard =", N_Her_380Mhard,  "    1:1 equal =", N_Her_380Mequal, "    15:1 easy =", N_Her_380Measy)
    sub_Her_390F <-paste("N =", N_Herato390_fem, ",    no. choices:    1:15 hard =", N_Her_390Fhard,  "    1:1 equal =", N_Her_390Fequal, "    15:1 easy =", N_Her_390Feasy)
    sub_Her_390M <-paste("N =", N_Herato390_male, ",    no. choices:    1:15 hard =", N_Her_390Mhard,  "    1:1 equal =", N_Her_390Mequal, "    15:1 easy =", N_Her_390Measy)

   
     
#Plots-------------------------------------------------------------------------------------------------------
    
#-------------------------------------------------------------------------                        
# 1.    #boxplots, choices pooled by 1) sex and 2) light setting
#-------------------------------------------------------------------------  
    #Note: "Error in grid.Call" means expand the plot box.
    
        library(plyr)
        library(ggplot2)                      
        library(scales)   
    
    #H erato, 380+ 
    #-----
    dev.new(width=unit(c(40), "cm"), height=unit(c(10), "cm"))
    ggplot(Herato380, aes(x = LightSetting, y = proportioncorr, fill = Sex)) + geom_boxplot() + ylim(0,1) + 
      labs(x="\nLight Setting", y="Choice: proportion correct\n") + 
      ggtitle(expression(atop("H. erato 380+", atop(italic("\npooled choice by sex"), "")))) +
      scale_x_discrete(labels=c("1:15 (hard)", "1:1 (equal)", "15:1 (easy) ")) +
      
      theme_bw() + theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), 
                         axis.line = element_line(colour = "black")) +
    
      theme(plot.title = element_text(color="#666666", face="bold", size=32)) +
      theme(axis.title = element_text(color="#666666", face="bold", size=22)) +
      theme(axis.text.x = element_text(face="bold", color="#993333", size=16),
            axis.text.y = element_text(face="bold", color="#993333", size=16, angle=45) ) +
      theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    
      geom_hline(yintercept = 0.5, colour="grey", linetype = "longdash") 
   
                sub_Her_380F
                sub_Her_380M
            
                    
                #H erato, 390+ 
                #-----
                  dev.new(width=unit(c(40), "cm"), height=unit(c(10), "cm"))
                  ggplot(Herato390, aes(x = LightSetting, y = proportioncorr, fill = Sex)) + geom_boxplot() + ylim(0,1) + 
                    labs(x="\nLight Setting", y="Choice: proportion correct\n") + 
                    ggtitle(expression(atop("H. erato 390+", atop(italic("\npooled choice by sex"), "")))) +
                    scale_x_discrete(labels=c("1:15 (hard)", "15:1 (easy)")) +
                    
                    theme_bw() + theme(panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(), 
                                       axis.line = element_line(colour = "black")) +
                    
                    theme(plot.title = element_text(color="#666666", face="bold", size=32)) +
                    theme(axis.title = element_text(color="#666666", face="bold", size=22)) +
                    theme(axis.text.x = element_text(face="bold", color="#993333", size=16),
                          axis.text.y = element_text(face="bold", color="#993333", size=16, angle=45) ) +
                    theme(plot.margin = unit(c(1,1,1,1), "cm")) +
                    
                    geom_hline(yintercept = 0.5, colour="grey", linetype = "longdash") 
      
                    sub_Her_390F
                    sub_Her_390M

                  
                  
#-------------------------------------------------------------------------                        
# 2.    #error plot with within-sex averages, choices pooled by 1) sex and 2) light setting
#------------------------------------------------------------------------- 
                    
                #First run this pre-made script for summarySE:
                    
                    # summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
                    ## Summarizes data.
                    ## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
                    ##   data: a data frame.
                    ##   measurevar: the name of a column that contains the variable to be summariezed
                    ##   groupvars: a vector containing names of columns that contain grouping variables
                    ##   na.rm: a boolean that indicates whether to ignore NA's
                    ##   conf.interval: the percent range of the confidence interval (default is 95%)
                    
                    summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                                          conf.interval=.95, .drop=TRUE) {
                      library(plyr)
                      
                      # New version of length which can handle NA's: if na.rm==T, don't count them
                      length2 <- function (x, na.rm=FALSE) {
                        if (na.rm) sum(!is.na(x))
                        else       length(x)
                      }
                      
                      # This does the summary. For each group's data frame, return a vector with
                      # N, mean, and sd
                      datac <- ddply(data, groupvars, .drop=.drop,
                                     .fun = function(xx, col) {
                                       c(N    = length2(xx[[col]], na.rm=na.rm),
                                         mean = mean   (xx[[col]], na.rm=na.rm),
                                         sd   = sd     (xx[[col]], na.rm=na.rm)
                                       )
                                     },
                                     measurevar
                      )
                      
                      # Rename the "mean" column    
                      datac <- rename(datac, c("mean" = measurevar))
                      
                      datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
                      
                      # Confidence interval multiplier for standard error
                      # Calculate t-statistic for confidence interval: 
                      # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
                      ciMult <- qt(conf.interval/2 + .5, datac$N-1)
                      datac$ci <- datac$se * ciMult
                      
                      return(datac)
                    }
  

#My errorplots:
#---------------------   
                    
#H erato, 380+ 
#----- 
                    
Her380_summary <- summarySE(Herato380, measurevar="proportioncorr", groupvars=c("Sex","LightSetting"))
            
  dev.new(width=unit(c(40), "cm"), height=unit(c(10), "cm")) #new outside plot window
  pd <- position_dodge(0.5) # # The errorbars overlapped, so use position_dodge to move them horizontally: move them .05 to the left and right
  
  # Standard error of the mean / 95% confidence interval (change se to ci for 95% confidence interval)
  ggplot(data=Her380_summary, aes(x = LightSetting, y = proportioncorr, colour=Sex)) + 
    scale_colour_manual(values = c("female" = "#CC0066", "male" = "#0000CC")) +
  
    geom_errorbar(aes(ymin=proportioncorr-ci, ymax=proportioncorr+ci), width=.2, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=4) +
    
    ylim(0,1) +
    labs(x="\nLight Setting", y="Choice: proportion correct\n") + 
    ggtitle(expression(atop("H. erato 380+", atop(italic("\npooled choice by sex"), "")))) +
    scale_x_discrete(labels=c("1:15 (hard)", "1:1 (equal)", "15:1 (easy) ")) +
    
    stat_summary(aes(group = Sex), geom = "line", fun.y = mean, position=pd, size = 1) +
    
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), 
                       axis.line = element_line(colour = "black")) +
    
    theme(plot.title = element_text(color="#666666", face="bold", size=32)) +
    theme(axis.title = element_text(color="#666666", face="bold", size=22)) +
    theme(axis.text.x = element_text(face="bold", color="#993333", size=16),
          axis.text.y = element_text(face="bold", color="#993333", size=16, angle=45) ) +
    theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    
    geom_hline(yintercept = 0.5, colour="grey", linetype = "longdash") 
  
  sub_Her_380F #"N = 8 ,    no. choices:    1:15 hard = 133     1:1 equal = 123     15:1 easy = 98"
  sub_Her_380M #"N = 2 ,    no. choices:    1:15 hard = 35     1:1 equal = 28     15:1 easy = 24"                         

#Her 390
#---------
Her390_summary <- summarySE(Herato390, measurevar="proportioncorr", groupvars=c("Sex","LightSetting"))
  
  ggplot(data=Her390_summary, aes(x = LightSetting, y = proportioncorr, colour=Sex)) + 
    scale_colour_manual(values = c("female" = "#CC0066", "male" = "#0000CC")) +
    
    geom_errorbar(aes(ymin=proportioncorr-ci, ymax=proportioncorr+ci), width=.2, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=4) +
    
    ylim(0,1) +
    labs(x="\nLight Setting", y="Choice: proportion correct\n") + 
    ggtitle(expression(atop("H. erato 390+", atop(italic("\npooled choice by sex"), "")))) +
    scale_x_discrete(labels=c("1:15 (hard)", "15:1 (easy)")) +
    
    stat_summary(aes(group = Sex), geom = "line", fun.y = mean, position=pd, size = 1) +
    
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), 
                       axis.line = element_line(colour = "black")) +
    
    theme(plot.title = element_text(color="#666666", face="bold", size=32)) +
    theme(axis.title = element_text(color="#666666", face="bold", size=22)) +
    theme(axis.text.x = element_text(face="bold", color="#993333", size=16),
          axis.text.y = element_text(face="bold", color="#993333", size=16, angle=45) ) +
    theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    
    geom_hline(yintercept = 0.5, colour="grey", linetype = "longdash") 
  
  sub_Her_390F #"N = 10 ,    no. choices:    1:15 hard = 117     1:1 equal = 115     15:1 easy = 110"
  sub_Her_390M #"N = 11 ,    no. choices:    1:15 hard = 143     1:1 equal = 136     15:1 easy = 118"
  
                  
                  
#-----------------------------------------------                        
#3.  spaghetti plots: individual choice, pooled by light setting
#-----------------------------------------------   
require(ggplot2)

#change Individual and Sex to factor variables
      Herato380_test <- within(Herato380, {
        Individual <-factor(Individual)
        Sex <- factor(Sex, levels = c("female", "male"), labels = c("female", "male"))
      })
      
      #define base for the graphs and store in the object Hdoris380_spag
      Herato380_spag <- ggplot(data= Herato380_test, aes(x = Herato380$LightSetting, y = Herato380$proportioncorr, group = Herato380$Individual)) 
       

      #Add a line using locally weighted regression (lowess) to "smooth" over all the variability and give a sense of the overall or average trend.
      
      Herato380_spag + ylim(0,1) + 
        
        aes(colour = factor(Sex)) + geom_point() + guides(colour=FALSE) +
        
        stat_smooth(aes(group = 1), se = FALSE) + 
        stat_summary(aes(group = 1), geom = "line", fun.y = mean, size = 1) + 
        
        geom_hline(yintercept = 0.5, colour="grey", linetype = "longdash") +
        
        theme_bw() + theme(panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), 
                           axis.line = element_line(colour = "black")) +
      
      ggtitle(expression(atop("H. erato 380+", atop("\nindividual choice by sex", "")))) 
          
      
      
      
      
      
      
      
#Herato390-----------------------------------

      #+++++++++++++++++++++++++
      # Function to calculate the mean and the standard deviation
      # for each group
      #+++++++++++++++++++++++++
      # data : a data frame
      # varname : the name of a column containing the variable
      #to be summariezed
      # groupnames : vector of column names to be used as
      # grouping variables
      
      data_summary <- function(data, varname, groupnames){
        require(plyr)
        summary_func <- function(x, col){
          c(mean = mean(x[[col]], na.rm=TRUE),
            sd = sd(x[[col]], na.rm=TRUE))
        }
        data_sum <-ddply(data, groupnames, .fun=summary_func,
                        varname)
        data_sum <- rename(data_sum, c("mean" = varname))
        return(data_sum)
      }  
      
      df2 <- data_summary(ToothGrowth, varname="len", 
                          groupnames=c("supp", "dose"))
      # Convert dose to a factor variable
      df2$dose=as.factor(df2$dose)
      head(df2)
      
# ---------------      
      
            Herato390_test <- within(Herato390, {
        Individual <-factor(Individual)
        Sex <- factor(Sex, levels = c("female"), labels = c("female"))
      })
      
      #define the base for the graphs and store in the object "species3x0_spag"
      Herato390_spag <- ggplot(data= Herato390_test, 
                               aes(x = Herato390$LightSetting, y = Herato390$proportioncorr, group = Herato390$Individual))
      
      #Add a line using locally weighted regression (lowess) to "smooth" over all the variability and give a sense of the overall or average trend.
      
      Herato390_spag + ylim(0,1) + 
        
        aes(colour = factor(Individual)) + geom_line(size=1) + geom_point(size=3) + guides(colour=FALSE) +
        
        geom_hline(yintercept = 0.5, colour="grey", linetype = "longdash") +
        
        facet_grid(. ~ Sex) +
        
        #geom_errorbar(aes(ymin=Herato390$proportioncorr-sd, ymax=Herato390$proportioncorrn+sd), width=.2, position=position_dodge(.9)) +
        
        theme_bw() + theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
      
        ggtitle(expression(atop("H. erato 390+", atop("\nindividual choice by sex", "")))) 
       
      
#------------------------
#Old spaghetti plots with base graphics
#------------------------
      
      
      ##H erato ------- 380+ and 390+
      Spag_Her380fem <-interaction.plot(Herato380_fem$LightSetting, Herato380_fem$Individual, Herato380_fem$proportioncorr, 
                                        type="l", lwd=3, 
                                        main="H. erato 380+ females: individual choice", 
                                        sub=sub_Her_380F, col.sub="blue",
                                        xlab="Light setting", ylab="Proportion correct choice", ylim=c(0,1))
      Spag_Her380male <-interaction.plot(Herato380_male$LightSetting, Herato380_male$Individual, Herato380_male$proportioncorr, 
                                         type="l", lwd=3, 
                                         main="H. erato 380+ males: individual choice", 
                                         sub=sub_Her_380M, col.sub="blue",
                                         xlab="Light setting", ylab="Proportion correct choice", ylim=c(0,1))
      Spag_Her390fem <-interaction.plot(Herato390_fem$LightSetting, Herato390_fem$Individual, Herato390_fem$proportioncorr, 
                                        type="l", lwd=3, 
                                        main="H. erato 390+ females: individual choice", 
                                        sub=sub_Her_390F, col.sub="blue",
                                        xlab="Light setting", ylab="Proportion correct choice", ylim=c(0,1))
      Spag_Her390male <-interaction.plot(Herato390_male$LightSetting, Herato390_male$Individual, Herato390_male$proportioncorr, 
                                         type="l", lwd=3, 
                                         main="H. erato 390+ males: individual choice", 
                                         sub=sub_Her_390M, col.sub="blue",
                                         xlab="Light setting", ylab="Proportion correct choice", ylim=c(0,1))
      
      
#------------------------
#Data analysis 
#------------------------
      
     
  #1) Non-parametric variance tests ----------------   USE THIS ONE
      
      #Performs the Ansari-Bradley two-sample test - rank based (nonparametric) two-sample test for difference in scale. Does not assume normality.
        #Sex comparison: 380 nm (all non-significant)
          ansari.test(Herato380_fem_hard$proportioncorr, Herato380_male_hard$proportioncorr, alternative = c("two.sided")) 
          ansari.test(Herato380_fem_equal$proportioncorr, Herato380_male_equal$proportioncorr, alternative = c("two.sided")) 
          ansari.test(Herato380_fem_easy$proportioncorr, Herato380_male_easy$proportioncorr, alternative = c("two.sided")) 
        #Sex comparison: 390 nm (all non-significant)
          ansari.test(Herato390_fem_hard$proportioncorr, Herato390_male_hard$proportioncorr, alternative = c("two.sided"))   
          ansari.test(Herato390_fem_equal$proportioncorr, Herato390_male_equal$proportioncorr, alternative = c("two.sided"))   
          ansari.test(Herato390_fem_easy$proportioncorr, Herato390_male_easy$proportioncorr, alternative = c("two.sided"))    
          
      #Performs Mood's two-sample test for a difference in scale parameters.
        #Sex comparison: 380 nm (all non-significant)
          mood.test(Herato380_fem_hard$proportioncorr, Herato380_male_hard$proportioncorr, alternative = c("two.sided")) 
          mood.test(Herato380_fem_equal$proportioncorr, Herato380_male_equal$proportioncorr, alternative = c("two.sided"))
          mood.test(Herato380_fem_easy$proportioncorr, Herato380_male_easy$proportioncorr, alternative = c("two.sided")) 
        #Sex comparison: 390 nm (all non-significant)
          mood.test(Herato390_fem_hard$proportioncorr, Herato390_male_hard$proportioncorr, alternative = c("two.sided"))   
          mood.test(Herato390_fem_equal$proportioncorr, Herato390_male_equal$proportioncorr, alternative = c("two.sided"))   
          mood.test(Herato390_fem_easy$proportioncorr, Herato390_male_easy$proportioncorr, alternative = c("two.sided"))   
        
   
  #2) Non-parametric mean comparison
      
    #The *Wilcoxon-Matt-Whitney* test (or Wilcoxon rank sum test, or Mann-Whitney U-test) is used when is asked to compare 
      #the means of two groups that do not follow a normal distribution: it is a non-parametrical test. 
      #It is the equivalent of the t test, applied for independent samples.
      
      #Sex comparison: 380 nm (all significant)
        wilcox.test(Herato380_fem_hard$proportioncorr, Herato380_male_hard$proportioncorr, alternative = c("greater"), correct=FALSE)   #W = 15, p-value = 0.04444
        wilcox.test(Herato380_fem_equal$proportioncorr, Herato380_male_equal$proportioncorr, alternative = c("greater"), correct=FALSE)   #W = 16, p-value = 0.02222
        wilcox.test(Herato380_fem_easy$proportioncorr, Herato380_male_easy$proportioncorr, alternative = c("greater"), correct=FALSE)     #W = 16, p-value = 0.01667
      #Sex comparison: 390 nm (all significant)
        wilcox.test(Herato390_fem_hard$proportioncorr, Herato390_male_hard$proportioncorr, alternative = c("greater"), correct=FALSE)   #W = 103, p-value = 0.0003566
        wilcox.test(Herato390_fem_equal$proportioncorr, Herato390_male_equal$proportioncorr, alternative = c("greater"), correct=FALSE)   #W = 86, p-value = 0.01431
        wilcox.test(Herato390_fem_easy$proportioncorr, Herato390_male_easy$proportioncorr, alternative = c("greater"), correct=FALSE)     #W = 77.5, p-value = 0.05591
        
        # alternative hypothesis: true location shift is not equal to 0
        
        
            
#Two independent samples *t-test* to compare the means of a normally distributed interval dependent variable for two independent groups.
      
      #0) Generate box plot for each group
        #^ See boxplots
        
      #1) Test for normality  in each group
      
        #Perform shapiro wilk normality test for each category
        aggregate(proportioncorr~LightSetting, data=Herato380_fem, FUN = function(x) shapiro.test(x)$p.value) #380nm p all non-sig
        aggregate(proportioncorr~LightSetting, data=Herato390_fem, FUN = function(x) shapiro.test(x)$p.value) #390nm p all non-sig
        aggregate(proportioncorr~LightSetting, data=Herato380_male, FUN = function(x) shapiro.test(x)$p.value) #Error: sample size too small
        aggregate(proportioncorr~LightSetting, data=Herato390_male, FUN = function(x) shapiro.test(x)$p.value) #390nm p all non-sig
          #p > 0.05 = "I cannot reject the hypothesis that the sample comes from a population which has a normal distribution".
        
                  #Visually.   QQPlot: If the plotted value vary more from a straight line, then the data is not normally distributed. 
                      qqnorm(Herato380_fem_hard$proportioncorr)
                      qqnorm(Herato380_fem_equal$proportioncorr)
                      qqnorm(Herato380_fem_easy$proportioncorr)
                        qqnorm(Herato380_male_hard$proportioncorr)
                        qqnorm(Herato380_male_equal$proportioncorr)
                        qqnorm(Herato380_male_easy$proportioncorr)
                      qqnorm(Herato390_fem_hard$proportioncorr)
                      qqnorm(Herato390_fem_equal$proportioncorr)
                      qqnorm(Herato390_fem_easy$proportioncorr)
                        qqnorm(Herato390_male_hard$proportioncorr)
                        qqnorm(Herato390_male_equal$proportioncorr)
                        qqnorm(Herato390_male_easy$proportioncorr)
                            #do not appear to be normally distributed. See non-parametric tests ^
                
      #2) Variance
      
      #Perform a Levene test for equality of variances in the two groups. Levene's test is an inferential statistic used to assess the equality of variances for a variable calculated for two or more groups.
              library(car) ##Load the car package that contains the levene test
                leveneTest(proportioncorr~LightSetting, data=Herato380_fem, center = mean)
                leveneTest(proportioncorr~LightSetting, data=Herato390_fem, center = mean)
                leveneTest(proportioncorr~LightSetting, data=Herato380_male, center = mean)
                leveneTest(proportioncorr~LightSetting, data=Herato390_male, center = mean)
                  #This test should not be significant to meet the assumption of equality of variances
      
        #Perform an F test to compare the variances of two samples from NORMAL populations.
                #Sex comparison: 380 nm (all non-significant)
                  var.test(Herato380_fem_hard$proportioncorr, Herato380_male_hard$proportioncorr, alternative = c("two.sided")) #F = 0.71242, num df = 11, denom df = 9, p-value = 0.5868
                  var.test(Herato380_fem_equal$proportioncorr, Herato380_male_equal$proportioncorr, alternative = c("two.sided")) #F = 1.2081, num df = 11, denom df = 9, p-value = 0.7891
                  var.test(Herato380_fem_easy$proportioncorr, Herato380_male_easy$proportioncorr, alternative = c("two.sided")) #F = 0.53981, num df = 11, denom df = 9, p-value = 0.3322
                #Sex comparison: 390 nm (1:15 is significant)
                  var.test(Herato390_fem_hard$proportioncorr, Herato390_male_hard$proportioncorr, alternative = c("two.sided"))   #F = 3.9999, num df = 10, denom df = 10, p-value = 0.03917
                  var.test(Herato390_fem_equal$proportioncorr, Herato390_male_equal$proportioncorr, alternative = c("two.sided"))   #F = 0.88683, num df = 10, denom df = 10, p-value = 0.8531
                  var.test(Herato390_fem_easy$proportioncorr, Herato390_male_easy$proportioncorr, alternative = c("two.sided"))     #F = 0.77233, num df = 10, denom df = 10, p-value = 0.6907

                                              #3) SKIP. Apply a log transformation to stabilize data variance (if the variance in the two groups is not equal, transform the data to remedy this)
                                                #Take the log of the data to stabilize variance
                                                
                                              #4) SKIP. Perform a t test on the TRANSFORMED variable (assuming EQUAL variance)
      
      #5) Perform a t test on the UNtransformed variable (assuming equal variance). See ^ *Wilcoxon-Matt-Whitney* test for non-parametric equivalent. 
      
      #Assuming EQUAL variances     
        #Sex comparison: 380 nm (all non-significant)
          t.test(Herato380_fem_hard$proportioncorr, Herato380_male_hard$proportioncorr, alternative = c("two.sided"), var.equal = TRUE)  #t = 0.87814, df = 20, p-value = 0.3903
          t.test(Herato380_fem_equal$proportioncorr, Herato380_male_equal$proportioncorr, alternative = c("two.sided"), var.equal = TRUE)  #t = -0.042034, df = 20, p-value = 0.9669
          t.test(Herato380_fem_easy$proportioncorr, Herato380_male_easy$proportioncorr, alternative = c("two.sided"), var.equal = TRUE)  #t = 1.4839, df = 20, p-value = 0.1534
        #Sex comparison: 390 nm (all non-significant)
          t.test(Herato390_fem_hard$proportioncorr, Herato390_male_hard$proportioncorr, alternative = c("two.sided"), var.equal = TRUE)  #t = -0.69977, df = 20, p-value = 0.4921
          t.test(Herato390_fem_equal$proportioncorr, Herato390_male_equal$proportioncorr, alternative = c("two.sided"), var.equal = TRUE)  #t = 1.0681, df = 20, p-value = 0.2982
          t.test(Herato390_fem_easy$proportioncorr, Herato390_male_easy$proportioncorr, alternative = c("two.sided"), var.equal = TRUE)  #t = -1.562, df = 20, p-value = 0.134
         
     #Assuming UNEQUAL variances (it doesn't change anything)
        #Sex comparison: 380 nm (all non-significant)
          t.test(Herato380_fem_hard$proportioncorr, Herato380_male_hard$proportioncorr, alternative = c("two.sided"), var.equal = FALSE)  #t = 0.86416, df = 17.742, p-value = 0.399
          t.test(Herato380_fem_equal$proportioncorr, Herato380_male_equal$proportioncorr, alternative = c("two.sided"), var.equal = FALSE)  #t = -0.042413, df = 19.813, p-value = 0.9666
          t.test(Herato380_fem_easy$proportioncorr, Herato380_male_easy$proportioncorr, alternative = c("two.sided"), var.equal = FALSE)  #t = 1.4421, df = 16.231, p-value = 0.1683
        #Sex comparison: 390 nm (all non-significant)
          t.test(Herato390_fem_hard$proportioncorr, Herato390_male_hard$proportioncorr, alternative = c("two.sided"), var.equal = FALSE)  #t = -0.69977, df = 14.706, p-value = 0.495
          t.test(Herato390_fem_equal$proportioncorr, Herato390_male_equal$proportioncorr, alternative = c("two.sided"), var.equal = FALSE)  #t = 1.0681, df = 19.928, p-value = 0.2982
          t.test(Herato390_fem_easy$proportioncorr, Herato390_male_easy$proportioncorr, alternative = c("two.sided"), var.equal = FALSE)  #t = -1.562, df = 19.675, p-value = 0.1342

    
#-----------------------------------------------------------------------------
#Analysis 2. Differences from 0.5
#----------------------------------------------------------------------------- 
    
##Binomial test - differences from 0.5 (1-tailed)
##------------------------------------------------
  #380+ Females
    binom.test( sum(Herato380_fem_hard$correct), sum(Herato380_fem_hard$total),.5, alternative=c("greater") ) #Light intensity 0.067 (hard)
    binom.test( sum(Herato380_fem_equal$correct), sum(Herato380_fem_equal$total),.5, alternative=c("greater") ) #Light intensity 1 (equal)
    binom.test( sum(Herato380_fem_easy$correct), sum(Herato380_fem_easy$total),.5, alternative=c("greater") ) #Light intensity 15 (easy)        
  #380+ Males
    binom.test( sum(Herato380_male_hard$correct), sum(Herato380_male_hard$total),.5, alternative=c("greater") ) #Light intensity 0.067 (hard)
    binom.test( sum(Herato380_male_equal$correct), sum(Herato380_male_equal$total),.5, alternative=c("greater") ) #Light intensity 1 (equal)
    binom.test( sum(Herato380_male_easy$correct), sum(Herato380_male_easy$total),.5, alternative=c("greater") ) #Light intensity 15 (easy)
  #390+ Females
    binom.test( sum(Herato390_fem_hard$correct), sum(Herato390_fem_hard$total),.5, alternative=c("greater") ) #Light intensity 0.067 (hard)
    binom.test( sum(Herato390_fem_equal$correct), sum(Herato390_fem_equal$total),.5, alternative=c("greater") ) #Light intensity 1 (equal)
    binom.test( sum(Herato390_fem_easy$correct), sum(Herato390_fem_easy$total),.5, alternative=c("greater") ) #Light intensity 15 (easy)        
  #390+ Males
    binom.test( sum(Herato390_male_hard$correct), sum(Herato390_male_hard$total),.5, alternative=c("greater") ) #Light intensity 0.067 (hard)
    binom.test( sum(Herato390_male_equal$correct), sum(Herato390_male_equal$total),.5, alternative=c("greater") ) #Light intensity 1 (equal)
    binom.test( sum(Herato390_male_easy$correct), sum(Herato390_male_easy$total),.5, alternative=c("greater") ) #Light intensity 15 (easy)

        
        ##Binomial test - differences from 0.6 (1-tailed)
        ##------------------------------------------------
            #380+ Females
              binom.test( sum(Herato380_fem_hard$correct), sum(Herato380_fem_hard$total),.6, alternative=c("greater") ) #Light intensity 0.067 (hard)
              binom.test( sum(Herato380_fem_equal$correct), sum(Herato380_fem_equal$total),.6, alternative=c("greater") ) #Light intensity 1 (equal)
              binom.test( sum(Herato380_fem_easy$correct), sum(Herato380_fem_easy$total),.6, alternative=c("greater") ) #Light intensity 15 (easy)        
            #380+ Males
              binom.test( sum(Herato380_male_hard$correct), sum(Herato380_male_hard$total),.6, alternative=c("greater") ) #Light intensity 0.067 (hard)
              binom.test( sum(Herato380_male_equal$correct), sum(Herato380_male_equal$total),.6, alternative=c("greater") ) #Light intensity 1 (equal)
              binom.test( sum(Herato380_male_easy$correct), sum(Herato380_male_easy$total),.6, alternative=c("greater") ) #Light intensity 15 (easy)
            #390+ Females
              binom.test( sum(Herato390_fem_hard$correct), sum(Herato390_fem_hard$total),.6, alternative=c("greater") ) #Light intensity 0.067 (hard)
              binom.test( sum(Herato390_fem_equal$correct), sum(Herato390_fem_equal$total),.6, alternative=c("greater") ) #Light intensity 1 (equal)
              binom.test( sum(Herato390_fem_easy$correct), sum(Herato390_fem_easy$total),.6, alternative=c("greater") ) #Light intensity 15 (easy)        
            #390+ Males
              binom.test( sum(Herato390_male_hard$correct), sum(Herato390_male_hard$total),.6, alternative=c("greater") ) #Light intensity 0.067 (hard)
              binom.test( sum(Herato390_male_equal$correct), sum(Herato390_male_equal$total),.6, alternative=c("greater") ) #Light intensity 1 (equal)
              binom.test( sum(Herato390_male_easy$correct), sum(Herato390_male_easy$total),.6, alternative=c("greater") ) #Light intensity 15 (easy)
       
              
                           
##------------------------------------------------------------------------------------------------------------------------          
##Bayesian first aid
##------------------------------------------------------------------------------------------------------------------------
              install.packages("devtools")
              
              install.packages("rjags")
              library(rjags)
              
              install.packages("BayesianFirstAid")
                library(BayesianFirstAid)  
          
  ###Bayesian binomial test: enter no. successes, no. trials. Ignores alternative argument "greater". 
  ###Gives confidence interval, relative frequency of success            
  ###-------------------------------------------------------------------------------------------------------          
 
   ### H doris -------            
       #380+ Females
        Hdoris380_fem_hard_bayes <-bayes.binom.test( sum(Hdoris380_fem_hard$correct), sum(Hdoris380_fem_hard$total),.5 ) #Light intensity 0.067 (hard)
        Hdoris380_fem_equal_bayes <-bayes.binom.test( sum(Hdoris380_fem_equal$correct), sum(Hdoris380_fem_equal$total),.5 ) #Light intensity 1 (equal)
        Hdoris380_fem_easy_bayes <-bayes.binom.test( sum(Hdoris380_fem_easy$correct), sum(Hdoris380_fem_easy$total),.5 ) #Light intensity 15 (easy)        
      #380+ Males
        Hdoris380_male_hard_bayes <-bayes.binom.test( sum(Hdoris380_male_hard$correct), sum(Hdoris380_male_hard$total),.5 ) #Light intensity 0.067 (hard)
        Hdoris380_male_equal_bayes <-bayes.binom.test( sum(Hdoris380_male_equal$correct), sum(Hdoris380_male_equal$total),.5 ) #Light intensity 1 (equal)
        Hdoris380_male_easy_bayes <-bayes.binom.test( sum(Hdoris380_male_easy$correct), sum(Hdoris380_male_easy$total),.5 ) #Light intensity 15 (easy)
      #390+ Females
        Hdoris390_fem_hard_bayes <-bayes.binom.test( sum(Hdoris390_fem_hard$correct), sum(Hdoris390_fem_hard$total),.5 ) #Light intensity 0.067 (hard)
        Hdoris390_fem_equal_bayes <-bayes.binom.test( sum(Hdoris390_fem_equal$correct), sum(Hdoris390_fem_equal$total),.5 ) #Light intensity 1 (equal)
        Hdoris390_fem_easy_bayes <-bayes.binom.test( sum(Hdoris390_fem_easy$correct), sum(Hdoris390_fem_easy$total),.5 ) #Light intensity 15 (easy)        
      #390+ Males
        Hdoris390_male_hard_bayes <-bayes.binom.test( sum(Hdoris390_male_hard$correct), sum(Hdoris390_male_hard$total),.5 ) #Light intensity 0.067 (hard)
        Hdoris390_male_equal_bayes <-bayes.binom.test( sum(Hdoris390_male_equal$correct), sum(Hdoris390_male_equal$total),.5 ) #Light intensity 1 (equal)
        Hdoris390_male_easy_bayes <-bayes.binom.test( sum(Hdoris390_male_easy$correct), sum(Hdoris390_male_easy$total),.5 ) #Light intensity 15 (easy)
    
        
        #160808 WORKING ON THIS ---------------------------------
        
        Hdoris380_fem_hard_bayes <-bayes.binom.test( sum(Hdoris380_fem_hard$correct), sum(Hdoris380_fem_hard$total),.5 ) #Light intensity 0.067 (hard)
        Hdoris380_fem_equal_bayes <-bayes.binom.test( sum(Hdoris380_fem_equal$correct), sum(Hdoris380_fem_equal$total),.5 ) #Light intensity 1 (equal)
        Hdoris380_fem_easy_bayes <-bayes.binom.test( sum(Hdoris380_fem_easy$correct), sum(Hdoris380_fem_easy$total),.5 ) #Light intensity 15 (easy)   
        
            Hdor380_alllighttrials_test <-c(Hdoris380_fem_hard_bayes, Hdoris380_fem_equal_bayes, Hdoris380_fem_easy_bayes)
        
                    
                        plot(Hdoris380_fem_hard_bayes, Hdoris380_fem_equal_bayes, Hdoris380_fem_easy_bayes) #test: Can I display all light trials on the same graph? Not this way. To do = look up superimpose Bayes plot.
                        plot(Hdor380_alllighttrials_test) #concat doesn't work either
                        
        #------------------------------
        
        
        ###Plots of relative freq. of success, data with post. pred. 
          ### H doris -------
            #380+ Hdor Females
              plot(Hdoris380_fem_hard_bayes)
              plot(Hdoris380_fem_equal_bayes)
              plot(Hdoris380_fem_easy_bayes)
            #380+ Hdor Males
              plot(Hdoris380_male_hard_bayes)
              plot(Hdoris380_male_equal_bayes)
              plot(Hdoris380_male_easy_bayes)
            #390+ Hdor Females
              plot(Hdoris390_fem_hard_bayes)
              plot(Hdoris390_fem_equal_bayes)
              plot(Hdoris390_fem_easy_bayes)
            #390+ Hdor Males
              plot(Hdoris390_male_hard_bayes)
              plot(Hdoris390_male_equal_bayes)
              plot(Hdoris390_male_easy_bayes)
          ### H erato -------  
            #380+ Her Females
              plot(Herato380_fem_hard_bayes)
              plot(Herato380_fem_equal_bayes)
              plot(Herato380_fem_easy_bayes)
            #380+ Her Males
              plot(Herato380_male_hard_bayes)
              plot(Herato380_male_equal_bayes)
              plot(Herato380_male_easy_bayes)
            #390+ Her Females
              plot(Herato390_fem_hard_bayes)
              plot(Herato390_fem_equal_bayes)
              plot(Herato390_fem_easy_bayes)
            #390+ Her Males
              plot(Herato390_male_hard_bayes)
              plot(Herato390_male_equal_bayes)
              plot(Herato390_male_easy_bayes)
              
                        #nice printout of the model 
                        model.code(bayes.binom.test(82, 121)) 
    
    
  ##Hdoris
    #380+ Females
    Hdoris380_fem_hard_bayes2 <-bayes.prop.test( sum(Hdoris380_fem_hard$correct), sum(Hdoris380_fem_hard$total),.5 ) #Light intensity 0.067 (hard)
    Hdoris380_fem_equal_bayes2 <-bayes.prop.test( sum(Hdoris380_fem_equal$correct), sum(Hdoris380_fem_equal$total),.5 ) #Light intensity 1 (equal)
    Hdoris380_fem_easy_bayes2 <-bayes.prop.test( sum(Hdoris380_fem_easy$correct), sum(Hdoris380_fem_easy$total),.5 ) #Light intensity 15 (easy)        
    #380+ Males
    Hdoris380_male_hard_bayes2 <-bayes.prop.test( sum(Hdoris380_male_hard$correct), sum(Hdoris380_male_hard$total),.5 ) #Light intensity 0.067 (hard)
    Hdoris380_male_equal_bayes2 <-bayes.prop.test( sum(Hdoris380_male_equal$correct), sum(Hdoris380_male_equal$total),.5 ) #Light intensity 1 (equal)
    Hdoris380_male_easy_bayes2 <-bayes.prop.test( sum(Hdoris380_male_easy$correct), sum(Hdoris380_male_easy$total),.5 ) #Light intensity 15 (easy)
    #390+ Females
    Hdoris390_fem_hard_bayes2 <-bayes.prop.test( sum(Hdoris390_fem_hard$correct), sum(Hdoris390_fem_hard$total),.5 ) #Light intensity 0.067 (hard)
    Hdoris390_fem_equal_bayes2 <-bayes.prop.test( sum(Hdoris390_fem_equal$correct), sum(Hdoris390_fem_equal$total),.5 ) #Light intensity 1 (equal)
    Hdoris390_fem_easy_bayes2 <-bayes.prop.test( sum(Hdoris390_fem_easy$correct), sum(Hdoris390_fem_easy$total),.5 ) #Light intensity 15 (easy)        
    #390+ Males
    Hdoris390_male_hard_bayes2 <-bayes.prop.test( sum(Hdoris390_male_hard$correct), sum(Hdoris390_male_hard$total),.5 ) #Light intensity 0.067 (hard)
    Hdoris390_male_equal_bayes2 <-bayes.prop.test( sum(Hdoris390_male_equal$correct), sum(Hdoris390_male_equal$total),.5 ) #Light intensity 1 (equal)
    Hdoris390_male_easy_bayes2 <-bayes.prop.test( sum(Hdoris390_male_easy$correct), sum(Hdoris390_male_easy$total),.5 ) #Light intensity 15 (easy)
    plot(Hdoris380_fem_hard_bayes2)                      
                    
  
    




