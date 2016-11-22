library(tidyverse)
library(apaTables)



# Main Analysis -----------------------------------------------------------

analytic.data <- read_csv("lectureData6060.csv")
apa.cor.table(analytic.data, table.number = 1)
apa.cor.table(analytic.data, filename="Table1_APA.doc", table.number=1)

#center variables
analytic.data$x.centered    <- as.numeric(scale(analytic.data$Anxiety, center=T, scale=F))
analytic.data$z.centered    <- as.numeric(scale(analytic.data$Preparation, center=T, scale=F))
interaction.regression <- lm(Exam~x.centered+z.centered+I(x.centered*z.centered), data=analytic.data, na.action = na.exclude)


# I get the table this way
apa.reg.table(interaction.regression, table.number = 2)
apa.reg.table(interaction.regression, filename="Table2_APA.doc", table.number=2)

#I get the values for the text this way
summary(interaction.regression)



# Surface cross-section line +1 SD  on z (i.e., +1 SD on Preparation) -----------------------------------------
sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)
analytic.data <- analytic.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z)

simple.slope.plus.1SD <- lm(Exam ~ x.centered + z.centered.at.plus.1SD
                            + I(x.centered*z.centered.at.plus.1SD),
                            data=analytic.data, na.action=na.exclude)

#table this way (not included in report)
apa.reg.table(simple.slope.plus.1SD)

#values for text this way
summary(simple.slope.plus.1SD)




# Surface cross-section line -1 SD on z (i.e., -1 SD Preparation) -----------------------------------------
analytic.data <- analytic.data %>% mutate(z.centered.at.minus.1SD=z.centered + sd.z)

simple.slope.minus.1SD <- lm(Exam ~ x.centered + z.centered.at.minus.1SD
                             + I(x.centered*z.centered.at.minus.1SD),
                             data=analytic.data,na.action=na.exclude)


#table this way (not included in report)
apa.reg.table(simple.slope.minus.1SD)


#values for text this way
summary(simple.slope.minus.1SD)


#  Figure 1: 3D Plot -----------------------------------------------------------------

# See: summary(interaction.regression) for numbers to input here
# b.0 is the intercept (55.5794), b.x is the reg coefficient for x (-2.1503), b.z is the reg
# coefficient for predictor z (4.5648), b.xz is the reg. coefficient for the interaction of x and z
# x.min, x.max, z.min, z.max are the ranges of x and z (david made is 2 standard deviations above/below)
# xlab is the title for the axis predictor x is on,zlab is the title for axis predictor z is on 
# ylab is the title for the axis dependent y is on
library(MBESS)

sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)
sd.z<-sd(analytic.data$z.centered, na.rm=TRUE)
#these are the predictors

intr.plot(b.0=55.5794,b.x=-2.1503,b.z=4.5648,b.xz=0.9077,
          x.min=-2*sd.x,x.max=2*sd.x,z.min=-2*sd.z,z.max=2*sd.z,
          xlab="Anxiety Centered",zlab="Preparation Centered",ylab="Exam Score",
          expand=1,hor.angle=60,gray.scale=TRUE) 

#save using menu's in RStudio. Manually include in MSWord Document


#  Figure 2: 2D Plot -----------------------------------------------------------------

# 2D Graph Range on X-Axis ----------------------------------------------------------------

sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)

#graph from -1SD on Anxiety to +1SD Anxiety for a given level of Prepartion (+/-1 SD)
x.axis.range <-seq(-1*sd.x,1*sd.x,by=.25*sd.x) #this indicates range on the x-axis -1SD to +1SD Anxiety


# 2D Graph Lines for +1 SD on Prep and -1SD on Prep ----------------------------------------------------------------

#level of z (+/- 1SD on Preparation)
sd.z<-sd(analytic.data$z.centered, na.rm=TRUE)
z.line.hi=  1*sd.z
z.line.lo= -1*sd.z


# 2D Graph: Create Data for Drawing Lines ----------------------------------------------------------------
#+1SD Line
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.hi)

#-1SD Line
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo) 
y.values.at.minus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.lo)

#Put the information describing the lines into a data frame
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)




# 2D Graph: Make the graph using above data -------------------------------

my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z)) 

#make +1 SD Z line (because it is the one in the aes statement above)
my.plot <- my.plot + geom_line(color="black",linetype="dotted",size=1.5) 

#make the -1 SD Z line
my.plot <- my.plot + geom_line(aes(x=x.axis.range,y=y.values.at.minus.1SD.z), 
                               color="black",linetype="solid",size=1.5) 

#set APA part of graph below
my.plot <- my.plot + theme_classic(18)

#labels
my.plot <- my.plot + labs(x="Anxiety (mean centered)", y="Exam Grade")

#the SD of Anxiety (see Table 1 is 2.00 so -1SD to +1SD is -2 to 2)
my.plot <- my.plot + coord_cartesian(xlim=c(-2,2),ylim=c(0,100))

print(my.plot)
#save using menu's in RStudio. Manually include in MSWord Document



