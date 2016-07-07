setwd("/Users/andrewbertoli/Dropbox/United Government/ReplicationCode")

library(ggplot2)
library(rdrobust)
library(easyGgplot2)
library(wordcloud)
library(scatterplot3d)
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)

source("RDPlot1.R")
source("rdplot.R")
source("rdpointest.R")
source("BalancePlot.R")

data=read.csv("elections.csv",stringsAsFactors=FALSE)

# First drop cases where the most powerful party controlled exactly 50% 
# of a legislature body, except for the US Senate (where United is set at 1)

data$MinDist[data$MinDist==0&data$United==0]=NA

data$MinDist[data$MinDist==0&data$United==1]=0.001


# Now subset to the countries that are within 10% of the cut-point 
# We will also drop the countries that only had one legislative body (Type=4)
# The results are signicant if you include these countries and count them as 
# united if a single party controls the legislative body (single party majority
# government).


close=data[abs(data$MinDist)<=0.1&is.na(data$MinDist)==FALSE&data$Type!=4,]
close=data[abs(data$MinDist)<=0.1&is.na(data$MinDist)==FALSE&data$Type!=4&data$Democracy==1,]

# Create the graphs showing how close the strongest parties were to achieving united government


# Making the democracies graph

setwd("/Users/andrewbertoli/Dropbox/United Government/ReplicationCode")
polity=read.csv("Polity.csv",stringsAsFactors=FALSE)

countries=c("Bolivia","Brazil","Burundi","Colombia","Dominican Rep","Mexico","United States",
"Uruguay","","","Australia", "Belgium","Czech Republic","Italy", "Japan","Spain", "Turkey",
"","","Austria","Cape Verde","Costa Rica","Croatia","Ecuador","France","El Salvador",
"Ghana","Guatemala","Honduras","Ireland","Kenya","Madagascar","Malawi","Mali","Mongolia",
"Nicaragua","Panama","Portugal","Korea South","Taiwan")

dems=polity[polity$polity>=6&polity$year%in%1815:2010&polity$country%in%countries,]

dems$index=match(dems$country,countries)

countries[countries=="Korea South"]="South Korea"

tick_colors=rep("black",length(countries))
tick_colors[countries==""]="white"

ylab="       Bicameral       Bicameral                 Unicameral                        \n     
Presidential   Parlimentary              Presidential                       \n   Systems         
Systems                    Systems                     "

dems$Type=NA
dems$Type[dems$index<=8]="dodgerblue"
dems$Type[dems$index>8&dems$index<=17]="gray48"
dems$Type[dems$index>8&dems$index>18]="darkorange"


pdf("DemocracyYears.pdf",height=6,width=9)
ggplot(dems,aes(dems$year,dems$index))+geom_segment(aes(xend=dems$year+1,yend=dems$index),size=1,
color=dems$Type) +ylab(ylab) +xlab("Years Countries Were Democracies")+theme_bw()+
theme(legend.position="none")+scale_y_continuous(breaks=1:length(countries),labels=countries) +
theme(axis.ticks.y=element_line(color=tick_colors))+scale_colour_gradientn(colours=rainbow(3))+
theme(axis.title.y=element_text(size=13.5,face="bold"),axis.title.x=element_text(size=17),
axis.text.x=element_text(size=15))
dev.off()





tbr=data[data$Type==2&is.na(data$PresVotes)==FALSE&is.na(data$Leg1Seats)==FALSE&is.na(data$StrongestParty)==FALSE,]

X=rep(NA,nrow(tbr))
Y=rep(NA,nrow(tbr))

for(i in 1:nrow(tbr)){
if(tbr$Leg1[i]==tbr$StrongestParty[i]){X[i]=tbr$Leg1Percent[i]-0.5}
if(tbr$Leg1Second[i]==tbr$StrongestParty[i]){X[i]=tbr$Leg1SecondPercent[i]-0.5}
if(tbr$Pres[i]==tbr$StrongestParty[i]){Y[i]=tbr$PresPercent[i]-tbr$PresSecondPercent[i]}
if(tbr$PresSecond[i]==tbr$StrongestParty[i]&tbr$Pres[i]!=tbr$PresSecond[i]){Y[i]=tbr$PresSecondPercent[i]-tbr$PresPercent[i]}
}




pdf("TwoBranchDistSlides.pdf", height=4.5, width=5.5)
qplot(X,Y,colour=factor(tbr$United))+xlab("Percent of Seats from Controlling Legislature")+
ylab("Percent of Votes from Controlling Presidency")+geom_vline(xintercept=0, colour="black")+
theme(plot.title=element_text(size=16),axis.title=element_text(size=14),axis.text=element_text(size=13))
+geom_hline(yintercept=0, colour="black")+theme_bw()+theme(legend.position="none")+
scale_colour_manual(values = c("royalblue4","goldenrod"))+ 
labs(title="Figure 2: Strength of Most Powerful Party in\nElections with a President and Legislature")+
xlab("Percent of Seats from Controlling House")
dev.off()

pdf("TwoBranchDist.pdf", height=4.5, width=5.5)
qplot(X,Y,colour=factor(tbr$United))+xlab("Percent of Seats from Controlling Legislature")+
ylab("Percent of Votes from Controlling Presidency")+geom_vline(xintercept=0, colour="black")+
theme(plot.title=element_text(size=16),axis.title=element_text(size=14),axis.text=element_text(size=13))+
geom_hline(yintercept=0, colour="black")+theme_bw()+theme(legend.position="none")+
scale_colour_manual(values = c("royalblue4","goldenrod"))
dev.off()




tlr=data[data$Type==3&is.na(data$Leg1Seats)==FALSE&is.na(data$Leg2Seats)==FALSE&is.na(data$StrongestParty)==FALSE,]

X=rep(NA,nrow(tlr))
Y=rep(NA,nrow(tlr))

for(i in 1:nrow(tlr)){
if(tlr$Leg1[i]==tlr$StrongestParty[i]){X[i]=tlr$Leg1Percent[i]-0.5}
if(tlr$Leg1Second[i]==tlr$StrongestParty[i]){X[i]=tlr$Leg1SecondPercent[i]-0.5}
if(tlr$Leg2[i]==tlr$StrongestParty[i]){Y[i]=tlr$Leg2Percent[i]-0.5}
if(tlr$Leg2Second[i]==tlr$StrongestParty[i]){Y[i]=tlr$Leg2SecondPercent[i]-0.5}
}

pdf("TwoHouseDist.pdf", height=4.5, width=5.5)
qplot(X,Y,colour=factor(tlr$United))+xlab("Percent of Seats from Controlling House")+
ylab("Percent of Seats from Controlling Senate")+geom_vline(xintercept=0, colour="black")+theme_bw()+ theme(plot.title=element_text(size=16),
axis.title=element_text(size=14),axis.text=element_text(size=13))+
geom_hline(yintercept=0, colour="black")+theme(legend.position="none")+
scale_colour_manual(values = c("royalblue4","goldenrod"))
dev.off()

pdf("TwoHouseDistSlides.pdf", height=4.5, width=5.5)
qplot(X,Y,colour=factor(tlr$United))+ 
labs(title="Figure 3: Strength of Most Powerful Party in\nElections with Two Legislative Bodies")+
xlab("Percent of Seats from Controlling House")+ylab("Percent of Seats from Controlling Senate")+
geom_vline(xintercept=0, colour="black")+theme_bw()+ theme(plot.title=element_text(size=16),
axis.title=element_text(size=14),axis.text=element_text(size=13))+geom_hline(yintercept=0, 
colour="black")+scale_colour_manual(values = c("royalblue4","goldenrod"))+theme_bw()+
theme(legend.position="none")
dev.off()

tbr=data[is.na(data$PresVotes)==FALSE&is.na(data$Leg1Seats)==FALSE&is.na(data$Leg2Seats)==FALSE&is.na(data$StrongestParty)==FALSE,]

X=rep(NA,nrow(tbr))
Y=rep(NA,nrow(tbr))
Z=rep(NA,nrow(tbr))

for(i in 1:nrow(tbr)){
if(tbr$Leg1[i]==tbr$StrongestParty[i]){X[i]=tbr$Leg1Percent[i]-0.5}
if(tbr$Leg1Second[i]==tbr$StrongestParty[i]){X[i]=tbr$Leg1SecondPercent[i]-0.5}
if(tbr$Pres[i]==tbr$StrongestParty[i]){Y[i]=tbr$PresPercent[i]-tbr$PresSecondPercent[i]}
if(tbr$PresSecond[i]==tbr$StrongestParty[i]&tbr$Pres[i]!=tbr$PresSecond[i]){Y[i]=tbr$PresSecondPercent[i]-tbr$PresPercent[i]}
if(tbr$Leg2[i]==tbr$StrongestParty[i]){Z[i]=tbr$Leg2Percent[i]-0.5}
if(tbr$Leg2Second[i]==tbr$StrongestParty[i]){Z[i]=tbr$Leg2SecondPercent[i]-0.5}
}

tbr$United[tbr$United=="1"]="goldenrod"
tbr$United[tbr$United=="0"]="royalblue4"

pdf("3dGraphSlides.pdf")
scatterplot3d(X,Y,Z,color=tbr$United,xlab="Percent of Seats from Controlling House",ylab="",
zlab="Percent of Seats from Controlling Senate",type="h",
main="Figure 4: Strength of Most Powerful Party in Elections\nwith a President and Two Legislative Bodies",
xlim=c(-0.4,0.4),ylim=c(-0.4,0.4),zlim=c(-0.4,0.4))
text(3.5,-1.5,"Percent of Votes from Controlling Presidency",srt=40,xpd=TRUE)
dev.off()



pdf("3dGraph.pdf")
scatterplot3d(X,Y,Z,color=tbr$United,xlab="Percent of Seats from Controlling House",ylab="",
zlab="Percent of Seats from Controlling Senate",type="h",main="",xlim=c(-0.4,0.4),
ylim=c(-0.4,0.4),zlim=c(-0.4,0.4));text(3.5,-1.5,"Percent of Votes from Controlling Presidency",
srt=40,xpd=TRUE)
dev.off()











# Testing for a Discontinuity in the Forcing Variable

pdf("ForcingDensity.pdf", height=4.5, width=7.5)
m <- ggplot(data[data$Type!=4&data$Democracy==1,], aes(x=MinDist*100))
m + geom_histogram(fill="cornflowerblue",
                   binwidth=2, color="black",
                   origin = -140.001)+
  theme_bw()+theme(axis.title = element_text(size=16),plot.title=element_text(size=20))+
  geom_vline(xintercept=0, colour="red")+
  xlab("Percentage of Seats/Votes Away from United Government")+
  ylab("Density") + labs(title="")+
  scale_x_continuous(breaks=seq(-100, 100, 20))+
  scale_y_continuous(breaks=seq(0, 50, 10))
dev.off()


pdf("ForcingDensitySlides.pdf", height=4.5, width=7.5)
m <- ggplot(data[data$Type!=4&data$Democracy==1,], aes(x=MinDist*100))
m + geom_histogram(fill="cornflowerblue",
                   binwidth=2, color="black",
                   origin = -140.001)+
  theme_bw()+theme(axis.title = element_text(size=16),plot.title=element_text(size=20))+
  geom_vline(xintercept=0, colour="red")+
  xlab("Percentage of Seats/Votes Away from United Government")+
  ylab("Density") + labs(title="Figure 5. Distribution of the Forcing Variable")+
  scale_x_continuous(breaks=seq(-100, 100, 20))+
  scale_y_continuous(breaks=seq(0, 25, 5))
dev.off()


pdf("UnitedBPSlides.pdf", width=7, height=6)
BalancePlot(close,close$United, c("lnirst","lnmilex","lnmilper","lnpec","lntpop","lnupop","PrevUnited",
"PreviousLowDisputes","PreviousHighDisputes"),c("ln(Iron and Steel Production)","ln(Military Expenditures)",
"ln(Military Personnel)","ln(Energy Consumption)","ln(Total Population)","ln(Urban Population)",
"Previously United", "Previous Low-Level Disputes per Year", "Previous High-Level Disputes per Year"),
Different.Test=RDTest, Built.In.Tests =NULL,na.rm=TRUE,pch=16, Shade.Color="cadetblue2",Point.Color="Black",
Title="Figure 6. Testing for Balance at Cutpoint")
dev.off()

pdf("UnitedBP.pdf", width=6.5, height=4.5)
BalancePlot(close,close$United, c("lnirst","lnmilex","lnmilper","lnpec","lntpop","lnupop","PrevUnited",
"PreviousLowDisputes","PreviousHighDisputes"),c("ln(Iron and Steel Production)","ln(Military Expenditures)",
"ln(Military Personnel)","ln(Energy Consumption)","ln(Total Population)","ln(Urban Population)",
"Previously United", "Previous Low-Level Disputes per Year", "Previous High-Level Disputes per Year"),
Different.Test=RDTest, Built.In.Tests =NULL,na.rm=TRUE,pch=16, Shade.Color="cadetblue2",Point.Color="Black",
Title="")
dev.off()




# All disputes

rdrobust(close$Aggression,close$MinDist, all=TRUE)

rdrobust(close$PreviousAggression,close$MinDist,all=TRUE)

rdrobust(close$Aggression-close$PreviousAggression,close$MinDist,all=TRUE)




# High-level disputes

rdrobust(close$HighDisputes,close$MinDist, all=TRUE)

rdrobust(close$PreviousHighDisputes,close$MinDist,all=TRUE)

rdrobust(close$HighDisputes-close$PreviousHighDisputes,close$MinDist,all=TRUE)


# Low-level disputes

rdrobust(close$LowDisputes,close$MinDist, all=TRUE)

rdrobust(close$PreviousLowDisputes,close$MinDist, all=TRUE)

rdrobust(close$LowDisputes-close$PreviousLowDisputes,close$MinDist, all=TRUE)










# Coefficient Plot

est1=as.numeric(rdrobust(close$Aggression-close$PreviousAggression,close$MinDist,all=TRUE)[[3]][3,c(1,5:6)])

est2=as.numeric(rdrobust(close$HighDisputes-close$PreviousHighDisputes,close$MinDist,all=TRUE)[[3]][3,c(1,5:6)])

est3=as.numeric(rdrobust(close$LowDisputes-close$PreviousLowDisputes,close$MinDist,all=TRUE)[[3]][3,c(1,5:6)])



theme_nolegend <- function (base_size = 9, base_family = "", height, width) 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.8)), 
          legend.position="none", 
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA,colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80",  colour = "grey50"), 
          strip.background = element_rect(fill = "grey80", colour = "grey50"))
}
# summary results:
cd <- as.data.frame(matrix(NA,3,5))
conditions <- c("All Disputes Initiated","High-Level Disputes Initiated","Low-Level Disputes Initiated")
names(cd) <- c("mean","se","measure")
cd$mean <- as.numeric(c(est1[1],est2[1],est3[1]))
cd$lower <- as.numeric(c(est1[2],est2[2],est3[2]))
cd$upper <- as.numeric(c(est1[3],est2[3],est3[3]))
cd$ord <- c(3,2,1)
cd$measure <- factor(conditions, levels=conditions[order(cd$ord)])
# make the graph
library(ggplot2)
f <- ggplot(cd, 
            aes(x=mean,y=measure,color=measure))
f <- f+geom_vline(xintercept=0, linetype="longdash")+

  geom_errorbarh(aes(xmax =  upper, #95 confience interval
                     xmin = lower),
                 size=1.5, height=0)+
  geom_point(stat="identity",size=4,fill="white")+
  scale_color_manual(name="",
                     values=c("mediumblue","firebrick3","forestgreen"))+
  xlab("Estimated Treatment Effect")+ylab("")+ labs(title="") +  theme_nolegend()+theme(axis.text=element_text(size=10),
  axis.title=element_text(size=11.5),plot.title = element_text(lineheight=1.8,size=rel(1.5),face="bold",hjust=1.835,vjust=2))+
   xlim(c(-0.9,1.5))

ggsave("UnitedBiasCorrected.pdf",width=5,height=2)


f <- ggplot(cd, 
            aes(x=mean,y=measure,color=measure))
f <- f+geom_vline(xintercept=0, linetype="longdash")+

  geom_errorbarh(aes(xmax =  mean + 1.96*se, #95 confience interval
                     xmin = mean - 1.96*se),
                 size=1.5, height=0)+
  geom_point(stat="identity",size=4,fill="white")+
  scale_color_manual(name="",
                     values=c("mediumblue","firebrick3","forestgreen"))+
  xlab("Estimated Treatment Effect\n(Standardized)")+ylab("")+ labs(title="Figure 7. Regression Discontinuity Estimates") 
  +  theme_nolegend()+theme(axis.text=element_text(size=10),axis.title=element_text(size=13),plot.title = 
  element_text(lineheight=1.8,size=rel(1.5),face="bold",hjust=1.835,vjust=2))+ xlim(c(-1,2))
ggsave("UnitedBiasCorrectedSlides.pdf",width=7,height=3)






pdf("UnitedRDLabel.pdf", width=5, height=5)

Boots=10000

Bandwidth=rdbwselect(close$HighDisputes-close$PreviousHighDisputes,close$MinDist)$bws[1]

RDPlot(close$MinDist,close$HighDisputes-close$PreviousHighDisputes, Bandwidth=Bandwidth,NBoots=Boots, 
Main="", ylab="Change in High-Level Disptues Initiated per Year", xlab="Vote Share from United Government", 
Tick.Marks = c(-0.1, -0.05, 0, 0.05, 0.1), ylim=c(-2,2),cex.main=1.2, cex.lab=0.78, Breaks=seq(-.1,1,.02),
Plot.Raw.Data=TRUE,Plot.Means=FALSE, Raw.Data.Colors=c("royalblue3","goldenrod"),LabelRawData=TRUE,
Labels=paste(close$Country,close$Year),RawDataLabelSize = 0.75, Smoother = "Local Linear")

dev.off()




USA=close[close$Country=="USA",]

Bandwidth=rdbwselect(USA$HighDisputes-USA$PreviousHighDisputes,USA$MinDist)$bws[1]

summary(lm(HighDisputes~United+MinDist+I(United*MinDist),USA[abs(USA$MinDist)<=Bandwidth,]))

RDPlot(USA$MinDist,USA$HighDisputes-USA$PreviousHighDisputes, Bandwidth=Bandwidth,NBoots=Boots, Main="", 
ylab="High-Level Disptues per Year", xlab="Vote Share from United Government", Tick.Marks = 
c(-0.1, -0.05, 0, 0.05, 0.1), ylim=c(-0.1,2),cex.main=1.2, cex.lab=0.78, Breaks=seq(-.1,1,.02),
Plot.Raw.Data=TRUE,Plot.Means=FALSE, Raw.Data.Colors=c("royalblue3","goldenrod"),LabelRawData=TRUE,
Labels=paste(close$Country,close$Year),RawDataLabelSize = 0.75,Plot.p.value=FALSE)





pdf("UnitedRDLabelSlides.pdf", width=5, height=5)

Boots=10000

Bandwidth=rdbwselect(close$HighDisputes,close$MinDist)$bws[1]

RDPlot(close$MinDist,close$HighDisputes, Bandwidth=Bandwidth,NBoots=Boots, Main="", ylab="High-Level Disptues per Year",
xlab="Vote Share from United Government", Tick.Marks = c(-0.1, -0.05, 0, 0.05, 0.1), ylim=c(-0.1,2),cex.main=1.2, 
cex.lab=0.78, Breaks=seq(-.1,1,.02),Plot.Raw.Data=TRUE,Plot.Means=FALSE, Raw.Data.Colors=c("royalblue3","goldenrod"),
LabelRawData=TRUE,Labels=paste(close$Country,close$Year),RawDataLabelSize = 0.75,Plot.p.value=FALSE)

dev.off()





USA=close[close$Country=="USA",]

rdrobust(USA$HighDisputes-USA$PreviousHighDisputes,USA$MinDist,all=TRUE)






# Robustness Checks

# t-test very close to the cut-point (0.5% RD window)

# Note to self-put this in paper

t.test(HighDisputes~United,close[abs(close$MinDist)<0.005,])

t.test(PreviousHighDisputes~United,close[abs(close$MinDist)<0.005,])

t.test(HighDisputes-PreviousHighDisputes~United,close[abs(close$MinDist)<0.005,])


# Controlling for factors from the balance plot

model=lm(HighDisputes-PreviousHighDisputes~lnirst+lnmilex+lnmilper+lnpec+lntpop+lnupop+PrevUnited, close) # Add mor covariates here

close$residuals=close$HighDisputes-predict(model,close)

rdrobust(close$residuals,close$MinDist, all=TRUE)








setwd("/Users/andrewbertoli/Dropbox/United Government/ReplicationCode")

USA=read.csv("USA.csv")

USA$BeginDate=as.Date(USA[,"BeginDate"],format='%m/%d/%Y')
USA$EndDate=as.Date(USA[,"EndDate"],format='%m/%d/%Y')

Wars=as.Date(c("1812-6-18","1846-4-25","1899-6-2","1917-4-1","1942-12-8","1950-7-1","1964-2-23","2001-10-7","2003-3-20"))

United.At.Time=c(1,1,1,1,1,1,1,0,1)

WarsNames=c("War of 1812","Mexican-American War","Spanish-American War","World War I","World War II","Korean War","Vietnam War",
"War in Afghanistan","War in Iraq")

USwars=data.frame(Wars,United.At.Time,Wars)

pdf("HistoricalUS.pdf", width=11, height=2.5)

ggplot(USA,aes(USA$BeginDate,USA$United))+geom_segment(aes(xend=USA$EndDate,yend=USA$United),size=11,color="cornflowerblue")+
geom_point(data=USwars,aes(x=Wars,y=United.At.Time,colour=WarsNames),size=5,fill="white")+scale_colour_manual(" ",values=rep("black",10))+
ylab(" ")+xlab("")+theme_bw()+theme(axis.text=element_text(size=20),axis.title.x = element_text(vjust=-0.7),
axis.title=element_text(size=20),plot.title = element_text(lineheight=1.8,size=rel(2),face="bold",vjust=2,hjust=1.25))+
scale_y_continuous(breaks=c(0,1),labels=c("Divided","United"),limits=c(-.125,1.125))+theme(legend.position="none")

dev.off()

pdf("HistoricalUSSlides.pdf", width=11, height=2.5)

ggplot(USA,aes(USA$BeginDate,USA$United))+geom_segment(aes(xend=USA$EndDate,yend=USA$United),size=11,color="cornflowerblue")+
geom_point(data=USwars,aes(x=Wars,y=United.At.Time,colour=WarsNames),size=5,fill="white")+scale_colour_manual(" ",
values=rep("black",10))+ylab(" ")+xlab("")+theme_bw()+theme(axis.text=element_text(size=20),axis.title.x = element_text(vjust=-0.7),
axis.title=element_text(size=20),plot.title = element_text(lineheight=1.8,size=rel(2),face="bold",vjust=2,hjust=1.25))+
labs(title="Figure 1. Interstate Wars with at Least 1,000 U.S. Battle Deaths")+ scale_y_continuous(breaks=c(0,1),
labels=c("Divided","United"),limits=c(-.125,1.125))+theme(legend.position="none")

dev.off()


z=ggplot(USA,aes(USA$BeginDate,USA$United))+geom_point(data=USwars,aes(x=Wars,y=United.At.Time,colour=WarsNames),size=5,fill="white")+
scale_colour_manual(" ",values=rep("black",10))+ylab(" ")+xlab("Year")+theme_bw()+theme(axis.text=element_text(size=20),axis.title.x =
 element_text(vjust=-0.7),axis.title=element_text(size=20),plot.title = element_text(lineheight=1.8,size=rel(2),face="bold",
 vjust=2,hjust=1.25))+labs(title="Figure 8. Interstate Wars with at Least 1,000 U.S. Battle Deaths")+theme(legend.position="none")
 
out=py$ggplotly(z,kwargs=list(filename="gg-basic-segment", fileopt="overwrite"))












# SpecificCountries

Countries=c(unique(close$Country)," ")
new_results=matrix(0,nrow=length(Countries),ncol=3)
n=rep(NA,length(Countries))

for(i in 1:length(Countries)){
n[i]=sum(close$Country==Countries[i])
new_data=close[-which(close$Country==Countries[i]),]
new_results[i,]=c(as.numeric(rdrobust(new_data$HighDisputes-new_data$PreviousHighDisputes,new_data$MinDist,all=TRUE)[[3]][3,c(1:2,4)]))}

ord=order(new_results[,1])



cbind(Countries[ord],new_results[ord,],n[ord])







t=close[close$United==1&abs(close$MinDist)<=0.02,]
c=close[close$United==0&abs(close$MinDist)<=0.02,]
dates=c("Date1","Date2","Date3","Date4","Date5","Date6","Date7","Date8","Date9","Date10","Date11","Date12","Date13",
"Date14","Date15","Date16","Date17","Date18","Date19","Date20")
t.dates=as.numeric(unlist(as.vector(t[,dates]))[is.na(unlist(as.vector(t[,dates])))==FALSE])
c.dates=as.numeric(unlist(as.vector(c[,dates]))[is.na(unlist(as.vector(c[,dates])))==FALSE])
tsdat=data.frame(rbind(cbind(t.dates,0),cbind(c.dates,1)));colnames(tsdat)=c("Date","Status")
tsdat[,1]=tsdat[,1]-1/16 # Recentering the graph


pdf("UnitedTimeSeries.pdf", height=4, width=5.5)
ggplot2.histogram(data=tsdat,xName="Date",groupName="Status",alpha=0.5,binwidth=1/8,brewerPalette="Paired",groupColors=c('goldenrod','royalblue4'))+
geom_vline(xintercept=-1/16)+xlab("")+ylab("Military Disputes Initiated")+xlab("Time")+guides(colour=FALSE)+ geom_rect(aes(xmin = -1.0375, 
xmax = -0.42, ymin = 5.25, ymax = 6.1),fill="white",colour="black") + annotate("text", x = c( -0.706, -0.69), y = c(5.875, 5.525), label = 
c("Barely United", "Barely Divided"), size=c(4, 4)) + theme(plot.title = element_text(lineheight=.8, face="bold",size=14.7),
axis.title=element_text(size=13.7),axis.text.x=element_text(size=13.7),axis.text.y=element_text(size=11.7))+theme_bw()+ 
theme(legend.position="none")+ geom_rect(aes(xmin = -0.9825, xmax = -0.9425, ymin = 5.78, ymax = 5.955),fill="goldenrod",colour="white",alpha=0.01)+
geom_rect(aes(xmin = -0.9825, xmax = -0.9425, ymin = 5.43, ymax = 5.595),fill="royalblue4",colour="white",alpha=0.016)+
scale_x_continuous(breaks=c(-0.5625,0.4375),labels=c("Previous Period","Period in Power"))+scale_y_continuous(breaks=0:6)+
annotate("text",y=4,x=-0.11,label="Election", angle=90,size=5)
dev.off()








pdf("UnitedTimeSeriesSlides.pdf", height=4.5, width=5.5)
ggplot2.histogram(data=tsdat,xName="Date",groupName="Status",alpha=0.5,binwidth=1/8,brewerPalette="Paired",groupColors=c('goldenrod','royalblue4'))+
geom_vline(xintercept=-1/16)+xlab("")+ylab("Military Disputes Initiated")+xlab("Time")+guides(colour=FALSE)+ geom_rect(aes(xmin = -1.0375, xmax = 
-0.42, ymin = 5.25, ymax = 6.1),fill="white",colour="black") + annotate("text", x = c( -0.706, -0.69), y = c(5.875, 5.525), label = 
c("Barely United", "Barely Divided"), size=c(4, 4)) + theme(plot.title = element_text(lineheight=.8, face="bold",size=14.7),
axis.title=element_text(size=13.7),axis.text.x=element_text(size=13.7),axis.text.y=element_text(size=11.7))+theme_bw()+ 
theme(legend.position="none")+ geom_rect(aes(xmin = -0.9825, xmax = -0.9425, ymin = 5.78, ymax = 5.955),fill="goldenrod",colour="white",alpha=0.01)+ 
geom_rect(aes(xmin = -0.9825, xmax = -0.9425, ymin = 5.43, ymax = 5.595),fill="royalblue4",colour="white",alpha=0.016)+
scale_x_continuous(breaks=c(-0.5625,0.4375),labels=c("Previous Period","Period in Power"))+scale_y_continuous(breaks=0:6)+
annotate("text",y=4,x=-0.11,label="Election", angle=90,size=5)+ggtitle("Figure 6: Change in Aggression for Barely\nUnited and Barely Divided Countries")
dev.off()



two_points=close[abs(close$MinDist)<0.02,]

sum(c$HighDisputes*c$Days/365)





# Plots for the Online Appendix

# Non-dem forcing density

pdf("NonDemForcingDensity.pdf", height=4.5, width=7.5)
m <- ggplot(data[data$Type!=4&data$Democracy==0,], aes(x=MinDist*100))
m + geom_histogram(fill="cornflowerblue",
                   binwidth=2, color="black",
                   origin = -140.001)+
  theme_bw()+theme(axis.title = element_text(size=16),plot.title=element_text(size=20))+
  geom_vline(xintercept=0, colour="red")+
  xlab("Percentage of Seats/Votes Away from United Government")+
  ylab("Density") + labs(title="")+
  scale_x_continuous(breaks=seq(-100, 100, 20))+
  scale_y_continuous(breaks=seq(0, 10, 1))
dev.off()



pdf("UnitedBPSlides.pdf", width=7, height=9)
BalancePlot(close,close$United, c("irst","lnirst","milex","lnmilex","milper","lnmilper","pec","lnpec","tpop",
"lntpop","upop","lnupop","PrevUnited","PreviousHighDisputes","PreviousLowDisputes"),
c("Iron and Steel Production","ln(Iron and Steel Production)","Military Expenditures","ln(Military Expenditures)",
"Military Personnel","ln(Military Personnel)","Energy Consumption","ln(Energy Consumption)","Total Population",
"ln(Total Population)","Urban Population","ln(Urban Population)","Previously United", "Previous High-Level Disputes",
"Previous Low-Level Disputes"),Different.Test=RDTest, Built.In.Tests =NULL,na.rm=TRUE,pch=16, Shade.Color="cadetblue2",
Point.Color="Black",Title="Figure 6. Testing for Balance at Cutpoint")
dev.off()

pdf("UnitedBP.pdf", width=6.5, height=6.5)
BalancePlot(close,close$United, c("irst","lnirst","milex","lnmilex","milper","lnmilper","pec","lnpec","tpop",
"lntpop","upop","lnupop","PrevUnited","PreviousHighDisputes","PreviousLowDisputes"),
c("Iron and Steel Production","ln(Iron and Steel Production)","Military Expenditures","ln(Military Expenditures)",
"Military Personnel","ln(Military Personnel)","Energy Consumption","ln(Energy Consumption)","Total Population",
"ln(Total Population)","Urban Population","ln(Urban Population)","Previously United", "Previous High-Level Disputes", 
"Previous Low-Level Disputes"),Different.Test=RDTest, Built.In.Tests =NULL,na.rm=TRUE,pch=16, Shade.Color="cadetblue2",
Point.Color="Black",Title="")
dev.off()



setwd("/Users/andrewbertoli/Dropbox/United Government/BuildData")
nmc=read.csv("NMC_v4_0(1).csv",stringsAsFactors=FALSE)

setwd("/Users/andrewbertoli/Dropbox/United Government/ReplicationCode")
polity=read.csv("Polity.csv",stringsAsFactors=FALSE)

dems=polity[polity$polity>=6,]

dems$index=paste(dems$scode,dems$year,sep=" ")
nmc$index=paste(nmc$stateabb,nmc$year,sep=" ")

alldems=(merge(dems,nmc,by=c("index","index")))

sample=close[abs(close$MinDist)<=0.02,]


pdf("External_Validity.pdf",width=5, height=3)
External_Validity(Sample=sample,Population=alldems,Covs=c("irst","milex","milper","pec","tpop","upop"), 
Names=c("Iron and Steel Production","Military Expenditures","Military Personel","Energy Consumption",
"Total Polulation","Urban Population"),ln=1:6,YLab="ln(Value)",Title="")
dev.off()



pdf("External_Validity2.pdf",width=5, height=4.3)
External_Validity(Sample=sample,Population=alldems,Covs=c("irst","milex","milper","pec","tpop","upop"), 
Names=c("Iron and Steel Production","Military Expenditures","Military Personel","Energy Consumption",
"Total Polulation","Urban Population"),ln=1:6,YLab="ln(Value)",Title="")
dev.off()