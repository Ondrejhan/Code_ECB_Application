
# The following code compares & visualizes Spanish employment data (2007-2011) from three different sources 
# (EU-SILC, Eurostat-LFS, OECD). After computing the relevant variables, the resulting plot shows differences
# in the unemployment levels from the three sources and underscores the importance of consistent data 
# collection, validation & aggregation.

setwd("INSERT")
getwd()

#no external packages required

#load relevant data [provided in the repository]
load("datdl.Rdat") #EU-SILC
load("dathl.Rdat") #EU-SILC
load("datpl.Rdat") #EU-SILC
load("datrl.Rdat") #EU-SILC
load("OECDdata.Rdat") #OECD
load("LFSdata.Rdat") #LFS

p <- datpl
d <- datdl
r <- datrl
h <- dathl

rm(datpl,datdl,datrl,dathl)

# data preparation of EUSILC timeseries
p2 <- p
act210 = paste("PL210",LETTERS[1:12],sep="")
act211 = paste("PL211",LETTERS[1:12],sep="")
for (var in act210) {
  p2[,var] = factor(p2[,var],levels=1:9)
  levels(p2[,var])[1:9] = c(1,1,1,1,2,4,4,4,4)
}
for (var in act211) {
  p2[,var] = factor(p2[,var],levels=1:11)
  levels(p2[,var])[1:11] = c(1,1,1,1,2,4,4,4,4,4,4)
}

allmissing210 = rowSums(is.na(p[,act210]))==12
allavail211 = rowSums(!is.na(p[,act211]))==12
use211 = which(allmissing210 & allavail211)

p2[use211,act210] = p2[use211,act211]
skip =na.action(na.omit(p2[,act210]))
nskip=length(skip); nkeep = nrow(p2)-nskip;

p2 = p2[-skip,]
p2 = merge(p2,d,by.x=c("PB010","PX030"),by.y=c("DB010","DB030"))
p2 = merge(p2,h[,c("HB010","HB030","HB050",
                   "HH010","HH020","HH021","HH030","HH031","HH050","HX040")],
           by.x=c("PB010","PX030"),by.y=c("HB010","HB030"))

p2$UNE = rowSums(p2[,act210]==2)
p2$EMPL = rowSums(p2[,act210]==1)
p2$OWN = 0+(p2[,"HH020"]==1 | p2[,"HH020"]==4)
p2$PL030=factor(p2$PL030,levels=1:9)

levels(p2$PL030)[1:9] = c(1,1,2,4,4,4,4,4,4)
p2$PL031=factor(p2$PL031,levels=1:11)
levels(p2$PL031)[1:11] = c(1,1,1,1,2,4,4,4,4,4,4)
p2$PL030[which(p2$PL030_F==-5)] = p2$PL031[which(p2$PL030_F==-5)]

p2 <- p2[-which(is.na(p2$PL030)),]
n=nrow(p2)

wa = (p2$PX020>=15 & p2$PX020<=64) # working age, TRUE or FALSE
emp = (p2$PL030==1) # employed, TRUE or FALSE
une = (p2$PL030==2) # unemployed, TRUE or FALSE
s = is.element(p2$PL020,1) # actively searching, TRUE or FALSE
a = is.element(p2$PL025,1) # available for work, TRUE or FALSE

une.rate=rep(NA,4) # yearly unemployment rates (based on PL030)
une2.rate=rep(NA,4) # yearly unemployment rates (based on PL210x)
une3.rate=rep(NA,48) # monthly unemployment rates (based on PL210x)
ep.rate=rep(NA,4) # yearly employment rates (=empl/population ratio)
lfp.rate=rep(NA,4) # yearly labor-force participation rates

#loop for annual data
for (year in 2008:2011) {
  y = (p2$PB010==year) # index rows of a particular year only via true/false
  une.rate[year-2007] = (sum(y & wa & une & s)/
                           ( sum(y & wa & une & s) + sum(y & wa & emp)))
  une2.rate[year-2007] = (sum(p2[y&wa&s,act210]==2)/
                            (sum(p2[y&wa&s,act210]==2) + sum(p2[y&wa,act210]==1)))
}

#loop for monthly data
for (m in 1:48) {
  year = 2007+ceiling(m/12)
  mo = m%%12 # month within year "%%" modulo division (shows rest only)
  if (mo==0) mo=12; # if ../12 rest is 0 it means month=december
  y = (p2$PB010==year) # index rows of a particular year via true/false
  une3.rate[m] = (sum(p2[y&wa&s,act210[mo]]==2) /
                    (sum(p2[y&wa&s,act210[mo]]==2) + sum(p2[y&wa,act210[mo]]==1)))
}

# transform into time series for easier plotting
une.rate = ts(une.rate,frequency=1,start=2008) # PL030 refers to time of interview
une2.rate = ts(une2.rate,frequency=1,start=2007) # PL210x refer to IRP
une3.rate = ts(une3.rate,frequency=12,start=c(2007,1)) # PL210x refer to IRP
uneOECD.rate = ts(OECDdata$unemplrat, frequency=12, start=c(2007,1))
uneLFS.rate = ts(LFSdata$Spain, frequency = 12, start=c(2007,1))

par(mar=c(2,3,0.5,0.5),oma=c(0,0,0,0.7),
    mgp=c(1.8,0.5,0),las=1,tcl=-0.2,yaxs="i")
plot(100*une.rate,ylim=c(0,25),type="o",xlim=c(2007,2011),
     xlab="",ylab="unemployment rate",lwd=2,pch=0)
grid()

lines(100*une2.rate,type="b",col=2,lty=1,lwd=2,pch=0)
lines(100*une3.rate,type="b",col=2,lty=2,lwd=2,pch=20)
lines(uneOECD.rate, type="b",col=4,lty=2,lwd=1,pch=20)
lines(uneLFS.rate, type="l",col=6,lty=2,lwd=2,pch=0)
legend("bottomright",c("EUSILC unemployment PL030","EUSILC unemployment PL210x annual", "EUSILC unemployment PL210x monthly", 
                       "OECD unemployment monthly", "Eurostat LFS unemployment monthly"),
       lty=c(1,1,0,0,2),pch=c(0,0,20,20,0),col=c(1,2,2,4,6),lwd=2,bty="n",seg.len=3)

# The plot displays five time series of national unemployment rates for Spain (2007-2011). 
# The datasets are: the Eurostat Income and Living Conditions Database (EU SILC Data); the
# Eurostat Employment and Unemployment Database (LFS Data); the OECD Employment Database.

# ------------------------------------------------------------------------------
#           mobility rate of unemployed
# ------------------------------------------------------------------------------

require(plm)
temp = pdata.frame(p2[,c("PB030","PB010","OWN","UNE","EMPL",
                         "DB040","DB100","DB110")],
                   index=c("PB030","PB010"),stringsAsFactors=FALSE,row.names=FALSE)
# index is a vector of two variables: 1. individual ID, 2. time
# resulting data.frame is sorted by first ID and then time

m1 = (temp$DB040!=lag(temp$DB040))
m2 = (temp$DB100!=lag(temp$DB100)); m2[is.na(m2)] = FALSE;
# hh status 2: hh has moved since last survey, 8: split-off from former hh
m3 = (temp$DB110==2 | temp$DB110==8)
#  create new variable to indicate geographical move of person
temp$MOVE = m1 | m2 | m3; # if any of the above conditions applies
temp$MOVE[is.na(temp$MOVE)] = 0 # replace NA values with 0
temp$OWN.L1 = lag(temp$OWN) # take ownership from last year as separate variable
# for later use add MOVE-info to p2-file
p2 = merge(p2,temp[,c("PB010","PB030","MOVE","OWN.L1")],by=c("PB010","PB030"))
# for illustration running simple regressions for mobility (linear+probit)
summary(lm(MOVE~OWN.L1+PB010,data=temp,subset=(temp$UNE>0)))
summary(glm(MOVE~OWN.L1+PB010,data=temp,subset=(temp$UNE>0),family="binomial"))

# plot mobility rate of have-been-unemployed and unemployment
mob=matrix(NA,5,2) # 3 rows, 2 columns, fill with NA
rownames(mob) = 2007:2011
colnames(mob) = c("MOB.RATE UNE","UNEMPL.RATE")
for (year in 2007:2011) {
  ri = (temp$PB010==year) & (temp$UNE>0) # row indices of obs with une in year...
  mob[as.character(year),1] = 100 * sum(temp$MOVE[ri]) / sum(temp$UNE[ri]>0)
  ri = (temp$PB010==year)  # row indices of obs in year...
  mob[as.character(year),2] = 100 * sum(temp$UNE[ri]) / (sum(temp$UNE[ri]) + sum(temp$EMPL[ri]))
}
# plotting mobility rate and unemployment rate
par(mar=c(2,3,0.5,2.5))
plot(2007:2011,mob[,1],type="b",lwd=2,ylim=c(0,4),pch=16,ylab="mobility rate of une")
par(new=TRUE)
plot(2007:2011,mob[,2],type="b",lwd=2,axes=FALSE,bty="n",col=2,lty=2,pch=17,ylim=c(11,21),ylab="")
axis(side=4, at = pretty(c(11:21)),col.axis=2)# separate axis on right
mtext(side=4, "unemployment rate",line=1.7,las=0,col=2)
grid()

#The last plot shows the rising unemployment rates from the first plot as well as the mobility
#rate.

