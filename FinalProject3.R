## Load Libraries
library(UsingR)
library(ggplot2)
library(dplyr)
library("lubridate")
library(prob)
options(max.print=1000000)
options(scipen = 999)
par(mfrow = c(1,3))
## connecting/using an existing file
setwd("D:/BU/CS544-SUMMER1 2016/Final Project/DataSets")
getwd()

## Read data file CSV

lCandidateSpend = read.csv(file='2016_presidential_candidate_expenditures.csv', header=TRUE)

head(lCandidateSpend)

mode(lCandidateSpend$disb_dt)

dfCandidateSpend = data.frame(lCandidateSpend)
typeof(dfCandidateSpend)
head(dfCandidateSpend,10)

unique(dfCandidateSpend$cand_nm, incomparables = FALSE) ## Listing out all candidates

testdf = data.frame(dfCandidateSpend)
colnames(testdf) <- c("Exp_ID", "Cand_ID","Cand_Name","Party","Recipient_Name",
                       "Distribution_Amount","Distribution_Date", "Recipient_City","Recipient_State",
                       "Recipient_ZIP","Dist_Description","Memo_CD","Memo_Text","Form_TP",
                       "File_NUM","Transaction_ID")

## Cleaning up data, first 
testdf[4] <- lapply(testdf[4], as.character)

d = subset(testdf, Cand_Name %in% c("Sanders, Bernard","Lessig, Lawrence",
                                    "Clinton, Hillary Rodham","O'Malley, Martin Joseph","Webb, James Henry Jr."))

d$Party <- 'Democrat'

r = subset(testdf, Cand_Name %in% c("Rubio, Marco","Paul, Rand","Huckabee, Mike","Graham, Lindsey O.","Christie, Christopher J.",
                                    "Kasich, John R.","Perry, James R. (Rick)","Bush, Jeb","Walker, Scott","Gilmore, James S IIII",
                                    "Carson, Benjamin S.","Santorum, Richard J.","Trump, Donald J.","Cruz, Rafael Edward 'Ted'",
                                    "Fiorina, Carly","Jindal, Bobby"))

r$Party <- 'Republican'
head(r)


g = subset(testdf, Cand_Name == "Stein, Jill")
g$Party <- 'Green Party'

l = subset(testdf, Cand_Name == "Johnson, Gary")
l$Party <- 'Libertarian'

## Combine all 4 dataframes into one, but with party added to the dataframe
allPartyDf=rbind(d,r,g,l, all=TRUE)

## Dropped unnecessary columns

allPartyDf <- allPartyDf[ -c(1,12:17) ]
typeof(allPartyDf)
head(allPartyDf,1)
dim(allPartyDf)
dim(testdf)
## the new dataframe has the same number of columns and rows as the original dataframe (testdf) 
unique(allPartyDf$Cand_Name, incomparables = FALSE)
allPartyDf = dplyr::arrange(allPartyDf, Distribution_Amount)


#################################
## Creating a barplot for the number of transactions for each candidate
transNumber =table(dplyr::select(allPartyDf,Cand_Name))

barplot(transNumber,col = "cyan", ylim=c(0,50000),las=2, cex.names=0.75)

## Hillary by far has the most number of transactions, among all candidates

boxplot(transNumber, col=hcl(0), xaxt = "n", horizontal = TRUE)
axis(side = 1, at = fivenum(transNumber), las=2)

text(fivenum(transNumber), rep(1.2,5), srt=90, adj=0,
     labels=c("Min","Lower Hinge", "Median",
              "Upper Hinge", "Max"))
## Using a boxplot, Hillary's transaction number is an outlier among candidates.
#################################
tail(allPartyDf,5)
## Bernie Sanders campaign payment to Old Towne Media made 4 out of 5 top amount transacations
allPartyDf.avgTrans = allPartyDf %>% na.omit() %>%
  group_by(Cand_Name) %>%
  summarise(avg = mean(Distribution_Amount)) %>%
  arrange(avg) 

utils::View(allPartyDf.avgTrans)

fivenum(allPartyDf.avgTrans$avg)

barplot(allPartyDf.avgTrans$avg, 
        col = "cyan", ylim=c(0,10000),
        ylab = "Avg. Amount",las=2,names.arg =gsub( " .*$", "", allPartyDf.avgTrans$Cand_Name ))
## Senator Sanders' campaign's average transaction amount is more than twice that of Fmr. Secretary Clinton

byCand <- group_by(allPartyDf,Recipient_State,Cand_Name)
( sumCand <- summarize(byCand,CandStateCount=n()) )

byParty <- group_by(allPartyDf,Recipient_State,Party)
( sumParty <- summarize(byParty,PartyStateCount=n()) )


allPartyDf1= allPartyDf %>% group_by(Party) %>% summarise(distMean = mean(Distribution_Amount),
                                                           distSum  = sum(Distribution_Amount),
                                                           distSD   = sd(Distribution_Amount))

allPartyDf2= allPartyDf %>% group_by(Cand_Name) %>% summarise(distMean = mean(Distribution_Amount),
                                                          distSum  = sum(Distribution_Amount),
                                                          distSD   = sd(Distribution_Amount))

barplot(allPartyDf2$distSum, 
        col = "cyan", ylim=c(0,200000000),cex.axis  = 0.65,cex.names = 0.65,
        las=2,names.arg =gsub( " .*$", "", allPartyDf2$Cand_Name ))

allPartyDf3= allPartyDf %>% group_by(Cand_Name,Recipient_State) %>% summarise(distMean = mean(Distribution_Amount),
                                                              distSum  = sum(Distribution_Amount),
                                                              distSD   = sd(Distribution_Amount))


allPartyDf4 = allPartyDf %>% 
  mutate(newDistDate = paste(year(as.Date(parse_date_time(as.character(allPartyDf$Distribution_Date), orders="dmy"))),'-',
                             month(as.Date(parse_date_time(as.character(allPartyDf$Distribution_Date), orders="dmy"))),sep='')) 

allPartyDf5= allPartyDf4 %>% group_by(Cand_Name,Recipient_State,newDistDate) %>% summarise(distMean = mean(Distribution_Amount),
                                                                                                distSum  = sum(Distribution_Amount),
                                                                                                distSD   = sd(Distribution_Amount))


head(allPartyDf,5)
head(allPartyDf1)
head(allPartyDf2)
head(allPartyDf3)
head(allPartyDf4)
head(allPartyDf5)


allPartyDf6 = dplyr::filter(allPartyDf3,Cand_Name == "Clinton, Hillary Rodham")

barplot(allPartyDf6$distSum, 
        col = "cyan", ylim=c(0,80000000),cex.axis  = 0.5,cex.names = 0.5,
        ylab = "Sum. Amount",las=2,names.arg =gsub( " .*$", "", allPartyDf6$Recipient_State))


allPartyDf7 = dplyr::filter(allPartyDf3, Cand_Name == "Sanders, Bernard")

barplot(allPartyDf7$distSum, 
        col = "cyan", ylim=c(0,80000000),
        ylab = "Sum. Amount",las=2,names.arg =gsub( " .*$", "", allPartyDf7$Recipient_State))


tail(filter(allPartyDf, Cand_Name == "Sanders, Bernard", Recipient_State =="DC"),5)
################### Normal Distribution
################### Comparing Sanders and Clinton

allPartyDf8 = allPartyDf
head(allPartyDf8)
xSanders = allPartyDf8 %>% select(Distribution_Amount,Cand_Name) %>%
    arrange(Distribution_Amount) %>% dplyr::filter(Cand_Name == "Sanders, Bernard")
xSandersNorm = xSanders %>%  select(Distribution_Amount)

xSandersNorm = as.numeric(unlist(xSandersNorm))
xSandersMean = mean(xSandersNorm)
xSandersSd = sd(xSandersNorm)
xSandersSd
xSandersPdf = dnorm(xSandersNorm,mean=xSandersMean,sd=xSandersSd)
head(xSandersPdf)

plot(xSandersNorm, xSandersPdf, type="h", main="Normal Distribution - Sanders",
     col="lightblue", xaxt="n", yaxt="n")

xSandersMean
xSandersSd

xSandersPdf.1 = dnorm(xSandersNorm, mean = xSandersMean, sd = (xSandersSd*0.5))
xSandersPdf.2 = dnorm(xSandersNorm, mean = xSandersMean, sd = (xSandersSd*0.75))
xSandersPdf.3 = dnorm(xSandersNorm, mean = xSandersMean, sd = (xSandersSd*1))

plot(xSandersNorm, xSandersPdf.1, type="l", col="green", xlim=c(-200,200000))
lines(xSandersNorm, xSandersPdf.2, col="red")
lines(xSandersNorm, xSandersPdf.3, col="blue")

## The cumulative probability of Sanders Campaign spend within the mean value of $3246
xSandersCdf = pnorm(xSandersMean, mean=xSandersMean, sd= xSandersSd)
## xClintonCdf is 0.5 - shows 50% of the spend are within the mean value

## The probability of spend value being less than the 3rd standard deviation is 0.00135

pnorm(xSandersMean - 3* xSandersSd, mean=xSandersMean, sd= xSandersSd)

## The probability that the spend is within 3 standard deviations is 0.9973
(pnorm(xSandersMean + 3* xSandersSd, mean=xSandersMean, sd= xSandersSd)) -
  (pnorm(xSandersMean - 3* xSandersSd, mean=xSandersMean, sd= xSandersSd))

## The probability that the spend is within 2 standard deviations is 0.9545
(pnorm(xSandersMean + 2* xSandersSd, mean=xSandersMean, sd= xSandersSd)) -
  (pnorm(xSandersMean - 2* xSandersSd, mean=xSandersMean, sd= xSandersSd))

## The probability that the spend is within 1 standard deviation is 0.6827
(pnorm(xSandersMean + 1* xSandersSd, mean=xSandersMean, sd= xSandersSd)) -
  (pnorm(xSandersMean - 1* xSandersSd, mean=xSandersMean, sd= xSandersSd))
## This value shows that 68.27% of the spend is within 1 standard deviation.
xSandersCdf = pnorm(xSandersNorm, mean=xSandersMean, sd= xSandersSd)
xSandersCdf

plot(xSandersNorm, xSandersCdf, type="l", col="red", 
     xlim=c(0,200000), ylim=c(0,1),
     xaxt="n",
     main="Sanders Spend CDF", xlab="Dollars", ylab="CDF")
abline(h=0)
axis(side = 1, at = c(0,100000,200000,300000,400000,500000), 
     labels = TRUE) 









####################################################################################
xClinton = allPartyDf8 %>% select(Distribution_Amount,Cand_Name) %>%
  arrange(Distribution_Amount) %>% dplyr::filter(Cand_Name == "Clinton, Hillary Rodham")
xClintonNorm = xClinton %>%  select(Distribution_Amount)

xClintonNorm = as.numeric(unlist(xClintonNorm))
xClintonMean = mean(xClintonNorm)
xClintonSd = sd(xClintonNorm)
xClintonSd
xClintonPdf = dnorm(xClintonNorm,mean=xClintonMean,sd=xClintonSd)
head(xClintonPdf)

plot(xClintonNorm, xClintonPdf, type="h", main="Normal Distribution - Clinton",
     col="lightblue", xaxt="n", yaxt="n")

xClintonMean
xClintonSd

xClintonPdf.1 = dnorm(xClintonNorm, mean = xClintonMean, sd = (xClintonSd*0.5))
xClintonPdf.2 = dnorm(xClintonNorm, mean = xClintonMean, sd = (xClintonSd*0.75))
xClintonPdf.3 = dnorm(xClintonNorm, mean = xClintonMean, sd = (xClintonSd*1))

plot(xClintonNorm, xClintonPdf.1, type="l", col="green", xlim=c(-200,200000))
lines(xClintonNorm, xClintonPdf.2, col="red")
lines(xClintonNorm, xClintonPdf.3, col="blue")

## The cumulative probability of Clinton Campaign spend within the mean value of $3246
xClintonCdf = pnorm(xClintonMean, mean=xClintonMean, sd= xClintonSd)
## xClintonCdf is 0.5 - shows 50% of the spend are within the mean value

## The probability of spend value being less than the 3rd standard deviation is 0.00135
pnorm(xClintonMean - 3* xClintonSd, mean=xClintonMean, sd= xClintonSd)

## The probability that the spend is within 3 standard deviations is 0.9973
(pnorm(xClintonMean + 3* xClintonSd, mean=xClintonMean, sd= xClintonSd)) -
     (pnorm(xClintonMean - 3* xClintonSd, mean=xClintonMean, sd= xClintonSd))
 
## The probability that the spend is within 2 standard deviations is 0.9545
(pnorm(xClintonMean + 2* xClintonSd, mean=xClintonMean, sd= xClintonSd)) -
  (pnorm(xClintonMean - 2* xClintonSd, mean=xClintonMean, sd= xClintonSd))

## The probability that the spend is within 1 standard deviation is 0.6827
(pnorm(xClintonMean + 1* xClintonSd, mean=xClintonMean, sd= xClintonSd)) -
  (pnorm(xClintonMean - 1* xClintonSd, mean=xClintonMean, sd= xClintonSd))
## This value shows that 68.27% of the spend is within 1 standard deviation.
xClintonCdf = pnorm(xClintonNorm, mean=xClintonMean, sd= xClintonSd)
xClintonCdf

plot(xClintonNorm, xClintonCdf, type="l", col="red", 
     xlim=c(0,200000), ylim=c(0,1),
     xaxt="n",
     main="Clinton Spend CDF", xlab="Dollars", ylab="CDF")
abline(h=0)
axis(side = 1, at = c(0,100000,200000,300000,400000,500000), 
     labels = TRUE) 


################### Normal Distribution
################### Comparing Sanders and Clinton and All

xAll = allPartyDf8 %>% select(Cand_Name,Distribution_Amount) %>%
  arrange(Distribution_Amount) 
xAllNorm = xAll %>%  select(Distribution_Amount)

xAllNorm = as.numeric(unlist(xAllNorm))
xAllMean = mean(xAllNorm)
xAllSd = sd(xAllNorm)
xAllSd
xAllPdf = dnorm(xAllNorm,mean=xAllMean,sd=xAllSd)
head(xAllPdf)
tail(xAll,5)

plot(xAllNorm, xAllPdf, type="h", main="Normal Distribution - All Candidates",
     col="lightblue", xaxt="n", yaxt="n")

xAllMean
xAllSd

xAllPdf.1 = dnorm(xAllNorm, mean = xAllMean, sd = (xAllSd*0.5))
xAllPdf.2 = dnorm(xAllNorm, mean = xAllMean, sd = (xAllSd*0.75))
xAllPdf.3 = dnorm(xAllNorm, mean = xAllMean, sd = (xAllSd*1))

plot(xAllNorm, xAllPdf.1, type="l", col="green", xlim=c(-200,200000))
lines(xAllNorm, xAllPdf.2, col="red")
lines(xAllNorm, xAllPdf.3, col="blue")

## The cumulative probability of All Campaign spend within the mean value of $3246
xAllCdf = pnorm(xAllMean, mean=xAllMean, sd= xAllSd)
## xClintonCdf is 0.5 - shows 50% of the spend are within the mean value

## The probability of spend value being less than the 3rd standard deviation is 0.00135
pnorm(xAllMean - 3* xAllSd, mean=xAllMean, sd= xAllSd)

## The probability that the spend is within 3 standard deviations is 0.9973
(pnorm(xAllMean + 3* xAllSd, mean=xAllMean, sd= xAllSd)) -
  (pnorm(xAllMean - 3* xAllSd, mean=xClintonMean, sd= xClintonSd))

## The probability that the spend is within 2 standard deviations is 0.9545
(pnorm(xAllMean + 2* xAllSd, mean=xAllMean, sd= xAllSd)) -
  (pnorm(xAllMean - 2* xAllSd, mean=xAllMean, sd= xAllSd))

## The probability that the spend is within 1 standard deviation is 0.6827
(pnorm(xAllMean + 1* xAllSd, mean=xAllMean, sd= xAllSd)) -
  (pnorm(xAllMean - 1* xAllSd, mean=xAllMean, sd= xAllSd))
## This value shows that 68.27% of the spend is within 1 standard deviation.
xAllCdf = pnorm(xAllNorm, mean=xAllMean, sd= xAllSd)
head(xAllCdf)

plot(xAllNorm, xAllCdf, type="l", col="red", 
     xlim=c(0,200000), ylim=c(0,1),
     xaxt="n",
     main="All Spend CDF", xlab="Dollars", ylab="CDF")
abline(h=0)
axis(side = 1, at = c(0,100000,200000,300000,400000,500000), 
     labels = TRUE) 



###################################################################
################         Central Limit Theorem
##################################################################

## Draw various random samples of the data and show the applicability of the
## Central Limit Theorem for this variable.
xClintonRNorm = rnorm(xClintonNorm, mean=xClintonMean, sd= xClintonSd)
xSandersRNorm = rnorm(xSandersNorm, mean=xSandersMean, sd= xSandersSd)
xAllRNorm = rnorm(xAllNorm, mean=xAllMean, sd= xAllSd)

hist(xClintonRNorm, prob = TRUE, xlim=c(-500000,500000), main = "Clinton - RNORM")
hist(xSandersRNorm, prob = TRUE, xlim=c(-500000,500000), main = "Sanders - RNORM")
hist(xAllRNorm, prob = TRUE, xlim=c(-500000,500000), main = "All Candidates - RNORM")
###############################
## Draw samples
head(allPartyDf)
par(mfrow = c(1,3))

samples100 <- 100
xbar100 <- numeric(samples100)

for (i in 1: samples100) {
  xbar100[i] <- mean(rnorm(xAllNorm, 
                        mean = xAllMean, sd = xAllSd))
}

hist(xbar100, prob = TRUE,breaks = 15, xlim=c(3000,5000), ylim = c(0, 0.005),main = "100 Samples")


asamples1000 <- 1000
xbar1000 <- numeric(samples1000)

for (i in 1: samples1000) {
  xbar1000[i] <- mean(rnorm(xAllNorm, 
                        mean = xAllMean, sd = xAllSd))
}

hist(xbar1000, prob = TRUE,breaks = 15, xlim=c(3000,5000), ylim = c(0, 0.005),main = "1000 Samples")

samples2000 <- 2000
xbar2000 <- numeric(samples2000)
for (i in 1: samples2000) {
  xbar2000[i] <- mean(rnorm(xAllNorm, 
                        mean = xAllMean, sd = xAllSd))
}



hist(xbar2000, prob = TRUE,breaks = 15, xlim=c(3000,5000), ylim = c(0, 0.005),main = "2000 Samples")

##########################################################
###############      Confidence Interval
##########################################################
## For confidence levels of 80 and 90, show the confidence intervals of the mean
## of the numeric variable for various samples and compare against the
## population mean.

xSandersMean
xClintonMean
xAllMean

xSandersSd
xClintonSd
xAllSd
#################################################

xbar = xAllMean
xbar



for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qnorm(1-i/2) * xAllSd,
                 xbar + qnorm(1-i/2) * xAllSd)
  cat(str,"\n")
}

####################################################################
xSandersSd
xbar = xSandersMean
xbar



for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qnorm(1-i/2) * xSandersSd,
                 xbar + qnorm(1-i/2) * xSandersSd)
  cat(str,"\n")
}

################################################################
xClintonSd
xbar = xClintonMean
xbar



for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qnorm(1-i/2) * xClintonSd,
                 xbar + qnorm(1-i/2) * xClintonSd)
  cat(str,"\n")
}

##############################################################


