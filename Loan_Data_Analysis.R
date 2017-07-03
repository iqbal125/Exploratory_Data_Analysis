

setwd("C:/Users/Mohammad/Desktop/Disk 0/Code/Data Analyst Udacity/Project 4")

data <- read.csv('prosperLoanData.csv', na.strings=c("", "NA"))

library(knitr)
library(dplyr)
library(ggplot2)





#This report explores loan amounts from prosper. This data contains 113937 observations with 81 variables.
#One of the most important parts of studying loan data would be to 
#know what makes it likely for someone to pay their loan back. This is what we will explore in this report



#We are looking at the structure of the Data
str(data)

#Getting an overview of the data
summary(data)

#Another basic overview of our data which gives us the first five observations in a column
head(data)

#Gives us all the Column names. This will be a helpful reference throughout the report.
colnames(data)

#This will also serve as a helpful reference for making quick calculations
total_observations <- 113937




#Since, We are investigating loan repayment, we will look closer at the LoanStatus Column
#We see that more than half have current or completed loans and less than 5% have defaulted their loan. 
#5018 have defaulted on their loan. We can investigate these people further and see if they have any common 
#characteristics that may be correlated with the loan default. 
#We also have 0 people with a Past Due loan status. This seems very strange and is worth looking into 
#in the next section.  
#Interesting to note that PAst_Due_Total is much less than Defaulted Loan Status
count(subset(data['LoanStatus'], LoanStatus == 'Completed'))
count(subset(data['LoanStatus'], LoanStatus == 'Current'))
count(subset(data['LoanStatus'], LoanStatus == 'Defaulted'))
count(subset(data['LoanStatus'], LoanStatus == 'Chargedoff'))


PD1 <- count(subset(data["LoanStatus"], LoanStatus == "Past Due (>120 days"))
PD2 <- count(subset(data['LoanStatus'], LoanStatus == 'Past Due (1-15 days)'))
PD3 <- count(subset(data['LoanStatus'], LoanStatus == 'Past Due (16-30 days)'))
PD4 <- count(subset(data['LoanStatus'], LoanStatus == 'Past Due (31-60 days)'))
PD5 <- count(subset(data['LoanStatus'], LoanStatus == 'Past Due (61-90 days)'))
PD6 <- count(subset(data['LoanStatus'], LoanStatus == 'Past Due (91-120 days)'))

Past_Due_Total <- PD1 + PD2 + PD3 + PD4 + PD5 + PD6

Past_Due_Total


#Basic statistics on the lower range of Credit Scores
#This is a basic plot showing distribution of credit scores. The vast majority of people have scores with 500-800
Credit_Score_Lower <- data['CreditScoreRangeLower']
summary(Credit_Score_Lower)
qplot(x=Credit_Score_Lower, binwidth=100)


#Lets see how the lower range compares to the Upper range of credit scores
#These two variables are not that different
#The Credit_Score_Upper variable contains values that are on average only 19 points high than the Credit_Score_Lower
Credit_Score_Upper <- data['CreditScoreRangeUpper']
summary(Credit_Score_Upper)
qplot(x=Credit_Score_Upper, binwidth=100)


#Another Important factor to keep in mind would be someone's occupation
#We can explore if someone's job would affect their loan repayment ability
Job <- data['Occupation']
summary(Job)
occupations <- as.data.frame(table(data$Occupation))
qplot(data=occupations,x=Var1,y=Freq)
qplot(data = subset(occupations, Freq > 1000), x=Var1,y=Freq) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#We see that 32% of our population has had a delinquincy in the last 7 years.
#This will be important as we explore further and try to answer our original question
DelinquinceiesLast7Years <- data['DelinquenciesLast7Years']
DelinquinceiesLast7Years
summary(DelinquinceiesLast7Years)
delinquincie_in_last_7years <- count(subset(DelinquinceiesLast7Years, DelinquinceiesLast7Years > 0))
percent_with_delinquincy <- delinquincie_in_last_7years/total_observations
percent_with_delinquincy


#Another important thing to explore would be someone's monthly income
#Someone with a higher income should be better at being able to repay a loan. 
#We will keep this question in mind, when doing deeper analysis further in the report. 
#We would also like to know if the income was verified to forsee any potential problems
#Less than 10% of the population dont have their income verified
MonthlyIncome <- data['StatedMonthlyIncome']
summary(MonthlyIncome)
IncomeVerified <- data['IncomeVerifiable']
summary(IncomeVerified)
count(subset(IncomeVerified, IncomeVerified == "False"))

#Only 2131 people had investments made by friends.
#We can explore if this had an affect on their repayment of the loan, later in the report
Number_of_Friends_Invested <- data['InvestmentFromFriendsCount']
head(Number_of_Friends_Invested)
summary(Number_of_Friends_Invested)
More_Than_1_Friend_Investor <- subset(Number_of_Friends_Invested, InvestmentFromFriendsCount > 0)
count(More_Than_1_Friend_Investor)

#Looking at the summary for borrower's ProsperScore, we find a fairly normal distribution.
#There are also almost 30,000 borrowers that dont have a Prosper Score. 
#This will be important to keep in mind. 
ProsperScore <- data['ProsperScore']
ggplot(data, aes(ProsperScore)) + geom_bar()

#Another variable we can consider is Borrowers intrest rate.
#Would a higher Interest rate lead to more defualts? 
#We can explore this question in the next section
#We also have Borrrower Rate and BOorrower APR.
#These two variables usually refer to the same thing
#They also have similar values.We will try to investigate why they are being treated as two different variables.
BorrowerAPR <- data['BorrowerAPR']
summary(BorrowerAPR)
BorrowerRate <- data['BorrowerRate']
summary(BorrowerRate)
qplot(BorrowerRate)
qplot(BorrowerAPR)

#Another variable to consider could be IsHomeOwner. 
#Being a homeowner could mean someone is responsible and therefore better able to pay back a loan.
HomeOwner <- data['IsBorrowerHomeowner']
summary <- HomeOwner
str(HomeOwner)
head(HomeOwner)
count(subset(HomeOwner, HomeOwner == "True"))

#Listing Category would be interesting to explore. Most borrowers have a catergory so this will provide useful information.
#We can see if the type of loan affects repayment. 
#Even with a basic plot, we can see that the vast majority of people chose "Debt Consolidation" as their category, 
#which is represented by the number 1 in the plot. 
ListingCategory <- data['ListingCategory..numeric.']
summary(ListingCategory)
head(ListingCategory)
count(subset(ListingCategory, ListingCategory > 0))
qplot(ListingCategory, binwidth=1)


#We can also look at recommendations and see if they have any effect on loan replayment. 
#There are only 4259 borrowers who recieved a recommendation, so this might not be a viable path to go down. 
Recommendations <- data['Recommendations']
head(Recommendations)
summary(Recommendations)
count(subset(Recommendations, Recommendations > 0))



#Another Interesting point of study could be Credit Grade.
#but considering there are 84984 missing values we may have to pass this up.
#The data is very uniform however, so it might be useful in later analysis.
CreditGrade <- data['CreditGrade']
summary(CreditGrade)
ggplot(data, aes(CreditGrade)) + geom_bar()

#We will continue our investigation of the variables and see if we find anything interesting



#Intersting point about this dataset is the average estimated loss on a loan is 8%. 
Estimated_loss <- data['EstimatedLoss']
summary(Estimated_loss)
head(Estimated_loss)
qplot(Estimated_loss)


#The average estimated return is 9%. Are they making an estimated average 1% profit on these loans?  
Estimated_return <- data['EstimatedReturn']
summary(Estimated_return)
head(Estimated_return)
qplot(Estimated_return)


#Im not that fimiliar with the finance world, but I was surprised to see the average number of credit lines was 10. 
#This seems like a lot. 
Credit_Lines <- data['CurrentCreditLines']
summary(Credit_Lines)
head(Credit_Lines)
qplot(Credit_Lines)


#Average open Credit Lines are 9.26. This is consistant with the last variable.
Open_Credit_Lines <- data['OpenCreditLines']
summary(Open_Credit_Lines)
head(Open_Credit_Lines)
qplot(Open_Credit_Lines)

#Almost 27 credit lines opened in last 7 years on average. WOW.
Total_Credit_Lines_7 <- data['TotalCreditLinespast7years']
summary(Total_Credit_Lines_7)
head(Total_Credit_Lines_7)
qplot(Total_Credit_Lines_7)


#THis is similar to credit lines. Nothing noteworthy
Revolv_Accounts <- data['OpenRevolvingAccounts']
summary(Revolv_Accounts)
head(Revolv_Accounts)
qplot(Revolv_Accounts)

# $271 median monthly payment. This is a big monthly expense.   
Revolv_Accounts_amount <- data['OpenRevolvingMonthlyPayment']
summary(Revolv_Accounts_amount)
head(Revolv_Accounts_amount)
qplot(Revolv_Accounts_amount)

#24000 people currently have a delinquincy. This variable will be important in later analysis. 
Current_Delinquincies <- data['CurrentDelinquencies']
summary(Current_Delinquincies)
head(Current_Delinquincies)
qplot(Current_Delinquincies)
count(subset(Current_Delinquincies, Current_Delinquincies > 0))

#24000 people have delinquicies but only 16500 have an amount due, seems strange. 
Amount_Delinquent <- data['AmountDelinquent']
summary(Amount_Delinquent)
head(Amount_Delinquent)
qplot(Amount_Delinquent)
count(subset(Amount_Delinquent, Amount_Delinquent > 0))

#Nothing jumps out immediately, excpet for the poor guy who has monthly expenses that is 10 times his monthly income.
#We will explore this variable further in the report. 
Debt_to_income_ratio <- data['DebtToIncomeRatio']
summary(Debt_to_income_ratio)
head(Debt_to_income_ratio)
qplot(Debt_to_income_ratio)

#Check that there is only one loan per member
members <- data['MemberKey']
summary(members)
head(members)
qplot(members)
count(members)




#Bivariate Analysis

Loan_Status <- data['LoanStatus']
Defaulted <- subset(data['LoanStatus'], LoanStatus == 'Defaulted')
Defaulted_Total <- count(subset(data['LoanStatus'], LoanStatus == 'Defaulted'))
Current_Delinquincies_Count <- count(subset(data['CurrentDelinquencies'], CurrentDelinquencies > 0))


#Lets start of easy and see if current deliquincies has any bearing on 
#whether or not the loan was defaulted

#As previously mentioned there are 5018 people that defaulted on their loan and 23498 with 
#delinquencies
Defaulted_Total
Current_Delinquincies_Count


#Here we see that 40% of people of with a current delinquincy have defaulted on their loan
#Over 99% of people with a past due have a current delinquincy
df1 <- data.frame(Current_Delinquincies, Loan_Status)
Defaulted_Current_Delinquincies <- subset(df1, LoanStatus == 'Defaulted' & Current_Delinquincies > 0)
num_defaulted_current_delinquincy <- count(Defaulted_Current_Delinquincies)
percent_defaulted_current_delinquincy <- num_defaulted_current_delinquincy/Defaulted_Total
percent_defaulted_current_delinquincy
percent_past_due_current_delinquincy <- num_defaulted_current_delinquincy/Past_Due_Total
percent_past_due_current_delinquincy



#Next we will try to answer the question posed in our first section. Does a higher interest rat
#lead to more delinquencies? . This is an interesting find, lets keep going. 
ggplot(data, aes(x=BorrowerAPR, y=Current_Delinquincies)) + geom_jitter()



#Another question we posed was whether Prosper Score could determine
#if someone would be less likely to be delinquent. Lets keep going.
ggplot(data, aes(x=ProsperScore, y=Current_Delinquincies)) + geom_jitter()



#Another question we asked was if Listing Category made a difference in delinquincies or
#not. This does not seem to be the case.
ggplot(data, aes(x=ListingCategory, y=Current_Delinquincies)) + geom_jitter()



#Credit Score did show a trend however. The trend shows that people with 
#higher credit scores do tend to have less delinquincies.  
ggplot(data, aes(x=Credit_Score_Upper, y=Current_Delinquincies)) + geom_jitter()
ggplot(data, aes(x=Credit_Score_Lower, y=Current_Delinquincies)) + geom_jitter()


#Lets now look at past delinquincies and see if it had any effect on current delinquincies.
ggplot(data, aes(x=DelinquinceiesLast7Years, y=Current_Delinquincies)) + geom_jitter()


#Looking at the data it is clear monthly income does have an affect on delinquencies
#There are less delinquencies as income increases. We also Choose a stated monthly income of 
#less than 150000 to get rid of outliers
df2 <- data.frame(Current_Delinquincies, MonthlyIncome)
#Current_Delinq_Monthly_Income <- subset(df2, MonthlyIncome < 250000)
Current_Delinq_Monthly_Income <- subset(df2, MonthlyIncome < 15000)
ggplot(Current_Delinq_Monthly_Income, aes(x=StatedMonthlyIncome, y=CurrentDelinquencies)) + geom_jitter()


#There is no corelation between being a homeowner and making good loan payments.
#Around 50% of homeowners have a delinquency
HomeOWner
df3 <- data.frame(Current_Delinquincies, HomeOWner)
is_homeowner_delinquency <- subset(df3, Current_Delinquincies > 0)
count(is_homeowner_delinquency)


#The number of credit lines may have an effect on loan repayment, 
#People with less credit lines have a larger population which may 
#explain the trend. We can explore this further.
ggplot(df3, aes(x=Credit_Lines, y=Current_Delinquincies)) + geom_jitter()

#Debt to income does not seem to have an effect on delinquencies, expect for people with a 
#Income to debt ratio of 10. 
ggplot(data, aes(x=Debt_to_income_ratio, y=Current_Delinquincies)) + geom_jitter()



#Multi Variate analysis


#We will go deeper in our Credit Score analysis and compare Delinquencies and credit scores in groups.
#There should be a clear trend of people with higher credit scores having lower delinquencies


breaks1 <- c(0, 650, 750, 1000)
df5 <- cut(data$CreditScoreRangeLower, breaks=breaks1, labels=c('Low','Medium','High'))
ggplot(data, aes(x=Credit_Score_Lower, y=Current_Delinquincies)) + geom_step(aes(color=df5))

Low_count <- count(subset(Credit_Score_Lower, Credit_Score_Lower < 650))
Mid_count <- count(subset(Credit_Score_Lower, Credit_Score_Lower > 650 & Credit_Score_Lower < 750))
High_count<- count(subset(Credit_Score_Lower, Credit_Score_Lower > 750))

percent_low_credit <- Low_count/total_observations
percent_low_credit

percent_mid_credit <- Mid_count/total_observations
percent_mid_credit

percent_high_credit <- High_count/total_observations
percent_high_credit

#After Running the Code we do in fact see a clear trend here. 


#We can also explore if being a homeowner and credit score affect delinquencies
ggplot(data, aes(x=Credit_Score_Lower, y=Current_Delinquincies)) + geom_point() + facet_grid(data$IsBorrowerHomeowner ~ .)




#We can know take a deeper look at the monthly incomes
breaks2 <- c(0, 1150, 4300, 10000, 30000, 50000)
df8 <- data.frame(Current_Delinquincies, MonthlyIncome)
Current_Delinq_Monthly_Income <- subset(df8, MonthlyIncome < 50000)
df9 <- cut(Current_Delinq_Monthly_Income$StatedMonthlyIncome, breaks=breaks2)
ggplot(Current_Delinq_Monthly_Income, aes(x=StatedMonthlyIncome, y=CurrentDelinquencies)) + geom_jitter(aes(color=df9))


#There is clearly a trend and corealtion between income and delinquencies. As income increases
#The number of delinquencies does decrease. 


#Now lets go deeper with Debt to Income and see if we can find anything
breaks3 <- c(0, 0.25, 0.50, 0.75)
df9 <- data.frame(Current_Delinquincies, Debt_to_income_ratio)
Current_Delinq_Debt <- subset(df9, Debt_to_income_ratio < 1)
df10 <- cut(Current_Delinq_Debt$DebtToIncomeRatio, breaks=breaks3)
ggplot(Current_Delinq_Debt, aes(x=DebtToIncomeRatio, y=CurrentDelinquencies)) + geom_step(aes(color=df10))

#There seems to be a clear trend here as well. AS Debt to Income ratio increases the number of delinquencies 
#does decrease


#Here we look at the population size of debt to income ratios to see if can find any trends

num_low_ItoD <- count(subset(Debt_to_income_ratio, Debt_to_income_ratio < .25))
num_low_ItoD
num_mid_ItoD <- count(subset(Debt_to_income_ratio, Debt_to_income_ratio > .25 & Debt_to_income_ratio < .50))
num_mid_ItoD
num_midupper_ItoD <- count(subset(Debt_to_income_ratio, Debt_to_income_ratio > .50 & Debt_to_income_ratio < .75))
num_midupper_ItoD
num_high_ItoD <- count(subset(Debt_to_income_ratio, Debt_to_income_ratio > .75))
num_high_ItoD

#This is the graph for income to debt rations above 1. Surprisingly the graph is fairly uniform.
breaks4 <- c(1, 2, 3, 4, 5, 11)
df11 <- data.frame(Current_Delinquincies, Debt_to_income_ratio)
Current_Delinq_Debt <- subset(df11, Debt_to_income_ratio > 1)
df12 <- cut(Current_Delinq_Debt$DebtToIncomeRatio, breaks=breaks4)
ggplot(Current_Delinq_Debt, aes(x=DebtToIncomeRatio, y=CurrentDelinquencies)) + geom_step(aes(color=df12))



#Final Plots



breaks2 <- c(0, 1150, 4300, 10000, 30000, 50000)
df8 <- data.frame(Current_Delinquincies, MonthlyIncome)
Current_Delinq_Monthly_Income <- subset(df8, MonthlyIncome < 50000)
Income_Class <- cut(Current_Delinq_Monthly_Income$StatedMonthlyIncome, breaks=breaks2, labels=c("Below Minimum Wage", "Below Median Wage", "Upper Middle Class", "Educated Professional", "Upper Class"))
ggplot(Current_Delinq_Monthly_Income, aes(x=StatedMonthlyIncome, 
                                          y=CurrentDelinquencies))  + geom_jitter(aes(color=Income_Class)) + ggtitle("Delinquencies and Income") + labs(x="Monthly Income", y="Delinquencies")





breaks1 <- c(0, 650, 750, 1000)
Credit_Score_Groups <- cut(data$CreditScoreRangeLower, breaks=breaks1, labels=c('Low Score','Medium Score','High Score'))
ggplot(data, aes(x=Credit_Score_Lower, 
                 y=Current_Delinquincies)) + geom_jitter(aes(color=Credit_Score_Groups)) + ggtitle("Delinquencies and Credit Score") + labs(x="Credit Score", y="Delinquencies")




breaks3 <- c(0, 0.25, 0.50, 0.75, 1)
df9 <- data.frame(Current_Delinquincies, Debt_to_income_ratio)
Current_Delinq_Debt <- subset(df9, Debt_to_income_ratio < 1)
Ratio_Quartiles <- cut(Current_Delinq_Debt$DebtToIncomeRatio, breaks=breaks3, labels=c("Q1", "Q2", "Q3", "Q4"))
ggplot(Current_Delinq_Debt, aes(x=DebtToIncomeRatio,
                                y=CurrentDelinquencies)) + geom_jitter(aes(color=Ratio_Quartiles)) + ggtitle("Delinquencies and Debt to Income Ratios") + labs(x="Debt to Income Ratio", y="Delinquencies")





