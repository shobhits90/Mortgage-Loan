library(readr)
Mortgage <- read_csv("C:/Users/shobh/Data Science/Mortgage.csv")
View(Mortgage)
Mortgage$Debt_income <- as.numeric(Mortgage$Loan_Amount_000/Mortgage$Applicant_Income_000)
Mortgage$Binary<- 1
str(Mortgage)
library(ggplot2)
library(dplyr)
ggplot(Mortgage) +  geom_bar( aes(As_of_Year) )
ggplot(data=Mortgage, aes(As_of_Year, Loan_Amount_000) ) +geom_bar(stat = "identity")
totaldi <- Mortgage %>%
group_by_(.dots=c("State","As_of_Year")) %>%
summarize(total = mean(Debt_income, na.rm = TRUE))
ggplot(data=totaldi, aes(x=As_of_Year, y=total, colour=As_of_Year)) +geom_bar(stat = "identity")+ylab("Average Debt to Income")
totalsav <- Mortgage %>%
      group_by_(.dots=c("State","As_of_Year")) %>%
      summarize(total = mean(Debt_income,na.rm= TRUE))
ggplot(totalsav, aes(x=As_of_Year, y=total)) +ylab("Average Loan Amount")+geom_bar(stat = "identity")
ggplot(data=Mortgage, aes(Loan_Type_Description, Loan_Amount_000,fill = State ) ) +geom_bar(stat = "identity")
ggplot(Mortgage, aes(As_of_Year, fill = State ) ) + geom_bar()
ggplot(Mortgage, aes(As_of_Year,Loan_Amount_000, fill = State ) ) + geom_point()+ylab("Loan Amount")
ggplot(Mortgage,
aes(x = factor(""), fill = State) ) +
geom_bar() +
coord_polar(theta = "y") +
scale_x_discrete("")
totals <- Mortgage %>%
group_by_(.dots=c("State","As_of_Year")) %>%
summarize(total = sum(Loan_Amount_000))
ggplot(data=totals,
aes(x=As_of_Year, y=total, colour=State)) +
geom_line()
ggplot(data=totalsav,
aes(x=As_of_Year, y=total, colour=State)) +
geom_line()+ylab("Average Amount")
ggplot(data=totaldi, aes(x=As_of_Year, y=total, colour=State)) +
geom_line()+ylab("Debt to Income")
totalbin <- Mortgage %>%
group_by_(.dots=c("State","As_of_Year")) %>%
summarize(total = sum(Binary))
ggplot(data=totalbin, aes(x=As_of_Year, y=total, colour=State)) +
geom_line()+ylab("Total Number of Applicant")
totalstype <- Mortgage %>%
group_by_(.dots=c("State","Loan_Type_Description")) %>%
summarize(total = sum(Loan_Amount_000))
ggplot(data=totalstype, aes(Loan_Type_Description, total, colour =State) ) +geom_point(size=10)
total12 <- Mortgage %>%
group_by_(.dots=c("Loan_Type_Description","As_of_Year")) %>%
summarize(total = sum(Loan_Amount_000))
ggplot(data=total12,aes(x=As_of_Year, y=total, colour=Loan_Type_Description)) +geom_line()
total13 <- Mortgage %>%
group_by_(.dots=c("Loan_Type_Description","As_of_Year")) %>%
summarize(total = sum(Binary))
ggplot(data=total13,aes(x=As_of_Year, y=total, colour=Loan_Type_Description)) +geom_line()+ylab("number of Application")
