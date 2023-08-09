library(tidyverse)
#1
#a
c(1:30, 50:80)
#b
seq(from=0, to=1000, by= 50)
#c
rep(c(1:3), times = 10)
#d
rep(c(3:30), each = 10)
#e
e= c(1:10)
sort(e,decreasing = TRUE)
#f
f= c(1:10)
sort(f,decreasing = FALSE)
#g
unique(c("A", "B", "B", "C"))
#h
a= c(12,4,6,73,67,84,45,74,5,52,35)
a[3]
a[6]
#i
a= c(12,4,6,73,67,84,45,74,5,52,35)
a[-3]
a[-6]
#j
a= c(12,4,6,73,67,84,45,74,5,52,35)
a[a<10]
#k
k <- 1
while (k<11){
  print(k)
  k=k+1
}
#l
i = 3 
ifelse(i==3, "TRUE", "FALSE") 
#m
a=letters
class(a)
#n
as.numeric(c("1","3","6"))
#o
as.character(c(6,3,1))
#p
rank(c(12,4,6,73,67,84,45,74,5,52,35))
#q
var(c(12,4,6,73,67,84,45,74,5,52,35))
#r
list=ls()
#s
rm(list= ls())
#t
t= matrix(c(1:9),nrow=3, ncol=3)
#u

#v

#w
w="today is saturday"
nchar(w)
#x
x= "today is saturday"
toupper(x)


#2
#a
ridmov = read_csv("RidingMowers.csv")
#b
head(ridmov, n=10)
#c
summary(ridmov$Income)
var(ridmov$Income)
sd(ridmov$Income)
summary(ridmov$Lot_Size)
var(ridmov$Lot_Size)
sd(ridmov$Lot_Size)
table(ridmov$Ownership)
#d
d=ridmov%>%
  group_by(ridmov$Ownership)%>%
  summarise(Avg_Income= mean(Income),SD_Income=sd(Income),Avg_Lot_Size=mean(Lot_Size), SD_Lot_Size=sd(Lot_Size))%>%
              ungroup()
#e
Temp1=ridmov%>%
  filter(Ownership== "Owner")%>%
  filter(Income < 50)
#f
Temp1=ridmov%>%
  filter(Income >= 50|Lot_Size >= 10)
#g
distinct(ridmov)
#h
mutate
library(tidyverse)
temp2 = ridmov %>% 
  filter(row_number()%% 3 ==0)
#i
sort(ridmov$Income,decreasing = FALSE)
#j
ridmov%>%
  arrange(desc(Lot_Size))
ridmov%>%
  arrange(Income)
#k
temp1=ridmov%>%
  mutate(Income_Cy=Income*6.37, LotSize_Sy=Lot_Size/3)%>%
  select(Income,Income_Cy,Lot_Size,LotSize_Sy)
rm(temp1)
#l
temp1=ridmov%>%
  data.frame(cs=cumsum(ridmov$Income),cm=cummean(ridmov$Income))
rm(temp1)
#m
temp1=ridmov%>%
  mutate(id=1:nrow(ridmov))%>%
  select(id,Income)
temp2=ridmov%>%
  mutate(id=1:nrow(ridmov))%>%
  select(id, Lot_Size)
fj=temp1%>%
  full_join(temp2, by = "id")
rm(temp1,temp2,fj)
#n
temp1=ridmov%>%
  slice(1:12)
temp2=ridmov%>%
  slice(13:24)
temp3=rbind(temp1,temp2)
rm(temp1,temp2,temp3)
#o
ridmov=ridmov%>%
  mutate(Household_Number=1:nrow(ridmov))%>%
  select(Household_Number,Income,Lot_Size, Ownership)
#p
temp1=ridmov%>%
  head( n=16)%>%
  select(Household_Number,Income)
#q
temp2=ridmov%>%
  tail(n=16)%>%
  select(Household_Number,Lot_Size)
#r
rj=temp1%>%
  right_join(temp2, by = "Household_Number")
#s
lj=temp1%>%
  left_join(temp2, by = "Household_Number")
#t
ij=temp1%>%
  inner_join(temp2, by = "Household_Number")
#u
fj=temp1%>%
  full_join(temp2, by = "Household_Number")
#v
a=mean(ridmov$Income, na.rm = TRUE)

b=mean(ridmov$Lot_Size, na.rm = TRUE)

temp5=fj%>%
  replace_na(list(Lot_Size=b, Income=a))
#w
x = ridmov$Income
y = ridmov$Lot_Size
plot(x,y,main = "Income & Lot Size",
     xlab = "Income ($000)", ylab = "Lot_Size(000 ft^2)",
     pch = 19, frame = "FALSE")

#x
boxplot(Income~Ownership, data = ridmov, main = "Income by Ownership", xlab = "Ownership of Riding Mower", ylab = "Income ($000)")











