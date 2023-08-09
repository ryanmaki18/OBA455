
# clear data from workspace

rm(list=ls())



# load tidyverse package in the current session

library(tidyverse)



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "p. cluster" folder and click "Open"



#### cluster analysis ####


# read utilities data
# assign it to an object "util"

util = read.csv ("utilities.csv")



 # scatter plot between Sales and Fuel_Cost
# Sales on the x-axis and Fuel_cost on the y-axis

p = ggplot(util, aes(x = sales, y = Fuel_Cost)) +  geom_point(size = 5) + 
  geom_text(aes(label = Company_Short), hjust = -0.1, vjust = -0.5, size = 5) +
  labs(x = "Sales (KWH/year)", y = "Fuel_cost (cents/KWH)") +
  scale_x_continuous(limits = c(3000, 20000))

print(p)



#### hierarchical clustering ####


### normalization of all the variables

# mean and standard deviation of all variables

a1 = mean(util$Fixed_Charge)

a2 = sd(util$Fixed_Charge)


b1 = mean(util$ROR)

b2 = sd(util$ROR)


c1 = mean(util$Cost)

c2 = sd(util$Cost)


d1 = mean(util$Load_Factor)

d2 = sd(util$Load_Factor)


e1 = mean(util$Demand_Growth)

e2 = sd(util$Demand_Growth)


f1 = mean(util$Sales)

f2 = sd(util$Sales)


g1 = mean(util$Nuclear)

g2 = sd(util$Nuclear)


h1 = mean(util$Fuel_Cost)

h2 = sd(util$Fuel_Cost)



# create normalized variables

util = util %>%
  mutate(Fixed_Charge_Norm = (Fixed_Charge-a1)/a2, 
         ROR_Norm = (ROR-b1)/b2,
         Cost_Norm = (Cost-c1)/c2,
         Load_Factor_Norm = (Load_Factor-d1)/d2, 
         Demand_Growth_Norm = (Demand_Growth-e1)/e2,
         Sales_norm = (Sales-f1)/f2, 
         Nuclear_norm = (Nuclear-g1)/g2, 
         Fuel_Cost_Norm = (Fuel_Cost-h1)/h2)
         



# pick only normalized variables 

util.norm = util %>%
  select(Company_Short, Fixed_Charge_Norm, ROR_Norm, Cost_Norm, Load_Factor_Norm, Demand_Growth_Norm, Sales_norm, Nuclear_norm, Nuclear_norm)



# convert a selected column into a row name

util.norm = util.norm %>%
  column_to_rownames(var = "Company_Short")


rm(a1, a2, b1, b2, c1, c2, d1, d2, e1, e2, f1, f2, g1, g2, h1, h2)



# euclidean distance matrix 

dist.norm = dist(util.norm, method = "euclidean")



# view dist.norm matrix

View(as.matrix(dist.norm))



# hierarchical clustering with single linkage

hc = hclust(dist.norm, method = "single")



# dendogram

plot(hc, hang = -1, ann = FALSE, main = "")



# cut the dendogram at required number of clusters

cutree()


cutree()


cutree()


cutree()



#### k-means clustering ####


# clusters with k = 3 

kmeans(util.norm, 3)



# populating within-cluster sum of squares for each cluster value

temp = as_tibble

for(k in 2:21)
{
  a = kmeans(util.norm, k)
  
  temp = temp %>%
    bind_rows(as_tibble(list(clusters = k, awss = mean(a$withinss))))
}



# plot of average within sum of squares vs cluster size

p = ggplot(temp, aes(x = clusters, y - awss)) + 
  geom_line(stat = "identity", size = 2, color = "red", alpha = 1)+
  scale_x_continuous("clusters", breaks = c(2:21))+ 
  scale_y_continuous("average within sum of squares")

print(p)



# best k

a = kmeans()
