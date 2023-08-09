
# basic operations - addition, subtraction, division, multiplication etc

8 + 9

10 - 5

1 / 7

5 * 7

5^2

pi


# rounding a real number to specified decimal places

round(3.324,2)


# absolute value returns the magnitude of the number

abs(-25)


# exponential transformation

exp(1)


# Infinite (ideally it is undefined, but in R its represented as Inf)

1/0


# Negative Infinite

-1/0


# Not a number (NAN)

0/0


# datatypes - numbers, characters, logical values

# uni dimensional representation

# numbers

a = 3


# characters (or strings)

b = "I am currently talking"

c = "Predictive modeling course" 


# concatenate the above two characters a and b separated by "_" (underscore symbol)

paste(b, c, sep = "_")

paste(b, c)


# lower case to upper case

tolower("DEF")


# upper case to lower case

toupper("def")


# logical - TRUE, FALSE

5 > 3

d = 5 > 3

4 == 4

10 < 5


# multidimensional representation - vectors

# numeric

c(2, 4, 6) 

e = c(2, 4, 6) 


# creating integer sequence from 1:10 by increment 1

c(1:10)

1:10


# generating sequence of numbers by a specified increment

seq(from = 1, to = 21, by = 2)


# repeat a vector multiple times

rep(c(1:4), times = 4 )


# repeat elements of a vector multiple times

rep(1:4, each = 2)  


# return the vector in ascending (increasing) order

f = c(12, 4, 6, 73, 67, 84, 45, 74, 5, 52, 35)

sort(f) 


# reverse the above vector a

rev(f) 


# accessing individual elements in a vector

# fourth element in the vector

f[4] 


# all but fourth

f[c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11)] 
f[-4]

# elements two, three and four

f[] 


# all except two to fourth

f[] 


# elements less than 20

f[] 


# vectors operations

g = c(1,2,5)

h = c(4,10,25)


# add each element in x and y

g+h


# multiply each element in x and y

g*h


# math functions

# natural logarithm

log(10) 


# logarithm to base 10

log10(100) 


i = c(12, 4, 5, 6, 73, 67, 84, 45, 74, 5, 52, 35)


# largest element

max(i)


# smallest element

min(i)



# sum

sum(i)


# mean

mean(i)


# median

median (i)


# quantile

quantile(i)


# rank

rank(i)


# variance

var(i)


# standard deviation

sd(i) 


# iterative loops

# for loop to generate numbers from 11 to 14 by increment 1

for () 
{
  
  
  print()
}


# if conditional statement

l = 3

if (l == 1)
{
  print("the value of l is 1")
} else {
  print("the value of l is not 1")
}


# matrices

m = 


# second row

m[, ] 


# second column

m[ , ] 


# element in second row and third column

m[, ] 


# transpose of matrix

t(m) 


# multiplication

m %*% t(m) 


# list all objects in your session

ls()              


# emptying the workspace

# remove defined object from the environment   

rm()


# removing multiple objects 

rm() 


# removes all objects from the environment 

rm() 


# help

# help of a particular function

?mean  


help("mean")