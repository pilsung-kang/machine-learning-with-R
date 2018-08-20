# Part 1-1: Data Handling (Vector) ----------------------------------------

# Assign values to the vector A & B
A <- c(1,2,3)
B <- c(1, "A", 0.5)

# Check the mode
mode(A)
mode(B)

# Select a subset of vector
A[1]
A[2:3]
A[c(2,3)]

# Assign names
names(A)
names(A) <- c("First", "Second", "Third")

# call by index or name
A[1]
A["First"]

# Data Handling: Vector
x <- c(1,2,3,4)
x <- c(x[1:3], 10, x[4])

length(x)

c(1,2,4) + c(10,11,12,13,14)

x <- matrix(1:6, nrow=3, ncol=2)
x + c(1:2)

x <- c(1,2,3)
y <- c(10,20,30)

x+y
x*y
x%%y

y <- c(10,20,30,40,50)
y[c(1,3)]
y[2:3]
v <- 2:3
y[v]

y[c(1,2,1,3)]

y[-5]
y[-length(y)]

x <- 1:5
y <- 5:1
z <- 2
1:z-1
1:(z-1)

seq(from=12,to=30,by=3)
seq(from=12,to=30,by=4)
seq(from=1.1,to=2,length=10)

rep(10,5)
rep(c(10,20,30),3)
rep(1:3,3)
rep(c(10,20,30),each=3)

x <- 1:10
x > 8
any(x > 8)
any(x > 20)
all(x > 8)
all(x > 0)

x <- c(1,2,NA,4,5)
y <- c(1,2,NULL,4,5)

mean(x)
mean(x, na.rm = TRUE)

mean(y)

x <- c(10,20,NA,40,50)
x[x>20]
subset(x, x>20)
which(x>20)


# Part 1-1: Data Handling (List) ------------------------------------------

# Example of a list
listA <- list(1, 2, "a")
print(listA)
listA[[1]]
listA[c(1,2)]
names(listA)
names(listA) <- c("First", "Second", "Third")

listA[["Third"]]
listA$Third

# Data Handling: List
A <- list(name="Kang", salary = 10000, union = TRUE)
A
A$name

B <- list("Kang", 10000, TRUE)
B
B[[1]]

C <- vector(mode="list")
C[["name"]] <- "Kang"
C[["salary"]] <- 10000
C[["union"]] <- TRUE
C

C$name
C[["name"]]
C[[1]]

C1 <- C[[1]]
class(C1)
C1

C2 <- C[1]
class(C2)
C2

C$office <- "frontier"
C

C$salary <- NULL
C

tmplist <- list(a = list(1:5, c("a","b","c")), b = "Z", c = NA)
tmplist
unlist(tmplist)
unlist(tmplist, use.names = FALSE)

A <- list(1:3,25:29)
A
lapply(A,median)
sapply(A,median)


# Part 1-3: Data Handling (Matrix) ----------------------------------------

# Example of a matrix
A <- 1:6
dim(A)
print(A)

dim(A) <- c(2,3)
print(A)

B <- list(1,2,3,4,5,6)
print(B)
dim(B)
dim(B) <- c(2,3)
print(B)

D <- 1:12
dim(D) <- c(2,3,2)
print(D)

# Data Handling: Matrix & Array
A = matrix(1:15, nrow=5, ncol=3)
A

B = matrix(1:15, nrow=5, byrow = T)
B

C = matrix(nrow=2,ncol=2)
C[1,1] = 1
C[1,2] = 2
C[2,1] = 3
C[2,2] = 4
C

A = matrix(1:4, nrow=2, ncol=2)
B = matrix(seq(from=2,to=8,by=2), nrow=2, ncol=2)
A
B

A*B # 행렬 원소간 곱셈
A %*% B # 행렬간 곱셈
A*3 # 행렬*상수
A+B # 행렬간 합

C = matrix(1:15, nrow=5, ncol=3)
C
C[3,2]
C[2,]
C[,3]
C[2:4,2:3]
C[-1,]

C[1,] <- c(10, 11, 12)
C

A <- matrix(c(1:6), nrow=3, ncol=2)
A
A[A[,2]>=5,]

which(A>3)

A <- matrix(c(1:6), nrow=3, ncol=2)
apply(A,1,mean)
apply(A,2,mean)

A <- matrix(c(1:6), nrow=3, ncol=2)
B <- matrix(c(11:16), nrow=3, ncol=2)

A
B

rbind(A,B)
cbind(A,B)

cbind(A[,1],B[,2])

A <- matrix(c(1:6), nrow=3, ncol=2)
colnames(A)
rownames(A)

colnames(A) <- c("1st","2nd")
colnames(A)

rownames(A) <- c("First","Second","Third")
rownames(A)

A[,"1st",drop=FALSE]

A <- matrix(c(1:15), nrow=5, ncol=3)
B <- matrix(c(11:25), nrow=5, ncol=3)
A
B

C <- array(data=c(A,B),dim=c(3,2,2))
C


# Part 1-4: Data Handling (Factor) ----------------------------------------

# Example of a factor
A <- c("Cho","Kim","Kang")
B <- as.factor(A)

print(A)
print(B)

mode(A)
mode(B)

A[1]+A[2]
B[1]+B[2]

# Data Handling: Factor
x <- c(5,12,13,12)
xf <- factor(x)
xf

str(xf)
unclass(xf)

length(xf)

xff <- factor(x, levels=c(5,12,13,88))
xff
xff[2] <- 88
xff

xff[2] <- 20
xff

ages <- c(25,26,55,37,21,42)
affils <- c("R","D","D","R","U","D")
tapply(ages, affils, mean)

gender <- c("M", "M", "F", "M", "F", "F")
age <- c(47,59,21,32,33,24)
income <- c(55000,88000,32450,76500,123000,45650)
tmp <- data.frame(gender, age, income)

tmp$over25 <- ifelse(tmp$age>25,1,0)
tmp
tapply(tmp$income, list(tmp$gender, tmp$over25), mean)

split(tmp$income, list(tmp$gender, tmp$over25))

table(tmp$gender, tmp$over25)


# Part 1-5: Data Handling (DataFrame) -------------------------------------

# Example of data frame
A <- c(1,2,3)
B <- c("a","b","c")
C <- data.frame(A,B)
C
C[[1]]
C[[2]]
C[1,2]
C$B[2]

C <- data.frame(A,B, stringsAsFactors=FALSE)
C
C[[1]]
C[[2]]
C[1,2]
C$B[2]

kids <- c("Jack", "Jill")
ages <- c(12,10)
d <- data.frame(kids, ages, stringsAsFactors=FALSE)
d

d[[1]]
class(d[[1]])

d$kids
class(d$kids)

d[,1]
class(d[,1])

d[1]
class(d[1])

Exam <-read.csv("Exam.csv", header = TRUE)
Exam

Exam[2:5,]
Exam[2:5,2]
Exam[2:5,2, drop=FALSE]

Exam[Exam$Exam1 > 3,]

dfA <- rbind(d,list("Laura",19))
kids <- c("Alice","Jill", "Laura")
state <- c("MA", "NY", "CA")
dfB <- data.frame(kids, state, stringsAsFactors=FALSE)

merge(dfA, dfB) # default: inner join
merge(dfA, dfB, all = TRUE) # outer join
merge(dfA, dfB, all.x = TRUE) # left join
merge(dfA, dfB, all.y = TRUE) # right join

firstname <- c("Alice","Jill", "Laura")
state <- c("MA", "NY", "CA")
dfC <- data.frame(firstname, state, stringsAsFactors=FALSE)
dfC

merge(dfA, dfC, by.x="kids", by.y="firstname")


# Part 2: Text Data Handling ----------------------------------------------

S <- "Welcome to Data Science!"
length(S)
nchar(S)

S1 <- "My name is"
S2 <- "Pilsung Kang"
paste(S1, S2)
paste(S1, S2, sep="-")
paste(S1, S2, sep="")

paste("The value of log10 is", log(10))

S1 <- c("My name is", "Your name is")
S2 <- c("Pilsung")
S3 <- c("Pilsung", "Younho", "Hakyeon")
paste(S1,S2)
paste(S1,S3)

stooges <- c("Dongmin", "Sangkyum", "Junhong")
paste(stooges, "loves", "R.")
paste(stooges, "loves", "R", collapse = ", and ")

substr("Data Science", 1, 4)
substr("Data Science", 6, 10)

stooges <- c("Dongmin", "Sangkyum", "Junhong")
substr(stooges, 1,3)

cities <- c("New York, NY", "Los Angeles, CA", "Peoria, IL")
substr(cities, nchar(cities)-1, nchar(cities))

path <- "C:/home/mike/data/trials.csv"
strsplit(path,"/")

path <- c("C:/home/mike/data/trials1.csv",
          "C:/home/mike/data/errors2.txt",
          "C:/home/mike/data/report3.doc")
strsplit(path,"/")

strsplit(path, "om")
strsplit(path, "[hm]")
strsplit(path, "i.e")
strsplit(path, "\\.")
strsplit(path, "r{2}")
strsplit(path, "[[:digit:]]")

tmpstring <- "Kim is stupid and Kang is stupid too"
sub("stupid", "smart", tmpstring)
gsub("stupid", "smart", tmpstring)

grep("mike",path)
grep("errors",path)


# Part 3: Conditions and Repititions --------------------------------------

# 조건문
r <- 1
if (r==4) {
  printf("The valus of r is 4")
} else {
  print("The valus of r is not 4")
}

carbon <- c(10, 12, 15, 19, 20)
if (mean(carbon) > median(carbon)) {
  print ("Mean > Median")
} else {
  print ("Median <= Mean")
}

# Caution!
if (mean(carbon) > median(carbon)) {
  print ("Mean > Median")
} 
else {
  print ("Median <= Mean")
}

# ifelse example
x <- 1:10
y <- ifelse(x%%2 == 0, "even", "odd")

# 반복문
n <- c(5,10,15)

for (i in n) {
  print(i^2)
}

i <- 1
while (i <= 10) {
  i <- i+4
  print(i)
}

i <- 1
repeat {
  i <- i+4
  print(i)
  if (i > 10) break
}


# Part 4: Functions -------------------------------------------------------

# User written functions
mean.and.sd1 <- function(x) {
  av <- mean(x)
  sdev <- sd(x)
  return(c(mean=av, SD=sdev))
}

distance <- c(148, 182, 173, 166, 109, 141, 166)
mean.and.sd1(distance)

mean.and.sd2 <- function(x) {
  av <- mean(x)
  sdev <- sd(x)
  c(mean=av, SD=sdev)
  return(av)
}

distance <- c(148, 182, 173, 166, 109, 141, 166)
mean.and.sd2(distance)

mean.and.sd3 <- function(x = rnorm(10)) {
  av <- mean(x)
  sdev <- sd(x)
  c(mean=av, SD=sdev)
}

mean.and.sd3()
mean.and.sd3(distance)

# Function arguments
addTheLog <- function(first, second) {first + log(second)}
# Exact names
addTheLog(second=exp(4),first=1)
# Partially matching names
addTheLog(s=exp(4),first=1)
# Argument order
addTheLog(1,exp(4))

# Return the result with return()
oddcount <- function(x) {
  k <- 0
  print(sprintf("odd number calculator"))
  for (n in 1:x) {
    if (n %% 2 == 1) {
      cat(sprintf("%d is an odd number. \n", n))
      k <- k+1
    }
  }
  return(k)
}

oddcount(10)

# Return the result without return() but explicitly designate the object
oddcount <- function(x) {
  k <- 0
  print(sprintf("odd number calculator"))
  for (n in 1:x) {
    if (n %% 2 == 1) {
      cat(sprintf("%d is an odd number. \n", n))
      k <- k+1
    }
  }
  k
}

oddcount(10)

# Return the result without either return() or explicit designation
oddcount <- function(x) {
  k <- 0
  print(sprintf("odd number calculator"))
  for (n in 1:x) {
    if (n %% 2 == 1) {
      cat(sprintf("%d is an odd number. \n", n))
      k <- k+1
    }
  }
}

oddcount(10)

# Function as an object
abline
args(abline)

sort

f1 <- function(a,b) return(a+b)
f2 <- function(a,b) return(a-b)

g <- function(h, x, y) h(x,y)
g(f1,5,2)
g(f2,5,2)

# Anonymous functions
apply.to.three <- function(f) {f(3)}
apply.to.three(function(x) {x*7})

a <- c(1,2,3,4,5)
sapply(a,function(x) {x+1})

# Local variable in a function
w <- 12
f <- function (y) {
  d <- 8
  w <- w+1
  y <- y-2
  print(w)
  h <- function() return(d*(w+y))
  return(h())
}

t <- 4
f(t)

w
t

# Function example 1
findrepeats <- function(x, k) {
  n <- length(x)
  repeats <- NULL
  for (i in 1:(n-k+1)) {
    if(all(x[i:(i+k-1)] == 1)) repeats <- c(repeats, i)
  }
  return(repeats)
}

vec <- c(0,1,1,0,0,1,1,1,0,1,1)
findrepeats(vec,2)
findrepeats(vec,3)
findrepeats(vec,4)

# Example 2: Kendall's tau
findud <- function(v) {
  vud <- v[-1] - v[-length(v)]
  return(ifelse(vud >0, 1, -1))
}

udcorr <- function(x,y) {
  ud <- lapply(list(x,y), findud)
  return(mean(ud[[1]] == ud[[2]]))
}

temp <- c(10, 15, 13, 17, 20)
pressure <- c(900, 920, 890, 940, 920)

udcorr(temp,pressure)


# Part 5: R Graphs --------------------------------------------------------
# R에서 사용하는 그래프 
data(iris)
x <- iris[,1]
y <- iris[,2]
subiris <- iris[,1:2]

# 그래프의 다형성: 입력 인자에 따라 다른 형태의 그래프가 생성 
plot(x,y)
plot(subiris)
plot(iris)

# 제목, x-y 축 이름 달기 
plot(subiris, main="The comparison between length and width",
     xlab = "The length of sepal",
     ylab = "The width of sepal")

# 그래프 객체의 색 및 모양 변경하기 
plot(iris[,1],iris[,2],pch=as.integer(iris[,5]))

# 사용 가능한 색 및 모양 예시 
plot(iris$Sepal.Length,iris$Sepal.Width,
     pch=as.integer(iris$Species),col=as.integer(iris$Species)+10)

# 사용 가능한 색 및 모양 예시 
plot(0,0, xlim=c(0,13), ylim=c(0,4), type="n")
xpos <- rep((0:12)+0.5,2)
ypos <- rep(c(3,1), c(13,13))
points(xpos, ypos, cex=seq(from=1,to=3,length=26), col=1:26, pch=0:25)
text(xpos, ypos, labels = paste(0:25), cex=seq(from=0.1,to=1,length=26))

# 조건화 그래프 
coplot(iris[,1]~iris[,2] | iris[,5])

# 막대그래프 
data(airquality)
heights <- tapply(airquality$Temp, airquality$Month, mean)
barplot(heights)
barplot(heights, main="Mean Temp. by Month",
        names.arg = c("May", "Jun", "Jul", "Aug", "Sep"),
        ylab = "Temp (deg.F)")

# 막대그래프 꾸미기 
rel.hts <- (heights-min(heights))/(max(heights)-min(heights))
grays <- gray(1-rel.hts)
barplot(heights, col=grays, ylim=c(50,90), xpd=FALSE,
        main="Mean Temp. by Month",
        names.arg = c("May", "Jun", "Jul", "Aug", "Sep"),
        ylab = "Temp (deg.F)")

# 히스토그램 그리기 
samp <- rgamma(500,2,2)
hist(samp, 20, prob=T)
lines(density(samp))

# 그림을 파일로 저장하기: png 형식 
png("Hist_dist.png")
hist(samp, 20, prob=T)
lines(density(samp))
dev.off()

# 그림을 파일로 저장하기: pdf 형식 
pdf("Hist_dist.pdf")
hist(samp, 20, prob=T)
lines(density(samp))
dev.off()

# 보다 다양한 그래프 생성을 위해 ggplot2 패키기 이용 
install.packages("ggplot2")
library(ggplot2) 
data(mtcars)

# 그래프 도시를 위한 팩터 생성  
mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5), labels=c("3gears","4gears","5gears")) 
mtcars$am <- factor(mtcars$am,levels=c(0,1), labels=c("Automatic","Manual")) 
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8), labels=c("4cyl","6cyl","8cyl")) 

# 연비(mpg)에 대한 커널 밀도 함수 추정
# 기어의 숫자에 따른 그룹(색상)별로 도시
qplot(mpg, data=mtcars, geom="density", fill=gear, 
      alpha=I(.5), main="Distribution of Gas Milage", 
      xlab="Miles Per Gallon", ylab="Density")

# 각 기어(gear)-실린더 조합에 따른 연비(mpg)와 마력(hp)의 산점도
# 각 산점도에서 변속기(am)은 색상과 모양으로 구분됨
qplot(hp, mpg, data=mtcars, shape=am, color=am, 
      facets=gear~cyl, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon")

# 실린더 갯수에 따라 공차중량(wt)과 연비(mpg)를 회귀선으로 표현 
p <- ggplot(mtcars, aes(y=mpg, x=wt, colour=factor(cyl))) 
p <- p + ggtitle("Regression of MPG on Weight") 
p <- p + stat_smooth(method=lm, aes(fill = factor(cyl))) + geom_point()
p

# 기어의 숫자에 따른 연비의 상자그림 
# 실제 관측치들을 점으로 표현
qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"), 
      fill=gear, main="Mileage by Gear Number",
      xlab="", ylab="Miles per Gallon")


