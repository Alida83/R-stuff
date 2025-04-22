# Coursework project
# Loading packages to start

library(readr)
library(dplyr)

# Loading data and creating sample 

X2005 <- read_csv("2005.csv")
view(X2005)

X2005=X2005
set.seed(100000) # first I wanted to use the whole annual data, hence this code
X2005_sample <- X2005 %>% sample_n(100000)
df1=X2005_sample
print(X2005_sample)

write_csv(df1, "C:\\Users\\alida\\Documents\\X2005_sample.csv")

X2006 <- read_csv("2006.csv")
View(X2006)

X2006=X2006
set.seed(100000)
X2006_sample <- X2006 %>% sample_n(100000)
df2=X2006_sample
print(X2006_sample)

write_csv(df2, "C:\\Users\\alida\\Documents\\X2006_sample.csv")


X2007 <- read_csv("2007.csv")
View(X2007)

X2007=X2007
set.seed(100000)
X2007_sample <- X2007 %>% sample_n(100000)
df3=X2007_sample
print(X2007_sample)

write_csv(df3, "C:\\Users\\alida\\Documents\\X2007_sample.csv")
print(X2007_sample.csv)


#======= Merge and saving ==============

X2005_sample <-read_csv("X2005_sample.csv")
X2006_sample <-read_csv("X2006_sample.csv")
X2007_sample <-read_csv("X2007_sample.csv")

project = bind_rows(X2005_sample, X2006_sample, X2007_sample)

project <- read_csv("project.csv")


summary(project)

write_csv(project, "C:\\Users\\alida\\Documents\\project.csv")


#========Creating Database, connecting to SQLite ===========

library(RSQLite)

if(file.exists("coursework.db"))
  file.remove("coursework.db")

conn <- dbConnect(RSQLite::SQLite(), "coursework.db")

airports <- read.csv("airports.csv", header = TRUE)
carriers <- read.csv("carriers.csv", header = TRUE)
planes <- read.csv("plane-data.csv", header = TRUE)
project <- read.csv("project.csv", header = TRUE)

dbWriteTable(conn, "airports", airports)
dbWriteTable(conn, "carriers", carriers)
dbWriteTable(conn, "planes", planes)
dbWriteTable(conn, "project", project)

dbListTables(conn)

#========= Q1 ============

library(ggplot2)

# ========================= Q1A =========================

Q1A <- dbGetQuery(conn, 
                  "SELECT DepTime AS DepTime, AVG(project.DepDelay) AS avg_delay
  FROM project
  WHERE project.Cancelled = 0 AND project.Diverted = 0 AND project.DepDelay > 0
  GROUP BY DepTime
  ORDER BY avg_delay")

Q1A <- Q1A %>% mutate(TimeSlot = 
                                case_when(DepTime <= 600 ~ "Night",
                                          DepTime <= 1200 ~ "Morning",
                                          DepTime <= 1800 ~ "Afternoon",
                                          DepTime <= 2400 ~ "Evening",
                                          DepTime <= 2800 ~ "Night"
                                )) # Part of day groups 

# ================= Boxplot of Q1A =======================

ggplot(Q1A, aes(x = TimeSlot, y = avg_delay)) +
  geom_boxplot() +
  labs(x = 'Time Of The Day', y = 'Delay In Minutes') +
  ylim(0, 100)


# ======================= Q1B ======================

Q1B <- dbGetQuery(conn, 
                  "SELECT DayOfWeek AS Day, AVG(project.DepDelay) AS avg_delay
  FROM project
  WHERE project.Cancelled = 0 AND project.Diverted = 0 AND project.DepDelay > 0
  GROUP BY DayOfWeek
  ORDER BY avg_delay")

Q1B$Day <- recode(Q1B$Day, 
                  "1"="Monday",
                  "2"="Tuesday",
                  "3"="Wednesday",
                  "4"="Thursday",
                  "5"="Friday",
                  "6"="Saturday",
                  "7"="Sunday")

print(paste(Q1B[1,"Day"],"has the lowest delay")) # actual answer

# ============= Lollipop chart of Q1B ================

ggplot(Q1B, aes(x = Day, y = avg_delay)) +
  geom_point(color = "red", size = 4) +
  geom_segment(aes(x = Day, xend = Day, y =27, yend = avg_delay)) +
  labs(x = "Day", y = "Average delay in minutes")

boxplot(Q1B$avg_delay ~ Q1B$Day, data=Q1B)

# ==================== Q1C ========================

Q1C <- dbGetQuery(conn, 
                  "SELECT Month, AVG(project.DepDelay) AS avg_delay
  FROM project
  WHERE project.Cancelled = 0 AND project.Diverted = 0 AND project.DepDelay > 0
  GROUP BY Month
  ORDER BY avg_delay")

Q1C$Month <- recode(Q1C$Month, 
                           "1"="Jan",
                           "2"="Feb",
                           "3"="Mar",
                           "4"="Apr",
                           "5"="May",
                           "6"="Jun",
                           "7"="Jul",
                           "8"="Aug",
                           "9"="Sep",
                           "10"="Oct",
                           "11"="Nov",
                           "12"="Dec")

print(paste(Q1C[1,"Month"],"has the lowest delay")) # actual answer

# ================= Lollipop chart of Q1C ===================

ggplot(Q1C, aes(x = Month, y = avg_delay)) +
  geom_point(color = "red", size = 4) +
  geom_segment(aes(x = Month, xend = Month, y =25, yend = avg_delay)) +
  labs(x = "Month", y = "Average delay in minutes")


# ========= Q2 ==============

Q2 <- dbGetQuery(conn,
                 "SELECT planes.year AS year, AVG(project.DepDelay) AS delay
 FROM planes JOIN project USING (tailnum)
 WHERE project.Cancelled = 0 AND project.Diverted = 0 
 AND project.DepDelay > 0 AND planes.year >= 1956
 GROUP BY planes.year
 ORDER BY planes.year
                 ")
# ========== Scatter plot of Q2 =================

ggplot(Q2, aes(x = delay, y = year)) +
  geom_point()


# ================= Q3 ====================

# 3 years in separate codes then merging them on 1 plot

Q3.1 <- dbGetQuery(conn,
                 "SELECT airports.city AS city, COUNT(*) AS total
 FROM airports JOIN project ON project.Origin = airports.iata
 WHERE project.Year = 2005 AND project.Cancelled = 0 AND project.Diverted = 0
 GROUP BY airports.city
 ORDER BY total DESC
 Limit 10")

Q3.2 <- dbGetQuery(conn,
                   "SELECT airports.city AS city, COUNT(*) AS total
 FROM airports JOIN project ON project.Origin = airports.iata
 WHERE project.Year = 2006 AND project.Cancelled = 0 AND project.Diverted = 0
 GROUP BY airports.city
 ORDER BY total DESC
 LIMIT 10")

Q3.3 <- dbGetQuery(conn,
                   "SELECT airports.city AS city, COUNT(*) AS total
 FROM airports JOIN project ON project.Origin = airports.iata
 WHERE project.Year = 2007 AND project.Cancelled = 0 AND project.Diverted = 0
 GROUP BY airports.city
 ORDER BY total DESC
 LIMIT 10")

# ================== Plot of Q3 ====================

ggplot() +
  geom_point(data = Q3.1, aes(x = total, y = city), color = "yellow", size = 6, alpha = 0.5) +
  geom_point(data = Q3.2, aes(x = total, y = city), color = "orange", size = 6, alpha = 0.5) +
  geom_point(data = Q3.3, aes(x = total, y = city), color = "red", size = 6, alpha = 0.5) 


# ================= Q4 ===================

# Comparing Origin and Destination locations

Q4A <- dbGetQuery(conn,
                   "SELECT project.Origin AS Origin, airports.airport AS Airport, COUNT(*) AS Total 
 FROM project JOIN airports ON project.Origin = airports.iata
 WHERE LateAircraftDelay > 0 AND Cancelled = 0 AND ArrDelay > 0
 AND DepDelay > 0 
 GROUP BY Origin
 ORDER BY Total DESC
 LIMIT 10 ")

Q4B <- dbGetQuery(conn,
                  "SELECT project.Dest AS Destination, airports.airport AS Airport, COUNT(*) AS Total 
 FROM project JOIN airports ON project.Dest = airports.iata
 WHERE LateAircraftDelay > 0 AND Cancelled = 0 AND ArrDelay > 0
 AND DepDelay > 0 
 GROUP BY Dest
 ORDER BY Total DESC
 LIMIT 10 ")

# ================= Plot of Q4 ======================
ggplot() +
  geom_point(data = Q4A, aes(x = Total, y = Airport), color = "green", size = 6, alpha = 0.5) +
  geom_point(data = Q4B, aes(x = Total, y = Airport), color = "blue", size = 6, alpha = 0.5) 


# =========== Digging deeper in Q4 ===========

X2005 <- read_csv("2005.csv")

print(X2005)
june <- filter(X2005, Month == "6")
print(june)
write_csv(june, "C:\\Users\\alida\\Documents\\june.csv")
# investigating a random month for actual number of flights 

june <- read.csv("june.csv", header = TRUE) # adding it to the above database
dbWriteTable(conn, "june", june)
dbListTables(conn)

Q4J <- dbGetQuery(conn,
                  "SELECT TailNUm, COUNT (*) AS T, Origin, Dest, DepTime, DayOfMonth
 FROM June
 WHERE LateAircraftDelay > 0 AND Cancelled = 0 AND ArrDelay > 0
 AND DepDelay > 0 AND Origin = 'ORD'
 GROUP BY TailNum ")

Q4J1<- dbGetQuery(conn,
                  "SELECT TailNUm, Origin, Dest, DepTime, DayOfMonth
 FROM June
 WHERE LateAircraftDelay > 0 AND Cancelled = 0 AND ArrDelay > 0
 AND DepDelay > 0 AND Tailnum = 'N518AE' AND DayOfMonth = 30
 ORDER BY DepTime ")
# Q4J1 was created by choosing a unique aircraft from Q4J and see it's route

dbDisconnect(conn)

# ==================== Q5 =======================

# =========== Logical regression ============

project <- read_csv("project.csv")
colnames(project)

df <- project
df <- df %>% mutate (TARGET = 
                       case_when(ArrDelay <= 0 ~ "0",
                                 ArrDelay > 0 ~ "1"))


df <- df %>% select(DepDelay, DepTime, TARGET)
print(df)
sapply(df, function(x) sum(is.na(x)))
df <- na.omit(df)
summary(df)


index <- sample(nrow(df), nrow(df)*0.7)
df_train = df[index,]
df_test = df[-index,]

df_train$TARGET <- as.numeric(df_train$TARGET)
str(df_train)

df_test$TARGET <- as.numeric(df_test$TARGET)
str(df_test)

model <- glm(TARGET~., family = binomial, data = df_train)
glm(formula = TARGET~., family = binomial, data = df_train)

predict(model, df_test, type ="response")

library(pROC)

auc(df_test$TARGET, predict)
plot <- roc(response = df_test$TARGET, predictor = df_test$DepDelay)

ggroc(plot)

