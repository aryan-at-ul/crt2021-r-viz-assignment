# all libs loaded here
library(tidyverse)
library(ggplot2)
#library(conflicted)
library(hms)
library(chron)
#library(data.table)
library(dplyr)

# check for versions (handy when working on diff versions of R)
sessionInfo()
rm(list = ls())
graphics.off()

# getting dataframe 
df <- read_csv("marathon.csv")
#df2 <- m
#correct the col names to work with them properly
names(df)<-make.names(names(df),unique = TRUE)
head(df)


df$Category = as.factor(df$Category)
levels(df$Category)


filter <- m$Category %in% c("MS","FS","M35","F35","M40","M40","MU19","FU19")
m2 <- m[filter,]
#m2$Chip.Time = as.numeric(as.character(m2$Chip.Time))
#m2$Chip.Time = strptime(x = m2$Chip.Time, format = "%H:%M:%S")

colnames(m2)
qplot(data = m2,x = Category,y = Chip.Time)

qplot(data = m2,x = Chip.Time,y = Overall.Position , colour = Category)


str(m2)

convert_types <- function(x) {
  stopifnot(is.list(x))
  x[] <- rapply(x, utils::type.convert, classes = "character",
                how = "replace", as.is = TRUE)
  return(x)
}

colnames(m2)
m2 %>% group_by(Category) %>% summarise(count=n())
m2 %>% group_by(Category) %>% summarise(count = sum(!is.na(X10K)))
m2 %>% group_by(Category) %>% summarise(count = sum(!is.na(X30K)))
m2 %>% group_by(Category) %>% summarise(count = sum(!is.na(Chip.Time)))

d <- m %>% group_by(Category) %>% summarise(count=n())
d2 <- m %>% group_by(Category) %>% summarise(count = sum(!is.na(X10K) & !('DQ' %in% X10K) & !('DNF' %in% X10K)))
d3 <- m %>% group_by(Category) %>% summarise(count = sum(!is.na(Halfway) & !('DQ' %in% X10K) & !('DNF' %in% X10K)))
d4 <- m %>% group_by(Category) %>% summarise(count = sum(!is.na(X30K) & !('DQ' %in% X10K) & !('DNF' %in% X10K)))
#d5 <- m %>% group_by(Category) %>% summarise(count = sum(!is.na(Chip.Time) & !('DQ' %in% X10K) & !('DNF' %in% X10K)))


d$X10K = d2$count
d$Halfway = d3$count
d$X30K = d4$count
#d$Final = d5$count

d

dt <- transpose(d)

matplot(dt , type = "b" , pch = 15:18)
colnames(df)


df %>%  distinct(Category)
df$Category = as.factor(df$Category)
df$Gender = as.factor(df$Gender)
df %>% drop_na()
df$Chip.Time =  as_hms(df$Chip.Time)#chron(times=df$Chip.Time)#as.ITime(df$Chip.Time)

levels(df$Category)

filter <- df$Category %in% c("MS","FS","M35","F35","M40","M40","MU19","FU19")
df2 <- df[filter,]


ggplot(df , aes(x = Category, y = Chip.Time)) + 
  geom_point() 


  
df <- df %>%  
    mutate(Category = case_when(drv == "f" ~ "front-wheel",
                                drv == "r" ~ "rear-wheel",
                                drv == "4" ~ "4-wheel drive"))
  





cat <- levels(df$Category)

typeof(cat)

for(x in cat){
  print(x)
}
 
ggplot(df , aes(x = Category, y =  Chip.Time, color = Gender)) + 
  geom_point(size = 5, alpha = 1/5 , position = "jitter") +
  xlab("age group") +
  ylab("race completion time") + 
  ggtitle("age group completion time spread") + 
  theme_minimal()


ggplot(df, aes(x=Chip.Time ,color = Gender)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

ggsave(filename = './figure/01_chiptime_density_histogram.png',plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)

ggplot(df, aes(x=Chip.Time, fill=Gender)) + geom_density(alpha=.3) + ggtitle("Density distibution of finish time , gender wise")
ggsave(filename = './figure/01_chiptime_density_plot.png',plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)

#sperating men rows and female rows 
df_m <- filter(df,starts_with("M"))
df_f <-











