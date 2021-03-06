# all libs loaded here
library(tidyverse)
library(ggplot2)
library(lubridate)
library(hms)
#library(chron)
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

df <- df %>%  
  mutate(Category = case_when(Category == "FU19" ~ "F19",
                              Category == "FS" ~ "F25",
                              Category == "MS" ~ "M25",
                              Category == "MU19" ~ "M19",
                              TRUE ~ Category))#


df_temp <- df


convert_types <- function(x) {
  stopifnot(is.list(x))
  x[] <- rapply(x, utils::type.convert, classes = "character",
                how = "replace", as.is = TRUE)
  return(x)
}


df %>%  distinct(Category)
df$Category = as.factor(df$Category)
df$Gender = as.factor(df$Gender)
df$Chip.Time <- as.character(strptime(df$Chip.Time, "%H:%M:%S"), "%H:%M:%S")
df %>% drop_na()
df$Chip.Time =  as_hms(df$Chip.Time)

df$Gun.Time <- as.character(strptime(df$Gun.Time, "%H:%M:%S"), "%H:%M:%S")
df %>% drop_na()
df$Gun.Time =  as_hms(df$Gun.Time)

levels(df$Category)

filter <- df$Category %in% c("MS","FS","M35","F35","M40","M40","MU19","FU19")
df2 <- df[filter,]


ggplot(df , aes(x = Category, y = Chip.Time)) + 
  geom_point() 


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




ggplot(df_f , aes(x = Category, y =  Chip.Time)) + 
  geom_point(size = 5, alpha = 1/5 ,color = "blue", position = "nudge") +
  xlab("female age groups") +
  ylab("race completion time") + 
  ggtitle("female age group completion time spread") + 
  theme_minimal()


ggplot(df_m , aes(x = Category, y =  Chip.Time)) + 
  geom_point(size = 5, alpha = 1/5 ,color = "red", position = "nudge") +
  xlab("male age groups") +
  ylab("race completion time") + 
  ggtitle("male age group completion time spread") + 
  theme_minimal()


# disqalification / Na based on gender 
#df_dm <- df[grep("M", df$Category),]
#df_df <- df[grep("F", df$Category),]

na_x<- df_temp %>% group_by(Category) %>% summarise(count = sum(is.na(Chip.Time)))
dq_x <- df_temp %>% group_by(Category) %>% summarise(count = sum(Chip.Time == "DQ"))
dnf_x <- df_temp %>% group_by(Category) %>% summarise(count = sum(Chip.Time == "DNF"))

na_g<- df_temp %>% group_by(Gender) %>% summarise(count = sum(is.na(Overall.Position)))
dq_g <- df_temp %>% group_by(Gender) %>% summarise(count = sum(Overall.Position == "DQ"))
dnf_g <- df_temp %>% group_by(Gender) %>% summarise(count = sum(Overall.Position == "DNF"))


gender_wise_drop_out_before_10k = df_temp %>% group_by(Gender) %>% summarise(count = sum(is.na(X10K)))
gender_wise_drop_out_before_halfway = df_temp %>% group_by(Gender) %>% summarise(count = sum(is.na(Halfway)))
gender_wise_drop_out_before_30k = df_temp %>% group_by(Gender) %>% summarise(count = sum(is.na(X30K)))

gender_wise_drop_out_before_10k
gender_wise_drop_out_before_halfway
gender_wise_drop_out_before_30k

age_wise_drop_out_before_10k = df_temp %>% group_by(Category) %>% summarise(count = sum(is.na(X10K)))
age_wise_drop_out_before_halfway = df_temp %>% group_by(Category) %>% summarise(count = sum(is.na(Halfway)))
age_wise_drop_out_before_30k = df_temp %>% group_by(Category) %>% summarise(count = sum(is.na(X30K)))

# trying to plot gender wise drop and age wise drop
#mat = matrix(ncol = 0, nrow = 0)
#d=data.frame(mat)
d <- df_temp %>% group_by(Category) %>% summarise(count=n())



age_wise_drop_out_before_10k
d$x10k = age_wise_drop_out_before_10k$count
d$halfway = age_wise_drop_out_before_10k$count
d$X30K = age_wise_drop_out_before_10k$count
d$pecent =  (d$x10k / d$count ) * 100


ggplot(d, aes(x = Category, y =  pecent)) + 
  geom_point(size = 5, alpha = 1/5 ,color = "red", position = "nudge") +
  xlab("age groups") +
  ylab("percentage  disqualification or could not complete") + 
  ggtitle("disqulification / give up count age-gender group wise") 

ggsave(filename = './figure/02_age_group_wise_exit_from_race.png',plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)


#count of walkers , joggers and runners 
df$type = ""
df <- df %>%  mutate(type = case_when(type < "03:00:00" ~ "runner",
                                      type > "03:00:00" & type < "05:00:00" ~ "jogger",
                                      type > "05:00:00" ~ "walker",
                                      TRUE ~ type))

df$type[df$Chip.Time <  period(3, 'hour')] <- "runner"

df$type[df$Chip.Time >   period(3, 'hour') & df$Chip.Time <   period(5, 'hour')] <- "jogger"

df$type[df$Chip.Time >  period(5, 'hour')] <- "walker"

df$type[df$Chip.Time == "" | is.na(df$Chip.Time)] <- "walker"

df$type = as.factor(df$type)
#sperating men rows and female rows 


df$type <- df$type %>% replace(.=="", "walker")

ggplot(df_m, aes(x = Category, y =  Chip.Time)) + 
  geom_point(size = 5, alpha = 1/5 , position = "jitter") +
  facet_grid(type ~ Gender) + 
  xlab("age group") +
  ylab("race completion time") + 
  ggtitle("age group completion time spread") + 
  theme_minimal()

type_group_count = df %>% group_by(type) %>% summarise(count=n())
df

df_m <- df[grep("M", df$Category),]#filter(df,Category == "Female")
df_f <- df[grep("F", df$Category),]



ggplot(df, aes(x=type, fill = Gender)) +
  geom_bar(position="fill")
#+ coord_flip()
ggsave(filename = './figure/03_type_of_participants_gender_wise.png',plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)


unique(df$type)

type_perc_count = df %>% group_by(Gender,type) %>%  summarise(count=n())
type_perc_count$type <- type_perc_count$type %>% replace(.=="", "walker")
#type_perc_count[is.na(type_perc_count)] <- "walker"
type_perc_count <- na.omit(type_perc_count) 

fc <- sum(type_perc_count[type_perc_count$Gender == 'Female',3])
mc <- sum(type_perc_count[type_perc_count$Gender == 'Male',3])


df_m_p <- type_perc_count[grep("M", type_perc_count$Gender),]#filter(df,Category == "Female")
df_f_p <- type_perc_count[grep("F", type_perc_count$Gender),]
#type_perc_count = df %>% group_by(Gender,type) %>%  summarise(count=n())

df_m_p <- df_m_p %>% mutate(perc = count/sum(count) * 100)
df_f_p <- df_f_p %>% mutate(perc = count/sum(count) * 100)

df_f_m_p <- rbind(df_m_p, df_f_p)

ggplot(df_m_p, mapping= aes(x=type, y=perc)) + 
  geom_col(color="lightblue", fill="lightblue")

ggsave(filename = './figure/03_type_of_participants_male.png',plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)

ggplot(df_f_p, mapping= aes(x=type, y=perc)) + 
  geom_col(color="lightblue", fill="lightblue")

ggsave(filename = './figure/03_type_of_participants_female.png',plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)

ggplot(df_f_m_p, mapping= aes(x=type, y=count, fill=Gender)) + 
  geom_col(position="fill")

ggsave(filename = './figure/03_type_of_participants_overall.png',plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)


ggplot(data=df, aes(x=Chip.Time, y=Gun.Time)) + 
  geom_line() +
  coord_cartesian(xlim = range(df$Chip.Time)) +
  geom_smooth(method="lm", se=FALSE)

ggsave(filename = './figure/04_relation_debtween_gun_time_chip_time.png',plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)

ggplot(data=df, aes(x=Chip.Time, y=Gun.Time)) + 
  geom_line() +
  coord_cartesian(xlim = range(df$Chip.Time)) +
  geom_smooth()

ggsave(filename = './figure/04_relation_debtween_gun_time_chip_time_smooth.png',plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)


ggplot(df, aes(Chip.Time)) +                  
  geom_line(aes(y=Chip.Time), colour="blue") +
  geom_line(aes(y=Gun.Time), colour="red") 


ggsave(filename = './figure/04_deviation_debtween_gun_time_chip_time.png',plot = last_plot(),
       units = "cm", width = 29.7, height = 21, dpi = 600)




#facet_grid(type ~ Gender) + 
ggplot(df_m, aes(x = Overall.Position, y =  Chip.Time)) + 
  geom_point(size = 5, alpha = 1/5 , position = "jitter") +
  xlab("age group") +
  ylab("race completion time") + 
  ggtitle("age group completion time spread") + 
  theme_minimal()


