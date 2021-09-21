# all libs loaded here
library(ggplot2)
library(dplyr)

#do it when switiching between completely un-related scrips 
rm(list = ls())
graphics.off()

# check for versions (handy when working on diff versions of R)
sessionInfo()

#load a sample dataset
df <- ggplot2::mpg
# some doc level info about the loaded dataset
help("mpg")

#checking unique value of a cols 
unique(df$trans)
#other way for unique col values
df %>%  distinct(drv)

# for counting the occurance of each disticnt val in a col
df %>%  group_by(drv) %>% count()

# ! shortcut for pipe in r studio ctrl + shift + m
#?mutate  >> ! for adding and deleting cols , calls fx to work on new col values

df <- df %>%  
  mutate(transmission = substr(trans, 1, 1)) %>% 
  mutate(transmission = case_when(transmission == "a" ~ "automatic trans.",
                                  transmission == "m" ~ "manual trans."))
df <- df %>%  
  mutate(type_of_drive = case_when(drv == "f" ~ "front-wheel",
                                   drv == "r" ~ "rear-wheel",
                                   drv == "4" ~ "4-wheel drive"))

# converting character variables to factor variables

df <- df  %>% 
  mutate(type_of_drive = factor(type_of_drive, levels = c("front-wheel",
                                                          "rear-wheel",
                                                          "4-wheel drive")),
         transmission = factor(transmission, levels = c("automatic trans.",
                                                        "manual trans.")))
# other way without using dplyr lib , above is when levels are know
#df$type_of_drive <- as.factor(df$type_of_drive)

#starting to plot basics
ggplot(data = df) # empty canvass
ggplot(data = df, mapping = aes(x = displ, y = hwy)) # adding the aesthetics x and y
# above we are plotting mileage on y and engine size on x

#now we add geometry>> geom, grapher object, depends on x and y 
ggplot(df,aes(x = displ, y = hwy)) + geom_point(size = 5,alpha = 0.1)

#now we add facets ,splitting original plot to multiple 
ggplot(df , aes(x = displ, y = hwy)) + geom_jitter(size = 5,alpha = 1/3) + 
  facet_grid(transmission ~ type_of_drive, scales = "free")

#adding some statistical layers to it
# a linear regression for each sub-plots/facets
ggplot(df , aes(x = displ, y = hwy)) + geom_jitter(size = 5,alpha = 1/3) + 
  facet_grid(transmission ~ type_of_drive, scales = "free") + 
  geom_smooth(method ="lm")

#working with co-ordinate layer in ggplot2, work with scales etc 
# todo


#assignment 1
ggplot(df , aes(x = displ, y = hwy)) + geom_point(size = 8, alpha = 1/5, color = "red")

ggplot(df , aes(x = displ, y = hwy)) + 
  geom_point(size = 8, alpha = 1/5, color = "red", position = "jitter") +
  xlab("Engine Displacement (volume in liters)") +
  ylab("Highway miles per gallon (MPG)") + 
  ggtitle("Car fuel consumption")

ggplot(df , aes(x = displ, y = hwy, color = transmission)) + 
  geom_point(size = 8, alpha = 1/5,  position = "jitter") +
  xlab("Engine Displacement (volume in liters)") +
  ylab("Highway miles per gallon (MPG)") + 
  ggtitle("Car fuel consumption")

ggplot(df , aes(x = cyl, y = hwy)) + 
  geom_point(size = 8, alpha = 1/5,color = "red" , position = "jitter") +
  xlab("Number of cylinders") +
  ylab("Highway miles per gallon (MPG)") + 
  ggtitle("Car fuel consumption") + 
  theme_minimal()



ggplot(df , aes(x = cyl, y = hwy)) + 
  geom_point(size = 8, alpha = 1/5,color = "red" , position = "nudge") +
  xlab("Number of cylinders") +
  ylab("Highway miles per gallon (MPG)") + 
  ggtitle("Car fuel consumption") + 
  theme_minimal()

ggplot(data=df, aes(x=Chip.Time, y=Gun.Time)) + 
  geom_line() +
  coord_cartesian(xlim = range(df$Chip.Time)) +
  geom_smooth(method="lm", se=FALSE)


ggplot(df, aes(Chip.Time)) +                  
  geom_line(aes(y=Chip.Time), colour="blue") +
  geom_line(aes(y=Gun.Time), colour="red") 


w <- as.data.frame(cbind(
  c(0.2446884, 0.0000000, 8.4293893), 
  c(0.3173719, 0.0000000, 4.9985040), 
  c(0.74258410, 0.08592243, 2.22526463)))
w$refseq <- c("NM_000014", "NM_000015", "NM_000016")

library(ggplot2)
library(reshape2)
w.plot <- melt(w) 
w.plot
p <- ggplot(aes(x=value, colour=variable), data=w.plot)
p + geom_density()




# data wranggling



