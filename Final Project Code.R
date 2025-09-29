## FINAL PROJECT CODE ##

# LIBRARIES
library(tidyverse)
library(ggplot2)
library(usmap)
library(ggExtra)
library(car)
library(ggpubr)
library(hrbrthemes)
library(plotly)
library(dplyr)
library(viridis)


# DATASETS
Diabetes2 <- read.csv("Diabetes Data 2.csv")
US <- read.csv("US.csv")
ARE <- read.csv("ARE.csv")
CDI <- read.csv("cdi.csv")

#########################
# Fig. 1
f <- Diabetes2 %>% ggplot(aes(x=Gender, y=Percentage, fill=Gender)) + 
  geom_boxplot() +
  labs(x = "Gender",
       y = "Diabetes Percentage",
       title = "Fig. 1: Percentage of Male vs. Female Diabetes",
       subtitle = "2000-2021")
f


#########################
# Fig. 2
# Race
Race1 <- ARE[-c(89:286),]
Race <- as.factor(Race1$Race)
Race <- factor(Race1$Race)
levels(Race1$Race) <- c("Hispanic", "Asian", "Black", "White")

h <- ggplot(Race1, aes(x=Year, y=Race.Percentage, group=Race)) +
  geom_line(aes(color=Race)) +
  theme(legend.text = element_text(size = 6)) +
  scale_color_manual(name = "Race",
                     values = c("red", "green", "blue", "purple"),
                     labels = c("Hispanic", "Asian", "Black", "White")) +
  labs(x = "Year",
       y = "Percentage",
       title = "Fig. 2.1: Diabetes by Race",
       subtitle = "U.S. 2000-2021")

# Education
Education1 <- ARE[-c(67:286),]
Education <- as.factor(Education1$Education)
Education <- factor(Education1$Education, labels = c("< High School", "High School", "> High School"))

i <- ggplot(Education1, aes(x=Year, y=Education.Percentage)) +
  geom_line(aes(color=Education)) +
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Year",
       y = "Percentage",
       title = "Fig. 2.2: Diabetes by Education",
       subtitle = "U.S. 2000-2021")

# Age
Age1 <- ARE[-c(89:286),]
Age <- as.factor(Age1$Age)
Age <- factor(Age1$Age, labels = c("18-44","45-64","65-74","75+"))

j <- ggplot(Age1, aes(Year, Age.Percentage)) +
  geom_line(aes(color=Age)) +
  guides(color = guide_legend(title = "Age")) +
  scale_color_manual(name = "Race",
                     values = c("red", "green", "blue", "purple"),
                     labels = c("18-44", "45-64", "65-74", ">75")) +
  labs(x = "Year",
       y = "Percentage",
       title = "Fig. 2.3: Diabetes by Age",
       subtitle = "U.S. 2000-2021")

# Gender
k <- Diabetes2 %>% ggplot(aes(x=Year, y=Percentage)) + 
  geom_line(aes(color=Gender)) +
  labs(x = "Year",
       y = "Percentage",
       title = "Fig. 2.4: Diabetes by Gender",
       subtitle = "U.S. 2000-2021")

# Merge Plots onto 1 Page
ggarrange(h, i, j, k,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

#########################
# Fig. 3
fit2 <- lm(formula = Percentage ~ Stratification1 - 1, data= ARE)
summary(fit2)

summary(fit2)$coefficient

#########################
# Fig. 4
# Violin/Boxplot for Race
Race1 <- ARE[-c(89:286),]
t <- ggplot(Race1, aes(x=Race, y=Race.Percentage, fill=Race)) +
  geom_violin(width = 1.4) +
  geom_boxplot(width=0.3, color="tomato", alpha=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=15),
    axis.text.y = element_text(size=8)) +
  ylim(0,14) +
  coord_flip() +
  labs(x = "Race",
       y = "Race Percentage",
       title = "Fig. 4.1: Diagnosed Diabetes by Race 2000-2021",
       subtitle = "Boxplot within Violin Plot (U.S.)")

# Time Series plot for Race
h <- ggplot(Race1, aes(x=Year, y=Race.Percentage, group=Race)) +
  geom_line(aes(color=Race)) +
  geom_point(shape=21, color="purple", fill="#69b3a2", size=0.5) +
  theme(legend.text = element_text(size = 6)) +
  scale_color_manual(name = "Race",
                     values = c("red", "green", "blue", "purple"),
                     labels = c("Hispanic", "Asian", "Black", "White")) +
  labs(x = "Year",
       y = "Percentage",
       title = "Fig. 4.2: Diabetes by Race 2000-2021",
       subtitle = "Time Series (U.S.)")

# Using patchwork to apply both graphs to one page
t + h + plot_layout(ncol = 1, heights = c(2, 1))

#########################
# Fig. 5
map <- plot_usmap(data = US, values = "Percentage", color = "yellow") +
  scale_fill_continuous(low = "yellow", high = "red", name = "Diabetes % (2020)", label = scales::comma) +
  theme(legend.position = "right") +
  ggtitle("Fig. 5: Diabetes Percentage by State in 2020")
map

ggplotly(map, tooltip = c("region", "Percentage")) %>%
  layout(title = list(text = "Diabetes Percentage by State in 2020", font = list(size = 18, color = "black")),
         hoverlabel = list(font = list(size = 16)))

library(htmlwidgets)
saveWidget(map, file="USMap.html")

#########################
# Fig. 6
# Medication Use
a <- ggplot(Diabetes2, aes(x=Numbers.in.1000s, y=Medication.in.Millions..pills., colour=factor(Gender))) + geom_point() +
  labs( x= "Numbers in 1000s",
        y = "Medication in Millions (by Pills)",
        title = "Fig. 6.1: Diabetes vs. Medication",
        subtitle = "2000-2021") +
  guides(color = guide_legend(title = "Gender", reverse=TRUE)) +
  geom_smooth(se=FALSE) +
  theme(axis.text.x = element_text(size = 8),
        plot.title = element_text(size=12))

# Tobacco Use
Tobacco <- filter(Diabetes2, !is.na(Current.Tobacco.Use..Percentage.))

b <- ggplot(Tobacco, aes(x=Numbers.in.1000s, y=Current.Tobacco.Use..Percentage., colour=factor(Gender))) + geom_point() +
  labs( x = "Numbers in 1000s",
        y = "Current Tobacco Use (Percentage)",
        title = "Fig. 6.2: Diabetes vs. Tobacco",
        subtitle = "2000-2021") +
  guides(color = guide_legend(title = "Gender", reverse=TRUE)) +
  geom_smooth(se=FALSE) +
  theme(axis.text.x = element_text(size = 7),
        plot.title = element_text(size=12))

ggarrange(a, b,
          labels = c("A","B"),
          ncol=2, nrow=1)

#########################
# Fig. 7
fit5 <- lm(Percentage ~ Current.Tobacco.Use..Percentage. + Medication.in.Millions..pills., data = Diabetes2)
summary(fit5)

#########################

