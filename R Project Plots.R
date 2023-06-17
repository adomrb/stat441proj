library(tidyverse)
library(readxl)
library(scales)
library(RColorBrewer)
data <- read_excel("Project Analysis.xlsx", 
                                 sheet = "Clean Data")
View(data)
head(data)

# Plot salary distribution.

ggplot(data, aes(`salary estimate`)) +
  labs(x='Salary Estimate', y='Frequency',
       title='Salary Estimate Distribution',
       caption='Figure 2.1') +
  geom_histogram(bins=50, color='dodgerblue4', fill='dodgerblue') +
  scale_x_continuous(labels=comma)

summary(data$`salary estimate`)

# Plot data analyst salary distribution.

data.A <- data %>%
  select('salary estimate', job_simpl) %>%
  filter(job_simpl == 'Data Analyst')
ggplot(data.A, aes(`salary estimate`)) +
  labs(x='Salary Estimate', y='Frequency',
       title='Data Analyst Salary Distribution',
       caption='Figure 2.2') +
  geom_histogram(bins=40, color='seagreen4', fill='seagreen1') +
  scale_x_continuous(labels=comma)

summary(data.A$`salary estimate`)

# Plot data scientist salary distribution.

data.S <- data %>%
  select('salary estimate', job_simpl) %>%
  filter(job_simpl == 'Data Scientist')
ggplot(data.S, aes(`salary estimate`)) +
  labs(x='Salary Estimate', y='Frequency',
       title='Data Scientist Salary Distribution',
       caption='Figure 2.3') +
  geom_histogram(bins=50, color='tomato4', fill='tomato') +
  scale_x_continuous(labels=comma)

# Plot data engineer salary distribution.

data.E <- data %>%
  select('salary estimate', job_simpl) %>%
  filter(job_simpl == 'Data Engineer')
ggplot(data.E, aes(`salary estimate`)) +
  labs(x='Salary Estimate', y='Frequency',
       title='Data Engineer Salary Distribution',
       caption='Figure 2.4') +
  geom_histogram(bins=40, color='mediumorchid4', fill='mediumorchid1') +
  scale_x_continuous(labels=comma)

# Plot python proportions.

data.P <- data %>%
  select(python_yn, job_simpl) %>%
  group_by(job_simpl) %>%
  summarise(python_proportion = mean(python_yn))
ggplot(data.P) +
  geom_col(aes(x=job_simpl, y=python_proportion, fill=job_simpl), color='black') +
  scale_fill_brewer(palette="Set1") +
  labs(x=NULL, y=NULL, 
  title='Proportion of Job Descriptions Listing Python', caption='Figure 2.5') +
  theme(legend.position = 'none')

# Plot excel proportions.

data.E <- data %>%
  select(excel_yn, job_simpl) %>%
  group_by(job_simpl) %>%
  summarise(excel_proportion = mean(excel_yn))
ggplot(data.E) +
  geom_col(aes(x=job_simpl, y=excel_proportion, fill=job_simpl), color='black') +
  scale_fill_brewer(palette='Set1') +
  labs(x=NULL, y=NULL, 
  title='Proportion of Job Descriptions Listing Excel', caption='Figure 2.6') +
  theme(legend.position = 'none')

# Plot remote positions.

r1 <- data %>%
  select(location, job_simpl) %>%
  filter(location == 'Remote') %>%
  count(job_simpl)
r2 <- data %>%
  select(job_simpl) %>%
  count(job_simpl)
data.R <- tibble(job = r1$job_simpl, remote = r1$n / r2$n)
ggplot(data.R) +
  geom_col(aes(x=job, y=remote, fill=job), color='black') +
  scale_fill_brewer(palette='Set1') +
  labs(x=NULL, y=NULL,
       title='Proportion of Job Listings Classified as Remote',
       caption='Figure 2.7') +
  theme(legend.position = 'none')

# Import and plot sampling dist.

re <- read_excel("Project Analysis.xlsx", 
                  sheet = "Random Sampling Variances", 
                  col_names = FALSE)

re <- re %>%
  rename(s.var = ...1)
mu <- mean(re$s.var)
sigma <- sd(re$s.var)
ggplot(re) +
  geom_histogram(aes(x=s.var, y=after_stat(density)),
                 bins=50,
                 color='black',
                 fill='darkslategray3') +
  scale_x_continuous(labels=comma) +
  scale_y_continuous(labels=NULL) +
  labs(x='Sample Variance', y='Frequency',
       title='Sampling Distribution of the Sample Variance',
       caption='Figure 4.1') +
  stat_function(fun=dnorm,
                args=list(mean=mu, sd=sigma),
                linewidth=1.2,
                color='darkslategray')

mu
mu2 <- var(data$`salary estimate`)
mu2
abs(mu-mu2) / mu2

