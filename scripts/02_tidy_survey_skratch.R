# Source libraries  -----------------------------------------------------------

source(here::here("scripts", "00_libraries.R"))

# -----------------------------------------------------------------------------


# Tidy survey data ------------------------------------------------------------
# Data from Qualtrics 10/21 ---------------------------------------------------

tidy_data <- read.csv("data/tidy/tidy_survey_partial.csv")



# Me messing around and trying graphs / stats stuff so don't take this seriously
# Lets do a general descriptive stuff on this to find out information

# finding # of participants

# find # of participants per group
n_participant <- tidy_data$responseid %>%  unique() %>%  length() %>% View
n_trials <- tidy_data %>% nrow()
# total = 91 participants

# find # of participants by class format
tidy_data %>%
  group_by(class_format) %>%
  summarize(., totals = n_distinct(responseid)) %>%  View

# f = 58
# h = 8
# o = 30

# find out their year/promoción

tidy_data %>%
  group_by(year) %>%
  summarize(., totals = n_distinct(responseid)) %>%  View

# 1 = 40
# 2 = 28
# 3 = 13
# 4 = 10


# okay so lets look at practice_rate ~ practice type
tidy_data %>%
  ggplot(., aes(x = class_format, y = practicerate)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid("practice")

# ^ that needs to be made prettier

# then you want to look at progress overall etc

tidy_data %>%
  group_by(progress) %>%
  summarize(., totals = n_distinct(responseid)) %>%  View

# No 5
# Yes 82


# we can say something like the majority said that they progressed in their language skills
# then we can go into practice amount as a function of format (actually
# you should double check that w/ dependent and independent variables)
# actually i think we need to do an ordinal regression



practice_stats <- polr(as.factor(class_format) ~ practicerate, data = tidy_data, Hess = TRUE, method = c("logistic"))
summary(practice_stats)

practice_stats1 <- polr(as.factor(class_format) ~ practicerate + practice, data = tidy_data, Hess = TRUE, method = c("logistic"))
summary(practice_stats1)

practice_stats2 <- polr(as.factor(class_format) ~ practicerate + practice + progress, data = tidy_data, Hess = TRUE, method = c("logistic"))
summary(practice_stats2)

## It seems like nothing is interesting here, which we could make the argument that they're learning
## just as well in both formats, so thats cool.

# then you want to look at enrollment based on anxiety, work, and commute
# whatever code you do for above, is going to be the same thing here just with different variables

enrollment_stats <- polr(as.factor(class_format) ~ work, data = tidy_data, Hess = TRUE, method = c("logistic"))
summary(enrollment_stats)

enrollment_stats1 <- polr(as.factor(class_format) ~ work + work_hours, data = tidy_data, Hess = TRUE, method = c("logistic"))
summary(enrollment_stats1)

enrollment_stats2 <- polr(as.factor(class_format) ~ work + work_hours + commute, data = tidy_data, Hess = TRUE, method = c("logistic"))
summary(enrollment_stats2)

# lets look at anxiety now

anxiety <- polr(as.factor(format_anxiety) ~ anxiety, data = tidy_data, Hess = TRUE, method = c("logistic"))
summary(anxiety)

anxiety1 <- polr(as.factor(format_anxiety) ~ anxiety + class_format, data = tidy_data, Hess = TRUE, method = c("logistic"))
summary(anxiety1)


# say something about online pre covid
tidy_data %>%
  group_by(online_pre) %>%
  summarize(., totals = n_distinct(responseid)) %>%  View

# No 81
# Yes 9

# Before COVID, the vast majority of the people had never taken an online class, so if we look
# at the format of their classes now (figure this out) so we can say, people who have taken
# online classes before covid either stick to online or go back to in person



# I want to look at practice by skill, like average #
# So i should filter?

reading_data <- tidy_data %>%
  filter(practice == "reading")

reading_data %>%
  summary(practicerate) %>%  View

# Results:
# Min.   :1.000
# 1st Qu.:3.000
# Median :4.000
# Mean   :3.953
# 3rd Qu.:5.000
# Max.   :5.000


writing_data <- tidy_data %>%
  filter(practice == "writing")

table(writing_data['practicerate'])

# results
# Min.   :1.000
# 1st Qu.:3.000
# Median :4.000
# Mean   :3.872
# 3rd Qu.:5.000
# Max.   :5.000


listening_data <- tidy_data %>%
  filter(practice == "listening")

table(listening_data['practicerate'])

# results
# Min.   :1.000
# 1st Qu.:3.000
# Median :4.000
# Mean   :3.581
# 3rd Qu.:4.000
# Max.   :5.000

speaking_data <- tidy_data %>%
  filter(practice == "speaking")

table(speaking_data['practicerate'])

# results
# Min.   :1.000
# 1st Qu.:3.000
# Median :4.000
# Mean   :3.744
# 3rd Qu.:4.000
# Max.   :5.000


# we need info on # of responses of face-to-face vs online
# and i'm too lazy to filter and make this nice so i'm cheating lol
table(reading_data['motivation.1'])

# face-to-face: 71
# online: 14
# blanks: 6

# going to do the same thing to cheat and look at efficiency
table(reading_data['effective'])

#  1  2  3  4  5
#  6 16 36 24  3

table(reading_data['efficient'])

#    More efficient        Niether        Similar
# 5             12             55             19

