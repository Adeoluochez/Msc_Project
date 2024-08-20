# loading libraries

library(haven) # for handling spss files in R
library(dplyr) # for data manipulation
library(ggplot2) # for graphs & plots
library(lme4)  # for multilevel modeling
library(mitools)  # for handling multiple imputations (plausible values)
library(naniar) # for visualizing missing data


# reading data set into R

pisa2022_stu <- read_sav("pisa2022_stu.sav")
pisa2022_sch <- read_sav("pisa2022_sch.sav")


#****SELECTION OF RELEVANT VARIABLES******

# selecting required variables in school data set for the selected country of study: England

sch_data <- pisa2022_sch %>% filter(REGION==82611)%>%
  select(CNTSCHID,SC182Q01WA01,SC182Q01WA02, SC182Q06WA01, SC182Q06WA02, SC182Q08JA01, SC182Q08JA02,SC182Q10JA01, 
         SC182Q10JA02, SC025Q02NA, SC183Q02JA,SC183Q03JA, SC183Q04JA, SC184Q01JA,SC184Q02JA, SC184Q03JA, SC184Q04JA,
         SC184Q05JA, SC184Q06JA, SC184Q07JA, SC175Q01JA,SC053Q05NA, SC053Q06NA, SC180Q01JA, SC181Q01JA, SC181Q02JA, 
         SC181Q03JA, SC013Q01TA)

# selecting required variables in student data set for the selected country of study: England

stu_data <- pisa2022_stu %>% filter(REGION==82611) %>%
  select(CNTSCHID, ST004D01T,PV1MATH, PV2MATH, PV3MATH, PV4MATH, PV5MATH, PV6MATH, PV7MATH, PV8MATH,
         PV9MATH, PV10MATH)                                    

#****MERGING THE TWO DATA SETS******

#merging the two data sets together

my_data <- stu_data %>% inner_join(sch_data, by = "CNTSCHID")


#****DATA CLEANING******

#renaming variables in my data set

my_data_2 <- my_data %>%
  rename(
    gender = ST004D01T,
    ft_maths_tchs = SC182Q01WA01,
    pt_maths_tchs = SC182Q01WA02,
    ft_full_cert = SC182Q06WA01,
    pt_full_cert = SC182Q06WA02,
    ft_lvl6_tch = SC182Q08JA01,
    pt_lvl6_tch = SC182Q08JA02,
    ft_lvl5_tch = SC182Q10JA01,
    pt_lvl5_tch = SC182Q10JA02,
    percent_tchAttendPD = SC025Q02NA,
    pd_SchInviteSpecialist = SC183Q02JA,
    pd_SchOrganiseWorkshop = SC183Q03JA,
    pd_SchOrganiseSpecificWorkshop = SC183Q04JA,
    sch_offer_pd_MathsContent = SC184Q01JA,
    sch_offer_pd_MathsPedagogy = SC184Q02JA,
    sch_offer_pd_MathsCurriculum = SC184Q03JA,
    sch_offer_pd_IntegrateDigitalResources = SC184Q04JA,
    sch_offer_pd_ImproveStudentCriticalThinking = SC184Q05JA,
    sch_offer_pd_MathsAssessment = SC184Q06JA,
    sch_offer_pd_AddressStudentsNeeds = SC184Q07JA,
    minutes_spent_in_class = SC175Q01JA,
    sch_offer_maths_club = SC053Q05NA,
    sch_offer_maths_competitions = SC053Q06NA, 
    sch_offer_aditional_lessons = SC180Q01JA,
    mathsLesson_enrichment = SC181Q01JA,
    mathsessons_remedial = SC181Q02JA,
    mathslessons_others= SC181Q03JA,
    sch_type = SC013Q01TA
    
  )

# Exploring our data set

dim(my_data_2)
head(my_data_2)

# Identify missing data

summary(my_data_2)
sapply(my_data_2, function(x) sum(is.na(x)))
View(naniar::miss_var_summary(my_data_2))

# Two variables had complete missing values and as such had to be removed and the data set being re-evaluated
# removing two variables with complete NAs "sch_offer_maths_club" and sch_offer_maths_competitions

my_data_3 <-my_data_2 %>%
  select(-sch_offer_maths_club, -sch_offer_maths_competitions)

#Run the whole missing data analysis again on the new data set "my_data_3"
# Identify missing data

summary(my_data_3)
sapply(my_data_3, function(x) sum(is.na(x)))
View(naniar::miss_var_summary(my_data_3))

# Visualize missing data with a simple plot

naniar::vis_miss(my_data_3)

# Explore patterns of missing data using upset plot

naniar::gg_miss_upset(my_data_3)

# removing NAs

my_data_4 <-my_data_3 %>% na.omit(my_data_3)


# cleaning up the newly prepared data set for analysis
#
#recode of some variables in the data set

# gender variable: 1 for Female, 2 for Male

my_data_4 <- my_data_4 %>%
  mutate(gender = factor(gender, 
                         levels = c(1, 2), 
                         labels = c("female", "male")))

# school type variable: 1 for public, 2 for private

my_data_4 <- my_data_4 %>%
  mutate(sch_type = factor(sch_type, 
                         levels = c(1, 2), 
                         labels = c("public", "private")))

# PD - school invite specialist to conduct in-service Training: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(pd_SchInviteSpecialist = factor(pd_SchInviteSpecialist, 
                           levels = c(1, 2), 
                           labels = c("Yes", "No")))

# PD - school organizes in-service workshop: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(pd_SchOrganiseWorkshop = factor(pd_SchOrganiseWorkshop, 
                                         levels = c(1, 2), 
                                         labels = c("Yes", "No")))

# PD - school organizes specific workshop: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(pd_SchOrganiseSpecificWorkshop = factor(pd_SchOrganiseSpecificWorkshop, 
                                         levels = c(1, 2), 
                                         labels = c("Yes", "No")))

# PD - school offers professional development in maths content: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(sch_offer_pd_MathsContent = factor(sch_offer_pd_MathsContent, 
                                                 levels = c(1, 2), 
                                                 labels = c("Yes", "No")))

# PD - school offers professional development in maths pedagogy: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(sch_offer_pd_MathsPedagogy = factor(sch_offer_pd_MathsPedagogy, 
                                            levels = c(1, 2), 
                                            labels = c("Yes", "No")))

# PD - school offers professional development in maths curriculum: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(sch_offer_pd_MathsCurriculum = factor(sch_offer_pd_MathsCurriculum,
                                               levels = c(1, 2),
                                               labels = c("Yes", "No")))

# PD - school offers professional development in integrating digital: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(sch_offer_pd_IntegrateDigitalResources = factor(sch_offer_pd_IntegrateDigitalResources,
                                                         levels = c(1, 2),
                                                         labels = c("Yes", "No")))

# PD - school offers professional development in improving students critical thinking: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(sch_offer_pd_ImproveStudentCriticalThinking = factor(sch_offer_pd_ImproveStudentCriticalThinking,
                                                         levels = c(1, 2),
                                                         labels = c("Yes", "No")))

# PD - school offers professional development in mathematics assessment: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(sch_offer_pd_MathsAssessment = factor(sch_offer_pd_MathsAssessment,
                                                              levels = c(1, 2),
                                                              labels = c("Yes", "No")))

# PD - school offers professional development in assessing students needs: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(sch_offer_pd_AddressStudentsNeeds = factor(sch_offer_pd_AddressStudentsNeeds,
                                               levels = c(1, 2),
                                               labels = c("Yes", "No")))

# school offers additional lessons - Enrichment: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(mathsLesson_enrichment = factor(mathsLesson_enrichment,
                                              levels = c(1, 2),
                                              labels = c("Yes", "No")))

# school offers additional lessons - Remedial: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(mathsessons_remedial = factor(mathsessons_remedial,
                                         levels = c(1, 2),
                                         labels = c("Yes", "No")))

# school offers additional lessons - Others: 1 for Yes, 2 for No

my_data_4 <- my_data_4 %>%
  mutate(mathslessons_others = factor(mathslessons_others,
                                       levels = c(1, 2),
                                       labels = c("Yes", "No")))


#****DESCRIPTIVE ANALYSIS******

# Here we try to describe some characteristics of the data set as well answer some objective questions

# Calculate the proportion of each gender

gender_proportion <- my_data_4 %>%
  count(gender) %>%
  mutate(proportion = n / sum(n))

print(gender_proportion)

# Calculate the proportion of each school

school_proportion <- my_data_4 %>%
  count(sch_type) %>%
  mutate(proportion = n / sum(n))

print(school_proportion)

#****OBJECTIVE 1****** - Do the schools offer additional lessons in maths

# Calculate school offer additional lessons

additional_lesson <- my_data_4 %>%
  count(sch_offer_aditional_lessons) %>%
  mutate(proportion = n / sum(n))

print(additional_lesson)

# Calculate school offer additional lessons - Enrichment

Lesson_Enrichment <- my_data_4 %>%
  count(mathsLesson_enrichment) %>%
  mutate(proportion = n / sum(n))

print(Lesson_Enrichment)

# Calculate school offer additional lessons - Remedial

Lesson_Remedial <- my_data_4 %>%
  count(mathsessons_remedial) %>%
  mutate(proportion = n / sum(n))

print(Lesson_Remedial)

# Calculate school offer additional lessons - Others

Lesson_Others <- my_data_4 %>%
  count(mathslessons_others) %>%
  mutate(proportion = n / sum(n))

print(Lesson_Others)

# Tabulate schools offering additional classes by school type

schtype_enrichment <- table(my_data_4$sch_type, my_data_4$mathsLesson_enrichment)
schtype_remedial <- table(my_data_4$sch_type, my_data_4$mathsessons_remedial)
schtype_others <- table(my_data_4$sch_type, my_data_4$mathslessons_others)

# Calculate the percentage of schools offering remedial classes (column percentage)

schtype_enrichment_percentage <- prop.table(schtype_enrichment, margin = 1) * 100
schtype_remedial_percentage <- prop.table(schtype_remedial, margin = 1) * 100
schtype_others_percentage <- prop.table(schtype_others, margin = 1) * 100

# View the tabulated data

print(schtype_enrichment)
print(schtype_remedial)
print(schtype_others)
print(schtype_enrichment_percentage)
print(schtype_remedial_percentage)
print(schtype_others_percentage)

#****OBJECTIVE 2****** - How many minutes on average are spent in teaching mathematics

# Average time spent in lessons

avg_time_spent <- mean(my_data_4$minutes_spent_in_class)
print(avg_time_spent)

# Calculate the average time spent in class by school type

avg_time_sch <- my_data_4 %>%
  group_by(sch_type) %>%
  summarise(avg_time_ = mean(minutes_spent_in_class))

# View the result
print(avg_time_sch)

# Box plot to show the time spent in class by school type

ggplot(my_data_4, aes(x = sch_type, y = minutes_spent_in_class, col = sch_type)) +
  geom_boxplot() +
  labs(x = "School Type", y = "Time Spent in Class (mins)") +
  stat_summary(fun = mean,
               colour="darkblue",
               geom = "point",
               shape = 18,
               size = 3,
               show.legend = FALSE) +
  stat_summary(fun = mean,
               colour = "darkblue",
               geom = "text",
               show.legend = FALSE,
               vjust = -0.7,
               aes(label = round(after_stat(y), digits = 0)))


#****OBJECTIVE 3****** - Does your School offer professional development to maths teachers

# Calculate school offer professional development by inviting specialists

sch_inv_spe <- my_data_4 %>%
  count(pd_SchInviteSpecialist) %>%
  mutate(proportion = n / sum(n))

print(sch_inv_spe)

# Calculate school offer professional development by organizing workshops

sch_org_wor <- my_data_4 %>%
  count(pd_SchOrganiseWorkshop) %>%
  mutate(proportion = n / sum(n))

print(sch_org_wor)

# Calculate school offer professional development by organizing workshops for specific maths teachers

sch_org_spe_wor <- my_data_4 %>%
  count(pd_SchOrganiseSpecificWorkshop) %>%
  mutate(proportion = n / sum(n))

print(sch_org_spe_wor)

# Types of professional activities offered by schools - math content

math_content <- my_data_4 %>%
  count(sch_offer_pd_MathsContent) %>%
  mutate(proportion = n / sum(n))

print(math_content)

# Types of professional activities offered by schools - math pedagogy

math_pedagogy <- my_data_4 %>%
  count(sch_offer_pd_MathsPedagogy) %>%
  mutate(proportion = n / sum(n))

print(math_pedagogy)

# Types of professional activities offered by schools - math curriculum

math_curriculum <- my_data_4 %>%
  count(sch_offer_pd_MathsCurriculum) %>%
  mutate(proportion = n / sum(n))

print(math_curriculum)

# Types of professional activities offered by schools - integrate digital resources

math_int_digital <- my_data_4 %>%
  count(sch_offer_pd_IntegrateDigitalResources) %>%
  mutate(proportion = n / sum(n))

print(math_int_digital)

# Types of professional activities offered by schools - improve students critical thinking

math_imp_cri_thi <- my_data_4 %>%
  count(sch_offer_pd_ImproveStudentCriticalThinking) %>%
  mutate(proportion = n / sum(n))

print(math_imp_cri_thi)

# Types of professional activities offered by schools - maths assessment

math_assessment <- my_data_4 %>%
  count(sch_offer_pd_MathsAssessment) %>%
  mutate(proportion = n / sum(n))

print(math_assessment)

# Types of professional activities offered by schools - address students needs

math_add_stu_needs <- my_data_4 %>%
  count(sch_offer_pd_AddressStudentsNeeds) %>%
  mutate(proportion = n / sum(n))

print(math_add_stu_needs)

#****HYPOTHESES 1******

#Is commitment to professional development among teachers lower in public schools compared to teachers in private schools.

# A t-test of the hypotheses:
#
# Null hypothesis H_0: mu_public school >= mu_private school
# Alternative hypothesis H_1: mu_public school < mu_private school
#
# This is a one-tailed test

t.test(percent_tchAttendPD ~ sch_type,
       data = my_data_4,
       var.equal = TRUE,
       alternative = "less")


#****APPROPRAITENESS OF RANDOM EFFECT MODEL******
#*
# test for running appropriateness of a random effect model
#
# Create a vector of PV column names

pv_cols <- paste0("PV", 1:10, "MATH")

# Initialize an empty list to store models

empty_models <- list()

# Loop through PV columns and create models

for (i in 1:length(pv_cols)) {
  formula <- as.formula(paste(pv_cols[i], "~ 1 + (1 | sch_type)"))
  empty_models[[i]] <- lmer(formula, data = my_data_4)
}

# Print summaries

for (i in 1:length(empty_models)) {
  cat("\nSummary for", pv_cols[i], ":\n")
  print(summary(empty_models[[i]]))
}

# Now that we have observed there is measurable variability in mathematics scores attributable to school type we will go
# ahead to compute the HLM with random effect


#****HYPOTHESES 2******

#Does size of mathematics department have effect on mathematics performance of students

# Null hypothesis H_0: size of mathematics department have effect on mathematics performance of students
# Alternative hypothesis H_1: size of mathematics department does not have effect on mathematics performance of students
#
# ** Fit the Random effect regression model 10 times because of our 10 plausible values to the DV (maths performance)**
#
# We will fit the following model:
#
# Mathematics Performance 1 = beta_0 + beta_1*ft_maths_tchs + (1|sch_type) + error term
# Mathematics Performance 2 = beta_0 + beta_1*ft_maths_tchs + (1|sch_type) + error term
# Mathematics Performance 3 = beta_0 + beta_1*ft_maths_tchs + (1|sch_type) + error term
# Mathematics Performance 4 = beta_0 + beta_1*ft_maths_tchs + (1|sch_type) + error term
# Mathematics Performance 5 = beta_0 + beta_1*ft_maths_tchs + (1|sch_type) + error term
# Mathematics Performance 6 = beta_0 + beta_1*ft_maths_tchs + (1|sch_type) + error term
# Mathematics Performance 7 = beta_0 + beta_1*ft_maths_tchs + (1|sch_type) + error term
# Mathematics Performance 8 = beta_0 + beta_1*ft_maths_tchs + (1|sch_type) + error term
# Mathematics Performance 9 = beta_0 + beta_1*ft_maths_tchs + (1|sch_type) + error term
# Mathematics Performance 10 = beta_0 + beta_1*ft_maths_tchs + (1|sch_type) + error term
#
#
# Here,
# full time maths teachers is the explanatory variable or the predictor
# academic performance(PV1 MATH to PV10MATH) is the outcome variable
# school type is the random intercept
# #
# This is an example of a  multilevel (hierarchical) linear model (HLM) with random effect (school type)

# Create a list to store models for each plausible value

models_obj5 <- list()

for(i in 1:10) {
  pv_name <- paste0("PV", i, "MATH")
  models_obj5[[i]] <- lmer(get(pv_name) ~ ft_maths_tchs + (1|sch_type), data = my_data_4)
}

# Combine results using MIcombine (using Rubin's rules)

coef_list_obj5 <- lapply(models_obj5, fixef)
vcov_list_obj5 <- lapply(models_obj5, function(x) as.matrix(vcov(x)))
combined_results_obj5 <- MIcombine(results = coef_list_obj5, variances = vcov_list_obj5)

summary(combined_results_obj5)

#****HYPOTHESES 3******

#Does school commitment to professional development of maths teachers have impact on student achievement

# Create a list to store models for each plausible value

models_obj6 <- list()

for(i in 1:10) {
  pv_name <- paste0("PV", i, "MATH")
  models_obj6[[i]] <- lmer(get(pv_name) ~ pd_SchInviteSpecialist + pd_SchOrganiseWorkshop + pd_SchOrganiseSpecificWorkshop + (1|sch_type), data = my_data_4)
}

# Combine results using MIcombine (using Rubin's rules)

coef_list_obj6 <- lapply(models_obj6, fixef)
vcov_list_obj6 <- lapply(models_obj6, function(x) as.matrix(vcov(x)))
combined_results_obj6 <- MIcombine(results = coef_list_obj6, variances = vcov_list_obj6)

summary(combined_results_obj6)

#****HYPOTHESES 4******

# Does schools who have more level 6 teachers perform better in mathematics than schools with Level 5 teachers

# Create a list to store models for each plausible value

models_obj7 <- list()

for(i in 1:10) {
  pv_name <- paste0("PV", i, "MATH")
  models_obj7[[i]] <- lmer(get(pv_name) ~ ft_lvl6_tch + ft_lvl5_tch + (1|sch_type), data = my_data_4)
}

# Combine results using MIcombine (using Rubin's rules)

coef_list_obj7 <- lapply(models_obj7, fixef)
vcov_list_obj7 <- lapply(models_obj7, function(x) as.matrix(vcov(x)))
combined_results_obj7 <- MIcombine(results = coef_list_obj7, variances = vcov_list_obj7)

summary(combined_results_obj7)

