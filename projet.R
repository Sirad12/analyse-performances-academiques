#répertoire de travail
setwd("C:/Users/Sira Dia/OneDrive/Desktop/Langage R/Projet_sira")
getwd()

#importation de la base de données

data = student_habits_performance
View(data)
head(data)

#analyse descriptive
install.packages("psych")
library(psych)

students = subset(data, select = -c(age, student_id, gender, netflix_hours, part_time_job, 
                    netflix_hours, parental_education_level, internet_quality, diet_quality, mental_health_rating, extracurricular_participation))
View(students)
describe(students)
summary(students)

pairs(students)
attach(students)
str(students)



#visualisation : histogramme
par(mfrow = c(2,2))
hist(exam_score, main="Histogram of exam_score", xlab="Exam score", col="gray", proba =T, nclass = 50)
lines(density(exam_score))

hist(study_hours_per_day, main="Histogram of study_hours/day", xlab="Study hours per day",  col="lightblue",proba =T, nclass = 50)
lines(density(study_hours_per_day))

hist(social_media_hours, main="Histogram of social_media_hours", xlab="Social_media_hours", col="lightgreen", proba =T, nclass = 50)
lines(density(social_media_hours))

hist(attendance_percentage, main="Histogram of attendance_percentage", xlab="Attendance_percentage", col="red", proba =T, nclass = 50)
lines(density(attendance_percentage))

hist(sleep_hours, main="Histogram of sleep_hours", xlab="Sleep_hours", col="pink", proba =T, nclass = 50)
lines(density(sleep_hours))

hist(exercise_frequency, main="Histogram of exercise_frequency" , xlab="Exercise_frequency", col="brown", proba =T, nclass = 50)
lines(density(exercise_frequency))

# Voir la linéarité entre les deux variables explicatives
par(mfrow=c(1,5))
plot(x=study_hours_per_day, y=exam_score, main = "exam_score~study_hours/day")
abline(lm(exam_score~study_hours_per_day), col=1)

plot(x=social_media_hours, y=exam_score, main = "exam_score~social_media_hours")
abline(lm(exam_score~social_media_hours), col=3)

plot(x=attendance_percentage, y=exam_score, main = "exam_score~attendance_percentage")
abline(lm(exam_score~attendance_percentage), col=1)

plot(x=sleep_hours, y=exam_score, main = "exam_score~sleep_hours")
abline(lm(exam_score~sleep_hours), col=1)

plot(x=exercise_frequency, y=exam_score, main = "exam_score~exercise_frequency")
abline(lm(exam_score~exercise_frequency), col=1)


#corrélation
cor(study_hours_per_day, exam_score)
cor(social_media_hours , exam_score)
cor(attendance_percentage, exam_score)
cor(sleep_hours, exam_score)
cor(exercise_frequency, exam_score)

#table de corrélation complète
cor_matrix = cor(students)
cor_matrix


#boxplot
par(mfrow=c(1,5))
boxplot(study_hours_per_day,main="study_hours/day")
boxplot(social_media_hours, main="social_media")
boxplot(attendance_percentage, main="attendance%")
boxplot(sleep_hours, main="sleep_hours")
boxplot(exercise_frequency, main="exercise_frequency")

#divise notre base
set.seed(101) #pour avoir les memes individus
sample=sample.int(n=nrow(students), size= floor(.80*nrow(students)), replace=F) 
train=students[sample,]
test=students[-sample,]

#estimation de modèle simple
model1=lm(exam_score~study_hours_per_day, data=train) 
summary(model1)

model2 = lm(exam_score ~ attendance_percentage, data=train)
summary(model2)

model3 = lm(exam_score ~ social_media_hours, data=train)
summary(model3)

model4 = lm(exam_score ~ sleep_hours, data=train)
summary(model4)

model5 = lm(exam_score ~ exercise_frequency, data=train)
summary(model5)

#régression linéaire multiple
model_full = lm(exam_score ~ study_hours_per_day + attendance_percentage + sleep_hours + social_media_hours + exercise_frequency, data=train)
summary(model_full)

model6 = lm(exam_score ~ study_hours_per_day +  attendance_percentage, data=train)
summary(model6)

#test de la normalité des résidus
shapiro.test(residuals(model_full))

install.packages("lmtest")
library(lmtest)

#fiabilité du model
bptest(model_full)
dwtest(model_full)

#visualisation avec ggplot2
install.packages("ggplot2")
library(ggplot2)

ggplot(students, aes(x=study_hours_per_day, y=exam_score)) +
  geom_point(color="blue") + geom_smooth(method="lm", col="red") +
  labs(title="Relation entre heures d’étude et score d’examen", x="Heures d'étude", y="Score d'examen")

ggplot(students, aes(x=attendance_percentage, y=exam_score)) +
  geom_point(color="blue") + geom_smooth(method="lm", col="red") +
  labs(title="Relation entre fréquence de présence et score d’examen", x="Fréqence de présence", y="Score d'examen")

ggplot(students, aes(x=social_media_hours, y=exam_score)) +
  geom_point(color="blue") + geom_smooth(method="lm", col="red") +
  labs(title="Relation entre temps passé sur les réseaux sociaux et score d’examen", x="Temps passé sur les réseaux sociaux ", y="Score d'examen")

ggplot(students, aes(x=sleep_hours, y=exam_score)) +
  geom_point(color="blue") + geom_smooth(method="lm", col="red") +
  labs(title="Relation entre durée moyenne du sommeil et score d’examen", x="Durée moyenne du sommeil par jour", y="Score d'examen")

ggplot(students, aes(x=exercise_frequency, y=exam_score)) +
  geom_point(color="blue") + geom_smooth(method="lm", col="red") +
  labs(title="Relation entre fréquence d'activié physique et score d’examen", x="Fréqence d'activité physique", y="Score d'examen")





install.packages("car")
library(car)
vif(model_full)  # VIF > 5 ou 10 = multicolinéarité possible

# ajustement du modèle
model_final = glm(exam_score ~ study_hours_per_day + attendance_percentage + sleep_hours + social_media_hours + exercise_frequency, data=train)
summary(model_final)

AIC(model_full, model6)


 # Charger les bons packages
library(lmtest)
install.packages("sandwich")
library(sandwich)

# Appliquer une variance-covariance robuste à l'hétéroscédasticité ET à l'autocorrélation
vcov_hac <- NeweyWest(model_full, lag = 1, prewhite = FALSE)

# Recalculer les tests des coefficients avec cette matrice robuste
coeftest(model_full, vcov = vcov_hac)




