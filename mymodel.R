

#setting the working directory

setwd("C:\\Users\\ELCOT\\Desktop")
getwd()

#replacing the empty cell wit "NA"

df<-read.csv("dsc model.csv",na.strings = c("","NA"),header = TRUE)
str(df)
class(df)



#checking for missing values values present in the data

sapply(df,function(x)sum(is.na(x)))
str(df)
complete.cases(df)
df<- df[complete.cases(df), ]
str(df)
sum(is.na(df))

######Removing the variables

df$Performance_PG<-NULL 
df$Performance_10<-NULL
df$Performance_12<-NULL
df$Performance_UG<-NULL
df$Current.City<-NULL
df$Institute<-NULL


########package installing########

library(dplyr)
library(tidyr)


###########using comma separator splitting the values and storing it in skill

df<-df%>% separate(Other.skills,c("skill1","skill2","skill3","skill4","skill5","skill6","skill7","skill8","skill9","skill10","skill11","skill12","skill13","skill14","skill15","skill16"),sep = ",")
df$skill1


df<-df%>% separate(skill1,c("a","b","c","d"),sep = " ")
df$d <-NULL

df$a[is.na((df$a))]<-""
df$b[is.na((df$b))]<-""
df$c[is.na((df$c))]<-""
df<-unite(df,skill1,a,b,c,sep = " ")
df$skill1
sum(is.na(df$skill1))


df$skill1<-gsub("Machine Learning " ,3,df$skill1)
df$skill1<-gsub("Natural Language Processing"  ,3,df$skill1)
df$skill1<-gsub( "Deep Learning " ,3,df$skill1)
df$skill1<-gsub( "SQL  " ,3,df$skill1)
df$skill1<-gsub( "Amazon Web Services" ,3,df$skill1)#####
df$skill1<-gsub("NoSQL",3,df$skill5)#####
df$skill1<-gsub("MS-Excel  " ,3,df$skill1)


class(df$skill1)
df$skill1<-as.numeric(df$skill1)
sum(is.na(df$skill1))
df$skill1[is.na((df$skill1))]<-0



######### Skill2  ######################



df<-df%>% separate(skill2,c("a2","b2","c2","d2","e2"),sep = " ")
df$e2 <-NULL

df$a2[is.na((df$a2))]<-""
df$b2[is.na((df$b2))]<-""
df$c2[is.na((df$c2))]<-""
df$d2[is.na((df$d2))]<-""
df<-unite(df,skill2,a2,b2,c2,d2,sep = " ")
df$skill2

df$skill2<-gsub( " Machine Learning ",3,df$skill2)
df$skill2<-gsub(" Natural Language Processing",3,df$skill2)
df$skill2<-gsub( " Deep Learning " ,3,df$skill2)
df$skill2<-gsub( " SQL  ",3,df$skill2)
df$skill2<-gsub( "Amazon Web Services" ,3,df$skill2)######
df$skill2<-gsub("NoSQL",3,df$skill5)########
df$skill2<-gsub(" MS-Office  ",3,df$skill2)

class(df$skill2)
df$skill2<-as.numeric(df$skill2)
sum(is.na(df$skill2))
df$skill2[is.na((df$skill2))]<-0
sum(is.na(df$skill2))


######### Skill3  ######################



df<-df%>% separate(skill3,c("a3","b3","c3","d3","e3"),sep = " ")
df$e3 <-NULL

df$a3[is.na((df$a3))]<-""
df$b3[is.na((df$b3))]<-""
df$c3[is.na((df$c3))]<-""
df$d3[is.na((df$d3))]<-""
df<-unite(df,skill3,a3,b3,c3,d3,sep = " ")
df$skill3

 

df$skill3<-gsub( " Machine Learning ",3,df$skill3)
df$skill3<-gsub(" Natural Language Processing",3,df$skill3)
df$skill3<-gsub( " Deep Learning ",3,df$skill3)
df$skill3<-gsub( " SQL  ",3,df$skill3)
df$skill3<-gsub( " Amazon Web Services",3,df$skill3)
df$skill3<-gsub("NoSQL",3,df$skill3)########
df$skill3<-gsub(" MS-Office  ",3,df$skill3)




class(df$skill3)
df$skill3<-as.numeric(df$skill3)
sum(is.na(df$skill3))
df$skill3[is.na((df$skill3))]<-0


######### Skill4  ######################



df<-df%>% separate(skill4,c("a4","b4","c4","d4","e4"),sep = " ")
df$e4 <-NULL

df$a4[is.na((df$a3))]<-""
df$b4[is.na((df$b4))]<-""
df$c4[is.na((df$c4))]<-""
df$d4[is.na((df$d4))]<-""
df<-unite(df,skill4,a4,b4,c4,d4,sep = " ")
df$skill4


df$skill4<-gsub( " Machine Learning ",3,df$skill4)
df$skill4<-gsub(" Natural Language Processing",3,df$skill4)
df$skill4<-gsub( " SQL  ",3,df$skill4)
df$skill4<-gsub( " Deep Learning ",3,df$skill4)
df$skill4<-gsub( " Amazon Web Services",3,df$skill4)#####
df$skill4<-gsub("NoSQL",3,df$skill4)########
df$skill4<-gsub(" MS-Excel  ",3,df$skill4)

class(df$skill4)
df$skill4<-as.numeric(df$skill4)
sum(is.na(df$skill4))
df$skill4[is.na((df$skill4))]<-0


######### Skill5  ######################


df<-df%>% separate(skill5,c("a5","b5","c5","d5","e5"),sep = " ")
df$e5 <-NULL

df$a5[is.na((df$a5))]<-""
df$b5[is.na((df$b5))]<-""
df$c5[is.na((df$c5))]<-""
df$d5[is.na((df$d5))]<-""
df<-unite(df,skill5,a5,b5,c5,d5,sep = " ")
df$skill5

df$skill5<-gsub( " Machine Learning ",3,df$skill5)
df$skill5<-gsub(" Natural Language Processing",3,df$skill5)
df$skill5<-gsub( " SQL  ",3,df$skill5)
df$skill5<-gsub( " Deep Learning " ,3,df$skill5)
df$skill5<-gsub(" MS-Excel  ",3,df$skill5)


class(df$skill5)
df$skill5<-as.numeric(df$skill5)
sum(is.na(df$skill5))
df$skill5[is.na((df$skill5))]<-0


######### Skill6  ######################



df<-df%>% separate(skill6,c("a6","b6","c6","d6","e6"),sep = " ")
df$e6 <-NULL

df$a6[is.na((df$a6))]<-""
df$b6[is.na((df$b6))]<-""
df$c6[is.na((df$c6))]<-""
df$d6[is.na((df$d6))]<-""
df<-unite(df,skill6,a6,b6,c6,d6,sep = " ")
df$skill6


df$skill6<-gsub( " Machine Learning ",3,df$skill6)
df$skill6<-gsub(" Natural Language Processing" ,3,df$skill6)
df$skill6<-gsub(  " SQL  ",3,df$skill6)
df$skill6<-gsub( " Deep Learning " ,3,df$skill6)
df$skill6<-gsub( " Amazon Web Services",3,df$skill6)
df$skill6<-gsub(" MS-Excel  ",3,df$skill6)

class(df$skill6)
df$skill6<-as.numeric(df$skill6)
sum(is.na(df$skill6))
df$skill6[is.na((df$skill6))]<-0



######### Skill7  ######################

df<-df%>% separate(skill7,c("a7","b7","c7","d7","e7"),sep = " ")
df$e7 <-NULL

df$a7[is.na((df$a7))]<-""
df$b7[is.na((df$b7))]<-""
df$c7[is.na((df$c7))]<-""
df$d7[is.na((df$d7))]<-""
df<-unite(df,skill7,a7,b7,c7,d7,sep = " ")
df$skill7


df$skill7<-gsub( " Machine Learning " ,3,df$skill7)
df$skill7<-gsub(" Natural Language Processing",3,df$skill7)
df$skill7<-gsub(" SQL  ",3,df$skill7)
df$skill7<-gsub(" Deep Learning " ,3,df$skill7)
df$skill7<-gsub(" MS-Excel  " ,3,df$skill7)


class(df$skill7)
df$skill7<-as.numeric(df$skill7)
sum(is.na(df$skill7))
df$skill7[is.na((df$skill7))]<-0


######### Skill8  ######################



df<-df%>% separate(skill8,c("a8","b8","c8","d8","e8"),sep = " ")
df$e8 <-NULL

df$a8[is.na((df$a8))]<-""
df$b8[is.na((df$b8))]<-""
df$c8[is.na((df$c8))]<-""
df$d8[is.na((df$d8))]<-""
df<-unite(df,skill8,a8,b8,c8,d8,sep = " ")
df$skill8


df$skill8<-gsub( " Machine Learning " ,3,df$skill8)
df$skill8<-gsub(" Natural Language Processing",3,df$skill8)
df$skill8<-gsub(" SQL  ",3,df$skill8)
df$skill8<-gsub(" Deep Learning ",3,df$skill8)
df$skill8<-gsub(" MS-Excel  " ,3,df$skill8)


class(df$skill8)
df$skill8<-as.numeric(df$skill8)
sum(is.na(df$skill8))
df$skill8[is.na((df$skill8))]<-0

######### Skill9  ######################


df<-df%>% separate(skil9,c("a9","b9","c9","d9","e9"),sep = " ")
df$e9 <-NULL

df$a9[is.na((df$a9))]<-""
df$b9[is.na((df$b9))]<-""
df$c9[is.na((df$c9))]<-""
df$d9[is.na((df$d9))]<-""
df<-unite(df,skill9,a9,b9,c9,d9,sep = " ")
df$skill9



df$skill9<-gsub( " Machine Learning " ,3,df$skill9)
df$skill9<-gsub(" Natural Language Processing",3,df$skill9)
df$skill9<-gsub(" SQL  ",3,df$skill9)
df$skill9<-gsub(" Deep Learning ",3,df$skill9)
df$skill9<-gsub(" MS-Excel  " ,3,df$skill9)

class(df$skill9)
df$skill9<-as.numeric(df$skill9)
sum(is.na(df$skill9))
df$skill9[is.na((df$skill9))]<-0


######### Skill10  ######################



df<-df%>% separate(skill10,c("a10","b10","c10","d10","e10"),sep = " ")
df$e10 <-NULL

df$a10[is.na((df$a10))]<-""
df$b10[is.na((df$b10))]<-""
df$c10[is.na((df$c10))]<-""
df$d10[is.na((df$d10))]<-""
df<-unite(df,skill10,a10,b10,c10,d10,sep = " ")
df$skill10



df$skill10<-gsub( " Machine Learning " ,3,df$skill10)
df$skill10<-gsub(" Natural Language Processing",3,df$skill10)
df$skill10<-gsub(" SQL  ",3,df$skill0)
df$skill10<-gsub(" Deep Learning ",3,df$skill10)
df$skill10<-gsub( " Amazon Web Services",3,df$skill10)
df$skill10<-gsub(" MS-Excel  " ,3,df$skill10)



class(df$skill10)
df$skill10<-as.numeric(df$skill10)
sum(is.na(df$skill10))
df$skill10[is.na((df$skill10))]<-0


######### Skill11 ######################


df<-df%>% separate(skill11,c("a11","b11","c11","d11","e11"),sep = " ")
df$e11 <-NULL

df$a11[is.na((df$a11))]<-""
df$b11[is.na((df$b11))]<-""
df$c11[is.na((df$c11))]<-""
df$d11[is.na((df$d11))]<-""
df<-unite(df,skill11,a11,b11,c11,d11,sep = " ")
df$skill11


df$skill11<-gsub( " Machine Learning " ,3,df$skill11)
df$skill11<-gsub(" Natural Language Processing",3,df$skill11)
df$skill11<-gsub(" SQL  ",3,df$skill1)
df$skill11<-gsub(" Deep Learning ",3,df$skill11)
df$skill11<-gsub(" MS-Excel  " ,3,df$skill11)


class(df$skill11)
df$skill11<-as.numeric(df$skill11)
sum(is.na(df$skill11))
df$skill11[is.na((df$skill11))]<-0


######### Skill12 ######################


df<-df%>% separate(skill12,c("a12","b12","c12","d12","e12"),sep = " ")
df$e12 <-NULL

df$a12[is.na((df$a12))]<-""
df$b12[is.na((df$b12))]<-""
df$c12[is.na((df$c12))]<-""
df$d12[is.na((df$d12))]<-""
df<-unite(df,skill12,a12,b12,c12,d12,sep = " ")
df$skill12

df$skill12<-gsub( " Machine Learning " ,3,df$skill12)
df$skill12<-gsub(" Natural Language Processing",3,df$skill12)
df$skill12<-gsub(" SQL  ",3,df$skill2)
df$skill12<-gsub(" Deep Learning ",3,df$skill12)
df$skill12<-gsub(" MS-Excel  " ,3,df$skill12)

class(df$skill12)
df$skill12<-as.numeric(df$skill12)
sum(is.na(df$skill12))
df$skill12[is.na((df$skill12))]<-0


######### Skill13 ######################



df<-df%>% separate(skill13,c("a13","b13","c13","d13","e13"),sep = " ")
df$e13 <-NULL

df$a13[is.na((df$a13))]<-""
df$b13[is.na((df$b13))]<-""
df$c13[is.na((df$c13))]<-""
df$d13[is.na((df$d13))]<-""
df<-unite(df,skill13,a13,b13,c13,d13,sep = " ")
df$skill13

df$skill13<-gsub( " Machine Learning " ,3,df$skill13)
df$skill13<-gsub(" SQL  ",3,df$skill3)
df$skill13<-gsub(" Deep Learning ",3,df$skill13)
df$skill13<-gsub("NoSQL",3,df$skill13)
df$skill13<-gsub(" MS-Excel  " ,3,df$skill13)

class(df$skill13)
df$skill13<-as.numeric(df$skill13)
sum(is.na(df$skill13))
df$skill13[is.na((df$skill13))]<-0


######### Skill14 ######################

df<-df%>% separate(skill14,c("a14","b14","c14","d14","e14"),sep = " ")
df$e14 <-NULL

df$a14[is.na((df$a14))]<-""
df$b14[is.na((df$b14))]<-""
df$c14[is.na((df$c14))]<-""
df$d14[is.na((df$d14))]<-""
df<-unite(df,skill14,a14,b14,c14,d14,sep = " ")
df$skill14

df$skill14<-gsub( " Machine Learning " ,3,df$skill14)
df$skill14<-gsub(" Natural Language Processing",3,df$skill14)
df$skill14<-gsub(" SQL  ",3,df$skill4)

class(df$skill14)
df$skill14<-as.numeric(df$skill14)
sum(is.na(df$skill14))
df$skill14[is.na((df$skill14))]<-0


######### Skill15 ######################

df<-df%>% separate(skill15,c("a15","b15","c15","d15","e15"),sep = " ")
df$e15 <-NULL

df$a15[is.na((df$a15))]<-""
df$b15[is.na((df$b15))]<-""
df$c15[is.na((df$c15))]<-""
df$d15[is.na((df$d15))]<-""
df<-unite(df,skill15,a15,b15,c15,d15,sep = " ")
df$skill15

df$skill15<-gsub(" Natural Language Processing",3,df$skill15)
df$skill15<-gsub(" SQL  ",3,df$skill5)
df$skill15<-gsub(" Deep Learning ",3,df$skill15)


class(df$skill15)
df$skill15<-as.numeric(df$skill15)
sum(is.na(df$skill15))
df$skill15[is.na((df$skill15))]<-


######### Skill16 ######################

df<-df%>% separate(skill16,c("a16","b16","c16","d16","e16"),sep = " ")
df$e16 <-NULL

df$a16[is.na((df$a16))]<-""
df$b16[is.na((df$b16))]<-""
df$c16[is.na((df$c16))]<-""
df$d16[is.na((df$d16))]<-""
df<-unite(df,skill16,a16,b16,c16,d16,sep = " ")
df$skill16

df$skill16<-gsub( " Amazon Web Services",3,df$skill16)
df$skill16<-gsub(" MS-Excel  " ,3,df$skill16)


class(df$skill16)
df$skill16<-as.numeric(df$skill16)
sum(is.na(df$skill16))
df$skill16[is.na((df$skill16))]<-0


##########summup all the skill variables and adding it to the new column


df$skill<- df$skill1 + df$skill2 +df$skill3+
    df$skill4 + df$skill5 +df$skill6 + df$skill7 + 
    df$skill8 + df$skill9 + df$skill10 + df$skill11 +df$skill12
df$skill13 + df$skill14 + df$skill15 + 
  df$skill16


#########filtering based on te given criteria################

head(df)

df<-df%>%
  mutate(weigtage=case_when(Degree=="Bachelor of Technology (B.Tech)"| Current.Year.Of.Graduation==2019 ~8))

df<-df%>%
  mutate(weigtage=case_when(Degree=="Bachelor of Engineering (B.E)"| Current.Year.Of.Graduation==2019 ~8))

df<-df%>%
  mutate(weigtage1=case_when(Degree=="Bachelor of Technology (B.Tech)"| Current.Year.Of.Graduation==2020 ~10))

df<-df%>%
  mutate(weigtage1=case_when(Degree=="Bachelor of Engineering (B.E)"| Current.Year.Of.Graduation==2020 ~10))
  

df<-df%>%
  mutate(weigtage2=case_when(Degree=="Bachelor of Technology (B.Tech)"| Current.Year.Of.Graduation <=2018 ~ 5))
df<-df%>%
  mutate(weigtage2=case_when(Degree=="Bachelor of Engineering (B.E)"| Current.Year.Of.Graduation <=2018 ~ 5))




df<-df%>%
  mutate(weigtage3=case_when(Degree=="Master of Science (M.Sc)"| Current.Year.Of.Graduation <=2020 ~ 7))
df<-df%>%
  mutate(weigtage3=case_when(Degree=="Master of Technology (M.Tech)"| Current.Year.Of.Graduation <=2020 ~ 7))


df<-df%>%
  mutate(weigtage4=case_when(Degree=="Master of Science (M.Sc)"| Current.Year.Of.Graduation <=2019 ~ 3))
df<-df%>%
  mutate(weigtage4=case_when(Degree=="Master of Technology (M.Tech)"| Current.Year.Of.Graduation <=2019~ 3))



df<-df%>%
  mutate(weigtage5=case_when(Python..out.of.3.== 3 ~10))
df<-df%>%
  mutate(weigtage6=case_when(Python..out.of.3.== 2 ~7))
df<-df%>%
  mutate(weigtage7=case_when(Python..out.of.3.== 1 ~3))



df<-df%>%
  mutate(weigtage8=case_when(R.Programming..out.of.3.== 3 ~10))
df<-df%>%
  mutate(weigtage9=case_when(Python..out.of.3.== 2 ~7))
df<-df%>%
  mutate(weigtage10=case_when(Python..out.of.3.== 1 ~3))



df<-df%>%
  mutate(weigtage11=case_when(Data.Science..out.of.3.== 3 ~10))
df<-df%>%
  mutate(weigtage12=case_when(Data.Science..out.of.3.== 2 ~7))
df<-df%>%
  mutate(weigtage13=case_when(Data.Science..out.of.3.== 1 ~3))


sum(is.na(df$weigtage))
df$weigtage[is.na((df$weigtage))]<-0

sum(is.na(df$weigtage1))
df$weigtage1[is.na((df$weigtage1))]<-0

sum(is.na(df$weigtage2))
df$weigtage2[is.na((df$weigtage2))]<-0

sum(is.na(df$weigtage3))
df$weigtage3[is.na((df$weigtage3))]<-0


sum(is.na(df$weigtage4))
df$weigtage4[is.na((df$weigtage4))]<-0


sum(is.na(df$weigtage5))
df$weigtage5[is.na((df$weigtage5))]<-0


sum(is.na(df$weigtage6))
df$weigtage6[is.na((df$weigtage6))]<-0


sum(is.na(df$weigtage7))
df$weigtage7[is.na((df$weigtage7))]<-0


sum(is.na(df$weigtage8))
df$weigtage8[is.na((df$weigtage8))]<-0


sum(is.na(df$weigtage9))
df$weigtage9[is.na((df$weigtage9))]<-0


sum(is.na(df$weigtage10))
df$weigtage10[is.na((df$weigtage10))]<-0


sum(is.na(df$weigtage11))
df$weigtage11[is.na((df$weigtage11))]<-0


sum(is.na(df$weigtage12))
df$weigtage12[is.na((df$weigtage12))]<-0

sum(is.na(df$weigtage13))
df$weigtage13[is.na((df$weigtage13))]<-0


########adding all the weigtage and storing it in score variable


df$score <- df$weigtage + df$weigtage1 + df$weigtage2 +
             df$weigtage3 + df$weigtage4 + df$weigtage5 +
      df$weigtage6 + df$weigtage7 + df$weigtage8 + df$weigtage9 +
  df$weigtage10 + df$weigtage11 + df$weigtage12 + df$weigtage13 


######adding the column score and skill######

df$ApplicantScore<- df$skill + df$score
df$skill<-NULL
df$score<-NULL

df$skill1<-NULL
df$skill2<-NULL
df$skill3<-NULL
df$skill4<-NULL
df$skill5<-NULL
df$skill6<-NULL
df$skill7<-NULL
df$skill8<-NULL
df$skill9<-NULL
df$skill10<-NULL
df$skill11<-NULL
df$skill12<-NULL
df$skill13<-NULL
df$skill14<-NULL
df$skill15<-NULL
df$skill16<-NULL



df$weigtage<-NULL
df$weigtage1<-NULL
df$weigtage2<-NULL
df$weigtage3<-NULL
df$weigtage4<-NULL
df$weigtage5<-NULL
df$weigtage6<-NULL
df$weigtage7<-NULL
df$weigtage8<-NULL
df$weigtage9<-NULL
df$weigtage10<-NULL
df$weigtage11<-NULL
df$weigtage12<-NULL
df$weigtage13<-NULL

###########removing the variables present in the column 2,3,4

df<-df[c(-2,-3,-4)]


######### using if condition#####################


df<-df %>%
  mutate(Result = case_when(ApplicantScore >= 40 ~ "Congratulation !
Your profile has been shortlisted
for Data Scientist.",ApplicantScore < 40 ~ "Sorry !
Your profile did not qualify for
further discussion."))

######removing skill and score variable#######

df$skill<-NULL
df$score<-NULL
df$Factor<-NULL
df$Application_ID<-NULL

######converting into factors##########
         
  
df<-df %>%
  mutate(output = case_when(ApplicantScore >= 40 ~ 1,
                 ApplicantScore <= 40 ~ 0))
class(df$output)
df$output<-as.factor(df$output)




########## splitting the data #################

set.seed(102)
sam <- sample.int(n=nrow(df),0.7*nrow(df))

head(sam)
train<-df[sam,]

test<-df[-sam,]

str(train)


################# model building #######


library(rpart)

library(rpart.plot)

rpart.plot(dt, box.palette="RdRd", shadow.col="grey", nn=TRUE,cex = 0.8)


head(train)

dt<-rpart(Result~.,data = train,method = "class")

p<-predict(dt,train,type = "class")
train$prob<-predict(dt,train,type = "prob")[,2]

sum(diag(c_table))/sum(c_table)

c_table<-table(predicted=p,reference=train$output)



########  test data#####################

pt<-predict(dt,test,type = "class")

test$prob<-predict(dt,test,type = "prob")[,2]
pt_c<-table(predicted=pt,reference=test$output)
pt_c
#accuraccy
sum(diag(pt_c))/sum(pt_c)



