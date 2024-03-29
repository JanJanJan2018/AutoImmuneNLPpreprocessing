# this accompanying file 'AutoimmuneTwitterAI-Disease-Treatments.csv'
# scraped 11-119 twitter posts and comments on treatments
# for auto immune diseases: fibromyalgia, chrons, hashimoto, multiple sclerosis,
# rheumatoid arthritis (RA), leukemia, and also kidney disease and celiac disease

# This script will preprocess and put the diseases into groups by disease,
# then it will create what disease it is categorized, remove the disease name 
# from the twitter comment, and create a target variable to use ML on 
# to classify if the tweet is one of those types of diseases.

# Further NLP work will be done in python 3 to classify the disease and
# return the probability of the tweet being one of those above diseases.

Auto <- read.csv('AutoImmuneTwitterAI-Disease-Treatments.csv', sep=',',
                 header=TRUE, na.strings=c('',' '))

# remove rows that were formatted in excel but that have no data for all fields

auto <- Auto[!is.na(Auto$TwitterLeukemiaTreatment),]#Leukemia has most obs

rm(Auto)

colnames(auto) #remove col 12:16
auto <- auto[,1:11]

#remove twitter and treatment from the colnames

colnames(auto) <- gsub('Twitter','',colnames(auto))
colnames(auto) <- gsub('Treatment', '', colnames(auto))
colnames(auto) <- gsub('_Treatment', '', colnames(auto))
colnames(auto) <- gsub('_','', colnames(auto))

colnames(auto)
# [1] "CeliacDiseaseSymptoms" "KidneyDiseaseSymptoms" "RADiseaseSymptoms"    
# [4] "RAArthritis"           "Celiac"                "KidneyDisease"        
# [7] "Leukemia"              "multipleSclerosis"     "fibromyalgia"         
# [10] "hashimotoSyndrome"     "chrons"  

# some earlier fields were scraped with 'symptom' search but changed later

#combine the symptoms with the disease 

length(na.omit(auto$CeliacDiseaseSymptoms))#11
length(na.omit(auto$KidneyDiseaseSymptoms)) #13
length(na.omit(auto$RADiseaseSymptoms)) #18

celiac <- as.character(na.omit(auto$CeliacDiseaseSymptoms))
Kidney <- as.character(na.omit(auto$KidneyDiseaseSymptoms))
RA <- as.character(na.omit(auto$RADiseaseSymptoms))

celiac2 <- as.character(na.omit(auto$Celiac))
Kidney2 <- as.character(na.omit(auto$KidneyDisease))
RA2 <- as.character(na.omit(auto$RAArthritis))

celiac3 <- c(celiac2,as.character(rep('NA',119-length(celiac2)))) 
Kidney3 <- c(Kidney2,as.character(rep('NA',119-length(Kidney2))))
RA3 <- c(RA2, as.character(rep('NA', 119-length(RA2))))

auto$Celiac <- celiac3
auto$RAArthritis <- RA3
auto$KidneyDisease <- Kidney3

colnames(auto)
# [1] "CeliacDiseaseSymptoms" "KidneyDiseaseSymptoms" "RADiseaseSymptoms"    
# [4] "RAArthritis"           "Celiac"                "KidneyDisease"        
# [7] "Leukemia"              "multipleSclerosis"     "fibromyalgia"         
# [10] "hashimotoSyndrome"     "chrons"  

Auto <- auto[,4:11]

colnames(Auto)
# [1] "RAArthritis"       "Celiac"            "KidneyDisease"    
# [4] "Leukemia"          "multipleSclerosis" "fibromyalgia"     
# [7] "hashimotoSyndrome" "chrons"  
rm(auto);rm(celiac);rm(celiac2);rm(celiac3);rm(Kidney);rm(Kidney2);rm(Kidney3)
rm(RA);rm(RA2);rm(RA3)


write.csv(Auto, 'AutoImmune.csv', row.names=FALSE)


Auto$RAArthritis <- gsub('[aA]rthritis','UNKNOWN',Auto$RAArthritis)
Auto$RAArthritis <- gsub('[rR]heumatoid','UNKNOWN', Auto$RAArthritis)
Auto$RAArthritis <- gsub('[rR]heum','UNKNOWN', Auto$RAArthritis)
Auto$RAArthritis <- gsub('[R][A]','UNKNOWN', Auto$RAArthritis)

Auto$Celiac <- gsub('[cC]eliac','UNKNOWN', Auto$Celiac)
Auto$Celiac <- gsub('[cC][eE][lL][iI][aA][cC]','UNKNOWN', Auto$Celiac)
Auto$Celiac <- gsub('[lL][iI][aA][cC]','UNKNOWN', Auto$Celiac)

Auto$KidneyDisease <- gsub('[kK][iI][dD][nN][eE][yY]','UNKNOWN',Auto$KidneyDisease)

Auto$Leukemia <- gsub('[lL][eE][kK][eE][mM][iI][aA]','UNKNOWN',Auto$Leukemia)

Auto$multipleSclerosis <- gsub('multiple','UNKNOWN', Auto$multipleSclerosis)
Auto$multipleSclerosis <- gsub('sclerosis','UNKNOWN', Auto$multipleSclerosis)
Auto$multipleSclerosis <- gsub('MULTIPLE','UNKNOWN', Auto$multipleSclerosis)
Auto$multipleSclerosis <- gsub('SCLEROSIS','UNKNOWN', Auto$multipleSclerosis)
Auto$multipleSclerosis <- gsub('MS','UNKNOWN', Auto$multipleSclerosis)

Auto$fibromyalgia <- gsub('[fF][iI][bB][rR][oO][mM][yY][aA][lL][gG][iI][aA]',
                          'UNKNOWN', Auto$fibromyalgia)

Auto$hashimotoSyndrome <- gsub('hashimoto', 'UNKNOWN', Auto$hashimotoSyndrome)
Auto$hashimotoSyndrome <- gsub('HASHIMOTO', 'UNKNOWN', Auto$hashimotoSyndrome)
Auto$hashimotoSyndrome <- gsub('SYNDROM', 'UNKNOWN', Auto$hashimotoSyndrome)
Auto$hashimotoSyndrome <- gsub('syndrom', 'UNKNOWN', Auto$hashimotoSyndrome)
Auto$hashimotoSyndrome <- gsub('hashi', 'UNKNOWN', Auto$hashimotoSyndrome)
Auto$hashimotoSyndrome <- gsub('Hashi', 'UNKNOWN', Auto$hashimotoSyndrome)
Auto$hashimotoSyndrome <- gsub('moto', 'UNKNOWN', Auto$hashimotoSyndrome)

Auto$chrons <- gsub('chron','UNKNOWN', Auto$chrons)
Auto$chrons <- gsub('CHRON', 'UNKNOWN', Auto$chrons)

write.csv(Auto, 'AutoImmuneUNKNOWN.csv', row.names=FALSE)

# [1] "RAArthritis"       "Celiac"            "KidneyDisease"     "Leukemia"         
# [5] "multipleSclerosis" "fibromyalgia"      "hashimotoSyndrome" "chrons"  

RA <- Auto$RAArthritis
Celiac <- Auto$Celiac
Kidney <- Auto$KidneyDisease
Leukemia <- Auto$Leukemia
MS <- Auto$multipleSclerosis
fibro <- Auto$fibromyalgia
hashi <- Auto$hashimotoSyndrome
chron <- Auto$chrons

RA1 <- as.data.frame(na.omit(RA))#28
Celiac1 <- as.data.frame(na.omit(Celiac))#50
Kidney1 <- as.data.frame(na.omit(Kidney))#43
Leukemia1 <- as.data.frame(na.omit(Leukemia))#119
MS1 <- as.data.frame(na.omit(MS))#119
fibro1 <- as.data.frame(na.omit(fibro))#99
hashi1 <- as.data.frame(na.omit(hashi))#30
chron1 <- as.data.frame(na.omit(chron))#19

colnames(RA1) <- 'Tweet'
colnames(Celiac1) <- 'Tweet'
colnames(Kidney1) <- 'Tweet'
colnames(Leukemia1) <- 'Tweet'
colnames(MS1) <- 'Tweet'
colnames(fibro1) <- 'Tweet'
colnames(hashi1) <- 'Tweet'
colnames(chron1) <- 'Tweet'

tweets <- rbind(RA1,Celiac1,Kidney1,Leukemia1,MS1,fibro1,hashi1,chron1)

ra <- as.data.frame(rep('Rheumatoid Arthritis',28))
celiac <- as.data.frame(rep('Celiac Disease', 50))
kidney <- as.data.frame(rep('Kidney Disease', 43))
leukemia <- as.data.frame(rep('Leukemia', 119))
ms <- as.data.frame(rep('Multiple Sclerosis', 119))
fibro <- as.data.frame(rep('Fibromyalgia', 99))
hashi <- as.data.frame(rep('Hashimoto Disease', 30))
chron <- as.data.frame(rep('Chron\'s Disease', 19))

colnames(ra) <- 'Type'
colnames(celiac) <- 'Type'
colnames(kidney) <- 'Type'
colnames(leukemia) <- 'Type'
colnames(ms) <- 'Type'
colnames(fibro) <- 'Type'
colnames(hashi) <- 'Type'
colnames(chron) <- 'Type'

Type <- rbind(ra,celiac,kidney,leukemia,ms,fibro,hashi,chron)

Targeted <- cbind(tweets,Type)

write.csv(Targeted, 'TargetReady.csv', row.names=FALSE)

Target <- read.csv('TargetReady.csv', sep=',', header=TRUE)

Target$Tweet <- gsub('[fF]uck','CURSE WORD', Target$Tweet)
Target$Tweet <- gsub('[sS]hit', 'CURSE WORD', Target$Tweet)

write.csv(Target, 'TargetReady_noCuss.csv', row.names=FALSE)

##################################################################################

Target <- read.csv('TargetReady_noCuss.csv',sep=',', header=TRUE)

types <- unique(Target$Type)

RA <- Target[grep(types[1], Target$Type),]
Celiac <- Target[grep(types[2], Target$Type),]
Kidney <- Target[grep(types[3], Target$Type),]
Leukemia <- Target[grep(types[4], Target$Type),]
MS <- Target[grep(types[5], Target$Type),]
Fibromyalgia <- Target[grep(types[6], Target$Type),]
Hashimoto <- Target[grep(types[7], Target$Type),]
Chron <- Target[grep(types[8], Target$Type),]

dir.create('./RA')
dir.create('./Celiac')
dir.create('./Kidney')
dir.create('./Leukemia')
dir.create('./MS')
dir.create('./Fibromyalgia')
dir.create('./Hashimoto')
dir.create('./Chron')

write.csv(RA, 'RA.csv', row.names=FALSE)
write.csv(Celiac, 'Celiac.csv', row.names=FALSE)
write.csv(Kidney, 'Kidney.csv', row.names=FALSE)
write.csv(Hashimoto, 'Hashimoto.csv', row.names=FALSE)
write.csv(Fibromyalgia, 'Fibromyalgia.csv', row.names=FALSE)
write.csv(MS, 'MS.csv', row.names=FALSE)
write.csv(Leukemia, 'Leukemia.csv', row.names=FALSE)
write.csv(Chron, 'Chron.csv', row.names=FALSE)


ra <- as.character(RA$Tweet)
setwd('./RA')
for (j in 1:length(ra)){
  write(ra[j], paste(paste('RA',j, sep='.'), '.txt', sep=''))
}
setwd('../')


ms <- as.character(MS$Tweet)
setwd('./MS')
for (j in 1:length(ms)){
  write(ms[j], paste(paste('MS',j, sep='.'), '.txt', sep=''))
}
setwd('../')


lk <- as.character(Leukemia$Tweet)
setwd('./Leukemia')
for (j in 1:length(lk)){
  write(lk[j], paste(paste('Lk',j, sep='.'), '.txt', sep=''))
}
setwd('../')


Kd <- as.character(Kidney$Tweet)
setwd('./Kidney')
for (j in 1:length(Kd)){
  write(Kd[j], paste(paste('Kd',j, sep='.'), '.txt', sep=''))
}
setwd('../')


Hs <- as.character(Hashimoto$Tweet)
setwd('./Hashimoto')
for (j in 1:length(Hs)){
  write(Hs[j], paste(paste('Hs',j, sep='.'), '.txt', sep=''))
}
setwd('../')


Fs <- as.character(Fibromyalgia$Tweet)
setwd('./Fibromyalgia')
for (j in 1:length(Fs)){
  write(Fs[j], paste(paste('Fs',j, sep='.'), '.txt', sep=''))
}
setwd('../')

Cr <- as.character(Chron$Tweet)
setwd('./Chron')
for (j in 1:length(Cr)){
  write(Cr[j], paste(paste('Cr',j, sep='.'), '.txt', sep=''))
}
setwd('../')


Ce <- as.character(Celiac$Tweet)
setwd('./Celiac')
for (j in 1:length(Ce)){
  write(Ce[j], paste(paste('Ce',j, sep='.'), '.txt', sep=''))
}
setwd('../')


###############################################################################

#NLP and preprocessing 

library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)

celiac <- Corpus(DirSource("Celiac"))


celiac

celiac <- tm_map(celiac, removePunctuation)
celiac <- tm_map(celiac, removeNumbers)
celiac <- tm_map(celiac, tolower)
celiac <- tm_map(celiac, removeWords, stopwords("english"))
celiac <- tm_map(celiac, stripWhitespace)
celiac <- tm_map(celiac, stemDocument)

dtmCeliac <- DocumentTermMatrix(celiac)
dtmCeliac
# <<DocumentTermMatrix (documents: 50, terms: 22)>>
#   Non-/sparse entries: 1100/0
# Sparsity           : 0%
# Maximal term length: 16
# Weighting          : term frequency (tf)
# 
freq <- colSums(as.matrix(dtmCeliac))

# Replace the UNKNOWN with the disease name dn

dn <- 'celiac'
names(freq)[21] <- dn
names(freq)

FREQ <- data.frame(freq)
ord <- order(freq, decreasing=TRUE)

freq[head(ord, 25)]

findAssocs(dtmCeliac, dn, corlimit=0.01)

findAssocs(dtmCeliac, "enzym", corlimit=0.01)


findAssocs(dtmCeliac, "discov", corlimit=0.01)

wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq>2), aes(word, freq))
p <- p + geom_bar(stat= 'identity') 
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 
p


wordcloud(names(freq), freq, min.freq=10,colors=brewer.pal(3,'Dark2'))

wordcloud(names(freq), freq, max.words=40,colors=brewer.pal(6,'Dark2'))

############################################################################

Celiac <- Corpus(DirSource("Celiac"))
Celiac <- tm_map(Celiac, removePunctuation)
Celiac <- tm_map(Celiac, removeNumbers)
Celiac <- tm_map(Celiac, tolower)



Chron <- Corpus(DirSource("Chron"))
Chron <- tm_map(Chron, removePunctuation)
Chron <- tm_map(Chron, removeNumbers)
Chron <- tm_map(Chron, tolower)




Fibromyalgia <- Corpus(DirSource("Fibromyalgia"))
Fibromyalgia <- tm_map(Fibromyalgia, removePunctuation)
Fibromyalgia <- tm_map(Fibromyalgia, removeNumbers)
Fibromyalgia <- tm_map(Fibromyalgia, tolower)



Hashimoto <- Corpus(DirSource("Hashimoto"))
Hashimoto <- tm_map(Hashimoto, removePunctuation)
Hashimoto <- tm_map(Hashimoto, removeNumbers)
Hashimoto <- tm_map(Hashimoto, tolower)



Kidney <- Corpus(DirSource("Kidney"))
Kidney <- tm_map(Kidney, removePunctuation)
Kidney <- tm_map(Kidney, removeNumbers)
Kidney <- tm_map(Kidney, tolower)



Leukemia <- Corpus(DirSource("Leukemia"))
Leukemia <- tm_map(Leukemia, removePunctuation)
Leukemia <- tm_map(Leukemia, removeNumbers)
Leukemia <- tm_map(Leukemia, tolower)


RA <- Corpus(DirSource("RA"))
RA <- tm_map(RA, removePunctuation)
RA <- tm_map(RA, removeNumbers)
RA <- tm_map(RA, tolower)



MS <- Corpus(DirSource("MS"))
MS <- tm_map(MS, removePunctuation)
MS <- tm_map(MS, removeNumbers)
MS <- tm_map(MS, tolower)

#Celiac Disease
m <- strsplit(Celiac$content, '^ce*txt$')
M <- as.data.frame(m)
M_t <- t(M) #50X1
head(M_t)
row.names(M_t) <- NULL
colnames(M_t) <- 'Tweet'

type <- as.data.frame(rep("Celiac_Disease",length(M_t)))
colnames(type) <- 'Type'

CD <- cbind(M_t, type)

#Hashimoto Disease
m <- strsplit(Hashimoto$content, '^ce*txt$')
M <- as.data.frame(m)
M_t <- t(M) 
head(M_t)
row.names(M_t) <- NULL
colnames(M_t) <- 'Tweet'

type <- as.data.frame(rep("Hashimoto_Disease",length(M_t)))
colnames(type) <- 'Type'

HD <- cbind(M_t, type)

#Leukemia
m <- strsplit(Leukemia$content, '^ce*txt$')
M <- as.data.frame(m)
M_t <- t(M) 
head(M_t)
row.names(M_t) <- NULL
colnames(M_t) <- 'Tweet'

type <- as.data.frame(rep("Leukemia_Disease",length(M_t)))
colnames(type) <- 'Type'

LD <- cbind(M_t, type)

#RA
m <- strsplit(RA$content, '^ce*txt$')
M <- as.data.frame(m)
M_t <- t(M) 
head(M_t)
row.names(M_t) <- NULL
colnames(M_t) <- 'Tweet'

type <- as.data.frame(rep("RA_Disease",length(M_t)))
colnames(type) <- 'Type'

RA <- cbind(M_t, type)

#MS
m <- strsplit(MS$content, '^ce*txt$')
M <- as.data.frame(m)
M_t <- t(M) 
head(M_t)
row.names(M_t) <- NULL
colnames(M_t) <- 'Tweet'

type <- as.data.frame(rep("MS_Disease",length(M_t)))
colnames(type) <- 'Type'

MS <- cbind(M_t, type)

#Chron
m <- strsplit(Chron$content, '^ce*txt$')
M <- as.data.frame(m)
M_t <- t(M) 
head(M_t)
row.names(M_t) <- NULL
colnames(M_t) <- 'Tweet'

type <- as.data.frame(rep("Chron_Disease",length(M_t)))
colnames(type) <- 'Type'

Chron <- cbind(M_t, type)

#Kidney
m <- strsplit(Kidney$content, '^ce*txt$')
M <- as.data.frame(m)
M_t <- t(M) 
head(M_t)
row.names(M_t) <- NULL
colnames(M_t) <- 'Tweet'

type <- as.data.frame(rep("Kidney_Disease",length(M_t)))
colnames(type) <- 'Type'

Kd <- cbind(M_t, type)

#Fibromyalgia
m <- strsplit(Fibromyalgia$content, '^ce*txt$')
M <- as.data.frame(m)
M_t <- t(M) 
head(M_t)
row.names(M_t) <- NULL
colnames(M_t) <- 'Tweet'

type <- as.data.frame(rep("Fibromyalgia",length(M_t)))
colnames(type) <- 'Type'

FD <- cbind(M_t, type)

pythonReady <- rbind(CD,HD,Chron,FD,RA,MS,Kd,LD)

write.csv(pythonReady, 'pythonAutoImmuneNLP-ready.csv', row.names=FALSE)

library(textstem)

python <- lemmatize_strings(pythonReady$Tweet, dictionary=lexicon::hash_lemmas)

pyLemma <- as.data.frame(python)
LemmaPy <- cbind(pyLemma, pythonReady)

colnames(LemmaPy) <- c('LemmatizedTweets','StemmedTweets','AutoImmuneDisorder')

# write to file the comparison of the Lemmatization versus the Stemming 
# of words. The stopwords and whitespace was not removed, but the punctuation,
# numbers, and case changed to all lower case was processed on these tweets

write.csv(LemmaPy, 'LemmaPythonRead.csv', row.names=FALSE)