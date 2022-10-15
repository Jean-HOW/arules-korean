##### Load Libraries #####
library(arules)
library(sqldf)

##### Pull Data #####
BASE <- read.csv("C:/Users/jjhjiaje/Downloads/cs_korean.csv")
head(BASE)
str(BASE)

sqldf("SELECT RACE, COUNT(account_no) FROM BASE GROUP BY RACE")
sqldf("SELECT RACE, cs_kor_ala, count(account_no) from BASE 
        where RACE <> 'OTHERS'
        group by RACE, cs_kor_ala
        order by RACE, cs_kor_ala")

##### Data Preprocessing #####
BASE$L3M_AG_AVG_DAYS <- ifelse(BASE$L3M_AG_AVG_CONNECTED_DAYS > 0, '1','0')


BASE$CUST_TENURE <- ifelse(BASE$tenure_bins == '(20.0, 26.0]', '20 - 26 YEARS', 
                    ifelse(BASE$tenure_bins == '(17.0, 22.0]', '17 - 20 YEARS',
                    ifelse(BASE$tenure_bins == '(4.0, 6.0]', '4 - 6 YEARS', 
                    ifelse(BASE$tenure_bins == '(15.0, 17.0]', '15 - 17 YEARS', 
                    ifelse(BASE$tenure_bins == '(13.0, 15.0]', '13 - 15 YEARS', 
                    ifelse(BASE$tenure_bins == '(11.0, 13.0]', '11 - 13 YEARS', 
                    ifelse(BASE$tenure_bins == '(2.0, 4.0]', '2 - 4 YEARS',
                    ifelse(BASE$tenure_bins == '(8.0, 11.0]', '8 - 11 YEARS', 
                    ifelse(BASE$tenure_bins == '(6.0, 8.0]', '6 - 8 YEARS',
                    ifelse(BASE$tenure_bins == '(-0.001, 2.0]', 'LESS THAN 2 YEARS', 'N/A'))))))))))


BASE$ARPU <- ifelse(BASE$ARPU_bins == '(113.32, 136.4]', 'RM113.32 - RM136.40', 
             ifelse(BASE$ARPU_bins == '(136.4, 164.42]', 'RM136.40 - RM164.42',
             ifelse(BASE$ARPU_bins == '(164.42, 190.0]', 'RM164.42 - RM190.00', 
             ifelse(BASE$ARPU_bins == '(68.27, 93.0]', 'RM68.27 - RM93.00', 
             ifelse(BASE$ARPU_bins == '(93.0, 113.32]', 'RM93.00 - RM113.32', 
             ifelse(BASE$ARPU_bins == '(190.0, 220.0]', '> RM190', 
             ifelse(BASE$ARPU_bins == '(220.0, 256.0]', '> RM190', 
             ifelse(BASE$ARPU_bins == '(256.0, 318.0]', '> RM190',
             ifelse(BASE$ARPU_bins == '(318.0, 590.0]', '> RM190',
             ifelse(BASE$ARPU_bins == '(-0.001, 68.27]', '< RM68.27', 'N/A'))))))))))

BASE$SUM_SCORES_NEW <- as.character(BASE$SUM_SCORES)
BASE$DISNEY_ACTIVATED_FLAG_NEW <- as.character(BASE$DISNEY_ACTIVATED_FLAG)
BASE$MOV_NEW <- as.character(BASE$MOV)
BASE$SPT_NEW <- as.character(BASE$SPT)
BASE$VER_PACK_TIER_NEW <- as.character(BASE$ver_pack_tier)
BASE$ENG_PACK_TIER_NEW <- as.character(BASE$eng_pack_tier)
BASE$EDU_PACK_TIER_NEW <- as.character(BASE$edu_pack_tier)

str(BASE)

##### SELECT FINAL COLUMNS #####
BASE_FINAL <- BASE[c('RACE','CUST_TENURE','CRM_SET_TOP_BOX_TYPE','L3M_AG_AVG_DAYS','ARPU',
                     'SUM_SCORES_NEW','DISNEY_ACTIVATED_FLAG_NEW','MOV_NEW','SPT_NEW',
                     'VER_PACK_TIER_NEW','ENG_PACK_TIER_NEW', 'EDU_PACK_TIER_NEW', 'cs_kor_ala')]
BASE_FINAL$RACE <- as.factor(BASE_FINAL$RACE)
BASE_FINAL$CUST_TENURE <- as.factor(BASE_FINAL$CUST_TENURE)
BASE_FINAL$CRM_SET_TOP_BOX_TYPE <- as.factor(BASE_FINAL$CRM_SET_TOP_BOX_TYPE)
BASE_FINAL$L3M_AG_AVG_DAYS <- as.factor(BASE_FINAL$L3M_AG_AVG_DAYS)
BASE_FINAL$ARPU <- as.factor(BASE_FINAL$ARPU)
BASE_FINAL$SUM_SCORES_NEW <- as.factor(BASE_FINAL$SUM_SCORES_NEW)
BASE_FINAL$DISNEY_ACTIVATED_FLAG_NEW <- as.factor(BASE_FINAL$DISNEY_ACTIVATED_FLAG_NEW)
BASE_FINAL$MOV_NEW <- as.factor(BASE_FINAL$MOV_NEW)
BASE_FINAL$SPT_NEW <- as.factor(BASE_FINAL$SPT_NEW)
BASE_FINAL$VER_PACK_TIER_NEW <- as.factor(BASE_FINAL$VER_PACK_TIER_NEW)
BASE_FINAL$ENG_PACK_TIER_NEW <- as.factor(BASE_FINAL$ENG_PACK_TIER_NEW)
BASE_FINAL$EDU_PACK_TIER_NEW <- as.factor(BASE_FINAL$EDU_PACK_TIER_NEW)
BASE_FINAL$cs_kor_ala <- as.factor(BASE_FINAL$cs_kor_ala)

str(BASE_FINAL)

##### RUNNING ASSOCIATION RULES #####
unique(BASE_FINAL[c("CRM_SET_TOP_BOX_TYPE")])

########################################################################################################
### RACE = MALAY ###
BASE_MALAY <- sqldf("SELECT CUST_TENURE,
                            CRM_SET_TOP_BOX_TYPE,
                            L3M_AG_AVG_DAYS,
                            ARPU,
                            SUM_SCORES_NEW,
                            DISNEY_ACTIVATED_FLAG_NEW,
                            MOV_NEW,
                            SPT_NEW,
                            VER_PACK_TIER_NEW,
                            ENG_PACK_TIER_NEW, 
                            EDU_PACK_TIER_NEW,
                            cs_kor_ala
                     FROM BASE_FINAL 
                     WHERE RACE = 'MALAY'")

# sqldf("SELECT pack_group, COUNT(account_no) FROM BASE WHERE RACE = 'MALAY' GROUP BY pack_group")

BASE_MALAY_KOREAN = BASE_MALAY[,c(1:12)]

# BASE_MALAY_PACKS = BASE_MALAY[,c(1:13)]
# BASE_MALAY_HORROR = BASE_MALAY[,c(1:3,5:12,14)]
# BASE_MALAY_NEWS = BASE_MALAY[,c(1:3,5:12,15)]
# BASE_MALAY_KIDS = BASE_MALAY[,c(1:3,5:12,16)]

# support=0.03, confidence=0.3
### 1. KOREAN ADD ON ###
MALAY_KOREAN <- apriori(BASE_MALAY_KOREAN,parameter = list(support=0.05, confidence=0.05),
                        appearance = list(default="lhs",rhs = "cs_kor_ala=ADD_KOREAN"))
MALAY_KOREAN_RANKED<-sort(MALAY_KOREAN, by="lift", decreasing=TRUE)
MALAY_KOREAN_RANKED <- MALAY_KOREAN_RANKED[!is.redundant(MALAY_KOREAN_RANKED)]
inspect(MALAY_KOREAN_RANKED)
MALAY_KOREAN_RANKED_DF <- as(MALAY_KOREAN_RANKED, 'data.frame')
head(MALAY_KOREAN_RANKED_DF)
write.csv(MALAY_KOREAN_RANKED_DF,'MALAY_KOREAN_RANKED_DF.csv')

#############################################################################################################################
### RACE = CHINESE ###
BASE_CHINESE <- sqldf("SELECT CUST_TENURE,
                            CRM_SET_TOP_BOX_TYPE,
                            L3M_AG_AVG_DAYS,
                            ARPU,
                            SUM_SCORES_NEW,
                            DISNEY_ACTIVATED_FLAG_NEW,
                            MOV_NEW,
                            SPT_NEW,
                            VER_PACK_TIER_NEW,
                            ENG_PACK_TIER_NEW, 
                            EDU_PACK_TIER_NEW,
                            cs_kor_ala
                     FROM BASE_FINAL 
                     WHERE RACE = 'CHINESE'")

# sqldf("SELECT pack_group, COUNT(account_no) FROM BASE WHERE RACE = 'CHINESE' GROUP BY pack_group")

BASE_CHINESE_KOREAN = BASE_CHINESE[,c(1:12)]

# BASE_CHINESE_PACKS = BASE_CHINESE[,c(1:13)]
# BASE_CHINESE_KOREAN = BASE_CHINESE[,c(1:3,5:11,12)]
# BASE_CHINESE_NEWS = BASE_CHINESE[,c(1:3,5:12,15)]
# BASE_CHINESE_KIDS = BASE_CHINESE[,c(1:3,5:12,16)]


### 1. KOREAN ADD ON ###
CHINESE_KOREAN <- apriori(BASE_CHINESE_KOREAN,parameter = list(support=0.05, confidence=0.05),
                        appearance = list(default="lhs",rhs = "cs_kor_ala=ADD_KOREAN"))
CHINESE_KOREAN_RANKED<-sort(CHINESE_KOREAN, by="lift", decreasing=TRUE)
CHINESE_KOREAN_RANKED <- CHINESE_KOREAN_RANKED[!is.redundant(CHINESE_KOREAN_RANKED)]
inspect(CHINESE_KOREAN_RANKED)
CHINESE_KOREAN_RANKED_DF <- as(CHINESE_KOREAN_RANKED, 'data.frame')
head(CHINESE_KOREAN_RANKED_DF)
write.csv(CHINESE_KOREAN_RANKED_DF,'CHINESE_KOREAN_RANKED_DF.csv')


########################################################################################################
### RACE = INDIAN ###
BASE_INDIAN <- sqldf("SELECT CUST_TENURE,
                            CRM_SET_TOP_BOX_TYPE,
                            L3M_AG_AVG_DAYS,
                            ARPU,
                            SUM_SCORES_NEW,
                            DISNEY_ACTIVATED_FLAG_NEW,
                            MOV_NEW,
                            SPT_NEW,
                            VER_PACK_TIER_NEW,
                            ENG_PACK_TIER_NEW, 
                            EDU_PACK_TIER_NEW,
                            cs_kor_ala
                     FROM BASE_FINAL 
                     WHERE RACE = 'INDIAN'")

# sqldf("SELECT pack_group, COUNT(account_no) FROM BASE WHERE RACE = 'INDIAN' GROUP BY pack_group")

BASE_INDIAN_KOREAN = BASE_INDIAN[,c(1:3,5:11,12)]

# BASE_INDIAN_PACKS = BASE_INDIAN[,c(1:13)]
# BASE_INDIAN_HORROR = BASE_INDIAN[,c(1:3,5:12,14)]
# BASE_INDIAN_NEWS = BASE_INDIAN[,c(1:3,5:12,15)]
# BASE_INDIAN_KIDS = BASE_INDIAN[,c(1:3,5:12,16)]

### 1. KOREAN ADD ON ###
INDIAN_KOREAN <- apriori(BASE_INDIAN_KOREAN,parameter = list(support=0.01, confidence=0.01),
                          appearance = list(default="lhs",rhs = "cs_kor_ala=ADD_KOREAN"))
INDIAN_KOREAN_RANKED<-sort(INDIAN_KOREAN, by="lift", decreasing=TRUE)
INDIAN_KOREAN_RANKED <- INDIAN_KOREAN_RANKED[!is.redundant(INDIAN_KOREAN_RANKED)]
inspect(INDIAN_KOREAN_RANKED)
INDIAN_KOREAN_RANKED_DF <- as(INDIAN_KOREAN_RANKED, 'data.frame')
head(INDIAN_KOREAN_RANKED_DF)
write.csv(INDIAN_KOREAN_RANKED_DF,'INDIAN_KOREAN_RANKED_DF.csv')

########################################################################################################
### RACE = OTHERS ###
BASE_OTHERS <- sqldf("SELECT CUST_TENURE,
                            CRM_SET_TOP_BOX_TYPE,
                            L3M_AG_AVG_DAYS,
                            ARPU,
                            SUM_SCORES_NEW,
                            DISNEY_ACTIVATED_FLAG_NEW,
                            MOV_NEW,
                            SPT_NEW,
                            VER_PACK_TIER_NEW,
                            ENG_PACK_TIER_NEW, 
                            EDU_PACK_TIER_NEW,
                            cs_kor_ala
                     FROM BASE_FINAL 
                     WHERE RACE = 'OTHERS'")

# sqldf("SELECT pack_group, COUNT(account_no) FROM BASE WHERE RACE = 'OTHERS' GROUP BY pack_group")

BASE_OTHERS_KOREAN = BASE_OTHERS[,c(1:3,5:11,12)]

# BASE_OTHERS_PACKS = BASE_OTHERS[,c(1:13)]
# BASE_OTHERS_HORROR = BASE_OTHERS[,c(1:3,5:12,14)]
# BASE_OTHERS_NEWS = BASE_OTHERS[,c(1:3,5:12,15)]
# BASE_OTHERS_KIDS = BASE_OTHERS[,c(1:3,5:12,16)]

### 1. KOREAN ADD ON ###
OTHERS_KOREAN <- apriori(BASE_OTHERS_KOREAN,parameter = list(support=0.05, confidence=0.05),
                         appearance = list(default="lhs",rhs = "cs_kor_ala=ADD_KOREAN"))
OTHERS_KOREAN_RANKED<-sort(OTHERS_KOREAN, by="lift", decreasing=TRUE)
OTHERS_KOREAN_RANKED <- OTHERS_KOREAN_RANKED[!is.redundant(OTHERS_KOREAN_RANKED)]
inspect(OTHERS_KOREAN_RANKED)
OTHERS_KOREAN_RANKED_DF <- as(OTHERS_KOREAN_RANKED, 'data.frame')
head(OTHERS_KOREAN_RANKED_DF)
write.csv(OTHERS_KOREAN_RANKED_DF,'OTHERS_KOREAN_RANKED_DF.csv')
