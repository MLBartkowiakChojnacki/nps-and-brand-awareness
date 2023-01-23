library(dplyr)
library(tidyverse)

grzyby <- read.csv2('grupa.1.csv', fileEncoding = 'UTF-8-BOM')
RegionC <- read.csv2('RegionC.csv', fileEncoding = 'UTF-8-BOM')
Regional.Structure <- read.csv2('Regional.Structure.csv', fileEncoding = 'UTF-8-BOM')

#------------------------------------NPS----------------------------------------
NPS <- grzyby %>% 
  select(RecordNo, RegionA, X2a, X2b, starts_with("X11_")) %>% 
  mutate(RX2a = case_when(X2a<=25~"5-25 Ha",
                         50>=X2a~"25-50 Ha",
                         100>=X2a ~ "50-100 Ha",
                         250>=X2a~"100-250 Ha",
                         X2a>250~"Więcej niż 250 Ha")) %>%
  mutate(RX2b = case_when(X2b<=25~"5-25 Ha",
                          50>=X2b~"25-50 Ha",
                          100>=X2b ~ "50-100 Ha",
                          250>=X2b~"100-250 Ha",
                          X2b>250~"Więcej niż 250 Ha")) %>%
  left_join(Regional.Structure) %>% 
  left_join(RegionC) %>% 
  rename(Borowik = X11_1,
         Czubajka = X11_2,
         Gąska = X11_3,
         Podgrzybek = X11_4,
         Koźlarz = X11_5,
         Maślak = X11_6,
         Opieńka = X11_7,
         Gołąbek = X11_8,
         Boczniak = X11_9,
         Pieczarka = X11_10) %>% 
  pivot_longer(cols = c(5:11), names_to = "Company", values_to = "Recommendation") %>%
  filter(Recommendation != 99) %>% 
  mutate(Score = case_when(Recommendation<=6~"Detractors",
                           Recommendation<=8~"Neutrals",
                           Recommendation<=10~"Promoters")) %>% 
  select(RecordNo, Województwo, RX2a, RX2b, Company, Recommendation, Score) %>% 
  mutate(detrac = if_else(between(Recommendation, 0, 6), 1, 0),
         neutral = if_else(between(Recommendation, 7, 8), 1, 0),
         promo = if_else(between(Recommendation, 9, 10), 1, 0),
         total = 1)

write.csv(NPS, "NPS.csv", row.names=FALSE)

#-----------------------------BRAND AWARENESS-----------------------------------

count_awareness <- function(vector, col_name){
  df <- subset(grzyby, select = c("RecordNo", vector))
  df <- gather(df, key = "Index", value = {{col_name}}, vector, factor_key = TRUE) %>% 
    mutate(Index=as.numeric(Index))
  
  return(df)
}

add_col <- function(df, column){
  df <- left_join(df, column, by = c("RecordNo", "Index"))
  
  return(df)
}


MyFrame = subset(grzyby, select = c("RecordNo","X12","X13","X14"))
newCols <- paste("Index",1:20,sep="")
MyFrame[,newCols] <- NA


### gather / pivot longer
MyFrame <- gather(MyFrame, key = "Index", value = "Dummy", Index1:Index20, factor_key = TRUE)
MyFrame <- mutate(MyFrame, Index=as.numeric(Index))


### TOP OF MIND
TopofMind <- c("X3M1")
df1 = subset(grzyby, select = c("RecordNo",TopofMind))
df1$Index = 1
df1 <- rename(df1, TopofMind = X3M1)
MyFrame <- add_col(MyFrame, df1)


### UNAIDED
unaided <- c("X3M1","X3M2","X3M3","X3M4","X3M5","X3M6","X3M7","X3M8","X3M9","X3M10")
unaided_df <- count_awareness(unaided, "Unaided")
MyFrame <- add_col(MyFrame, unaided_df)


### AIDED
aided <- c("X4M1","X4M2","X4M3","X4M4","X4M5","X4M6","X4M7","X4M8","X4M9","X4M10")
aided_df <- count_awareness(aided, "Aided")
MyFrame <- add_col(MyFrame, aided_df)


### TOTAL AWARENESS
total_awareness <- count_awareness(vector = c(unaided, aided), "Total_awareness")
MyFrame <- add_col(MyFrame, total_awareness)


write.csv(MyFrame, "brand_awareness.csv", row.names=FALSE)



