# Loading Packages
#library(dplyr)
pacman::p_load(tidyverse, haven, readxl,dplyr,arsenal,gplots, xlsx, rJava, xlsxjars)

#this is the exam of xxxx

# Einlesen der excel-Daten aus Qualtrics
results <- read_excel("HS2019-1 Emot Ergebnisse.xlsx", sheet = "Antworten")%>%
  # Mit num_range("K",1:10) werden alle MC Fragen selektiert von 1 bis 10. 
  #Die Anzahl 1:10 kann beleibig angepasst werden an Anzahl Fragen in Pruefung
  # Mit num_range("K",1:10) werden alle KPrim Fragen selektiert von 1 bis 10. 
  #Die Anzahl 1:10 kann beleibig angepasst werden an Anzahl Fragen in Pruefung
  select(Matrikel,StudisID,Nachname,Vorname,starts_with("A_"),starts_with("K_"))

# Einlesen Anwortexcel
antworten <- read_excel("HS2019-1 Emot Ergebnisse.xlsx", sheet = "Loesung")%>%
  select(starts_with("A_"),starts_with("K_"))

results <- results  
results[is.na(results)] <- 0
results <- results  %>%
  mutate(scoreA_1 =if_else(A_1==antworten$A_1,1,0),
         scoreA_2 =if_else(A_2==antworten$A_2,1,0),
         scoreA_3 =if_else(A_3==antworten$A_3,1,0),
         scoreA_4 =if_else(A_4==antworten$A_4,1,0),
         scoreA_5 =if_else(A_5==antworten$A_5,1,0),
         scoreA_6 =if_else(A_6==antworten$A_6,1,0),
         scoreA_7 =if_else(A_7==antworten$A_7,1,0),
         scoreA_8 =if_else(A_8==antworten$A_8,1,0),
         scoreA_9 =if_else(A_9==antworten$A_9,1,0),
         scoreA_10 =if_else(A_10==antworten$A_10,1,0),
         scoreA_11 =if_else(A_11==antworten$A_11,1,0),
         scoreA_12 =if_else(A_12==antworten$A_12,1,0),
         scoreA_13 =if_else(A_13==antworten$A_13,1,0),
         scoreA_14 =if_else(A_14==antworten$A_14,1,0))
         # scoreA_15 =if_else(A_15==antworten$A_15,1,0),
         # scoreA_16 =if_else(A_16==antworten$A_16,1,0),
         # scoreA_17 =if_else(A_17==antworten$A_17,1,0),
         # scoreA_18 =if_else(A_18==antworten$A_18,1,0),
         # scoreA_19 =if_else(A_19==antworten$A_19,1,0),
         # scoreA_20 =if_else(A_20==antworten$A_20,1,0),
         # scoreA_21 =if_else(A_21==antworten$A_21,1,0),
         # scoreA_22 =if_else(A_22==antworten$A_22,1,0),
         # scoreA_23 =if_else(A_23==antworten$A_23,1,0),
         # scoreA_24 =if_else(A_24==antworten$A_24,1,0),
         # scoreA_25 =if_else(A_25==antworten$A_25,1,0),
         # scoreA_26 =if_else(A_26==antworten$A_26,1,0),
         # scoreA_27 =if_else(A_27==antworten$A_27,1,0),
         # scoreA_28 =if_else(A_28==antworten$A_28,1,0),
         # scoreA_29 =if_else(A_29==antworten$A_29,1,0),
         # scoreA_30 =if_else(A_30==antworten$A_30,1,0),
         # scoreA_31 =if_else(A_31==antworten$A_31,1,0),
         # scoreA_32 =if_else(A_32==antworten$A_32,1,0),
         # scoreA_33 =if_else(A_33==antworten$A_33,1,0),
         # scoreA_34 =if_else(A_34==antworten$A_34,1,0),
         # scoreA_35 =if_else(A_35==antworten$A_35,1,0),
         # scoreA_36 =if_else(A_36==antworten$A_36,1,0),
         # scoreA_37 =if_else(A_37==antworten$A_37,1,0),
         # scoreA_38 =if_else(A_38==antworten$A_38,1,0),
         # scoreA_39 =if_else(A_39==antworten$A_39,1,0),
         # scoreA_40 =if_else(A_40==antworten$A_40,1,0),
         # scoreA_41 =if_else(A_41==antworten$A_41,1,0),
         # scoreA_42 =if_else(A_42==antworten$A_42,1,0),
         # scoreA_43 =if_else(A_43==antworten$A_43,1,0))
  
  #Auswertung KPrim Fragen
results <- results  %>%
  mutate(scoreK_1_1 =if_else(K_1_1==antworten$K_1_1,0.25,0),
       scoreK_1_2 =if_else(K_1_2==antworten$K_1_2,0.25,0),
       scoreK_1_3 =if_else(K_1_3==antworten$K_1_3,0.25,0),
       scoreK_1_4 =if_else(K_1_4==antworten$K_1_4,0.25,0),
       K_1TOT = scoreK_1_1+scoreK_1_2+scoreK_1_3+scoreK_1_4,
       scoreK_1=if_else(K_1TOT==1,1, if_else(K_1TOT==0.75,0.5,0)),
       
       scoreK_2_1 =if_else(K_2_1==antworten$K_2_1,0.25,0),
       scoreK_2_2 =if_else(K_2_2==antworten$K_2_2,0.25,0),
       scoreK_2_3 =if_else(K_2_3==antworten$K_2_3,0.25,0),
       scoreK_2_4 =if_else(K_2_4==antworten$K_2_4,0.25,0),
       K_2TOT = scoreK_2_1+scoreK_2_2+scoreK_2_3+scoreK_2_4,
       scoreK_2=if_else(K_2TOT==1,1, if_else(K_2TOT==0.75,0.5,0)),
       
       scoreK_3_1 =if_else(K_3_1==antworten$K_3_1,0.25,0),
       scoreK_3_2 =if_else(K_3_2==antworten$K_3_2,0.25,0),
       scoreK_3_3 =if_else(K_3_3==antworten$K_3_3,0.25,0),
       scoreK_3_4 =if_else(K_3_4==antworten$K_3_4,0.25,0),
       K_3TOT = scoreK_3_1+scoreK_3_2+scoreK_3_3+scoreK_3_4,
       scoreK_3=if_else(K_3TOT==1,1, if_else(K_3TOT==0.75,0.5,0)),
       
       scoreK_4_1 =if_else(K_4_1==antworten$K_4_1,0.25,0),
       scoreK_4_2 =if_else(K_4_2==antworten$K_4_2,0.25,0),
       scoreK_4_3 =if_else(K_4_3==antworten$K_4_3,0.25,0),
       scoreK_4_4 =if_else(K_4_4==antworten$K_4_4,0.25,0),
       K_4TOT = scoreK_4_1+scoreK_4_2+scoreK_4_3+scoreK_4_4,
       scoreK_4=if_else(K_4TOT==1,1, if_else(K_4TOT==0.75,0.5,0)),
       
       scoreK_5_1 =if_else(K_5_1==antworten$K_5_1,0.25,0),
       scoreK_5_2 =if_else(K_5_2==antworten$K_5_2,0.25,0),
       scoreK_5_3 =if_else(K_5_3==antworten$K_5_3,0.25,0),
       scoreK_5_4 =if_else(K_5_4==antworten$K_5_4,0.25,0),
       K_5TOT = scoreK_5_1+scoreK_5_2+scoreK_5_3+scoreK_5_4,
       scoreK_5=if_else(K_5TOT==1,1, if_else(K_5TOT==0.75,0.5,0)),
       
       scoreK_6_1 =if_else(K_6_1==antworten$K_6_1,0.25,0),
       scoreK_6_2 =if_else(K_6_2==antworten$K_6_2,0.25,0),
       scoreK_6_3 =if_else(K_6_3==antworten$K_6_3,0.25,0),
       scoreK_6_4 =if_else(K_6_4==antworten$K_6_4,0.25,0),
       K_6TOT = scoreK_6_1+scoreK_6_2+scoreK_6_3+scoreK_6_4,
       scoreK_6=if_else(K_6TOT==1,1, if_else(K_6TOT==0.75,0.5,0)),
       
       scoreK_7_1 =if_else(K_7_1==antworten$K_7_1,0.25,0),
       scoreK_7_2 =if_else(K_7_2==antworten$K_7_2,0.25,0),
       scoreK_7_3 =if_else(K_7_3==antworten$K_7_3,0.25,0),
       scoreK_7_4 =if_else(K_7_4==antworten$K_7_4,0.25,0),
       K_7TOT = scoreK_7_1+scoreK_7_2+scoreK_7_3+scoreK_7_4,
       scoreK_7=if_else(K_7TOT==1,1, if_else(K_7TOT==0.75,0.5,0)),
       
       scoreK_8_1 =if_else(K_8_1==antworten$K_8_1,0.25,0),
       scoreK_8_2 =if_else(K_8_2==antworten$K_8_2,0.25,0),
       scoreK_8_3 =if_else(K_8_3==antworten$K_8_3,0.25,0),
       scoreK_8_4 =if_else(K_8_4==antworten$K_8_4,0.25,0),
       K_8TOT = scoreK_8_1+scoreK_8_2+scoreK_8_3+scoreK_8_4,
       scoreK_8=if_else(K_8TOT==1,1, if_else(K_8TOT==0.75,0.5,0)),
       
       scoreK_9_1 =if_else(K_9_1==antworten$K_9_1,0.25,0),
       scoreK_9_2 =if_else(K_9_2==antworten$K_9_2,0.25,0),
       scoreK_9_3 =if_else(K_9_3==antworten$K_9_3,0.25,0),
       scoreK_9_4 =if_else(K_9_4==antworten$K_9_4,0.25,0),
       K_9TOT = scoreK_9_1+scoreK_9_2+scoreK_9_3+scoreK_9_4,
       scoreK_9=if_else(K_9TOT==1,1, if_else(K_9TOT==0.75,0.5,0)),
       
       scoreK_10_1 =if_else(K_10_1==antworten$K_10_1,0.25,0),
       scoreK_10_2 =if_else(K_10_2==antworten$K_10_2,0.25,0),
       scoreK_10_3 =if_else(K_10_3==antworten$K_10_3,0.25,0),
       scoreK_10_4 =if_else(K_10_4==antworten$K_10_4,0.25,0),
       K_10TOT = scoreK_10_1+scoreK_10_2+scoreK_10_3+scoreK_10_4,
       scoreK_10=if_else(K_10TOT==1,1, if_else(K_10TOT==0.75,0.5,0)),
       
       scoreK_11_1 =if_else(K_11_1==antworten$K_11_1,0.25,0),
       scoreK_11_2 =if_else(K_11_2==antworten$K_11_2,0.25,0),
       scoreK_11_3 =if_else(K_11_3==antworten$K_11_3,0.25,0),
       scoreK_11_4 =if_else(K_11_4==antworten$K_11_4,0.25,0),
       K_11TOT = scoreK_11_1+scoreK_11_2+scoreK_11_3+scoreK_11_4,
       scoreK_11=if_else(K_11TOT==1,1, if_else(K_11TOT==0.75,0.5,0)),
       
       scoreK_12_1 =if_else(K_12_1==antworten$K_12_1,0.25,0),
       scoreK_12_2 =if_else(K_12_2==antworten$K_12_2,0.25,0),
       scoreK_12_3 =if_else(K_12_3==antworten$K_12_3,0.25,0),
       scoreK_12_4 =if_else(K_12_4==antworten$K_12_4,0.25,0),
       K_12TOT = scoreK_12_1+scoreK_12_2+scoreK_12_3+scoreK_12_4,
       scoreK_12=if_else(K_12TOT==1,1, if_else(K_12TOT==0.75,0.5,0)),
       
       scoreK_13_1 =if_else(K_13_1==antworten$K_13_1,0.25,0),
       scoreK_13_2 =if_else(K_13_2==antworten$K_13_2,0.25,0),
       scoreK_13_3 =if_else(K_13_3==antworten$K_13_3,0.25,0),
       scoreK_13_4 =if_else(K_13_4==antworten$K_13_4,0.25,0),
       K_13TOT = scoreK_13_1+scoreK_13_2+scoreK_13_3+scoreK_13_4,
       scoreK_13=if_else(K_13TOT==1,1, if_else(K_13TOT==0.75,0.5,0)),
       
       scoreK_14_1 =if_else(K_14_1==antworten$K_14_1,0.25,0),
       scoreK_14_2 =if_else(K_14_2==antworten$K_14_2,0.25,0),
       scoreK_14_3 =if_else(K_14_3==antworten$K_14_3,0.25,0),
       scoreK_14_4 =if_else(K_14_4==antworten$K_14_4,0.25,0),
       K_14TOT = scoreK_14_1+scoreK_14_2+scoreK_14_3+scoreK_14_4,
       scoreK_14=if_else(K_14TOT==1,1, if_else(K_14TOT==0.75,0.5,0)))
       
       # scoreK_15_1 =if_else(K_15_1==antworten$K_15_1,0.25,0),
       # scoreK_15_2 =if_else(K_15_2==antworten$K_15_2,0.25,0),
       # scoreK_15_3 =if_else(K_15_3==antworten$K_15_3,0.25,0),
       # scoreK_15_4 =if_else(K_15_4==antworten$K_15_4,0.25,0),
       # K_15TOT = scoreK_15_1+scoreK_15_2+scoreK_15_3+scoreK_15_4,
       # scoreK_15=if_else(K_15TOT==1,1, if_else(K_15TOT==0.75,0.5,0)),
       # 
       # scoreK_16_1 =if_else(K_16_1==antworten$K_16_1,0.25,0),
       # scoreK_16_2 =if_else(K_16_2==antworten$K_16_2,0.25,0),
       # scoreK_16_3 =if_else(K_16_3==antworten$K_16_3,0.25,0),
       # scoreK_16_4 =if_else(K_16_4==antworten$K_16_4,0.25,0),
       # K_16TOT = scoreK_16_1+scoreK_16_2+scoreK_16_3+scoreK_16_4,
       # scoreK_16=if_else(K_16TOT==1,1, if_else(K_16TOT==0.75,0.5,0)),
       # 
       # scoreK_17_1 =if_else(K_17_1==antworten$K_17_1,0.25,0),
       # scoreK_17_2 =if_else(K_17_2==antworten$K_17_2,0.25,0),
       # scoreK_17_3 =if_else(K_17_3==antworten$K_17_3,0.25,0),
       # scoreK_17_4 =if_else(K_17_4==antworten$K_17_4,0.25,0),
       # K_17TOT = scoreK_17_1+scoreK_17_2+scoreK_17_3+scoreK_17_4,
       # scoreK_17=if_else(K_17TOT==1,1, if_else(K_17TOT==0.75,0.5,0)),
       # 
       # scoreK_18_1 =if_else(K_18_1==antworten$K_18_1,0.25,0),
       # scoreK_18_2 =if_else(K_18_2==antworten$K_18_2,0.25,0),
       # scoreK_18_3 =if_else(K_18_3==antworten$K_18_3,0.25,0),
       # scoreK_18_4 =if_else(K_18_4==antworten$K_18_4,0.25,0),
       # K_18TOT = scoreK_18_1+scoreK_18_2+scoreK_18_3+scoreK_18_4,
       # scoreK_18=if_else(K_18TOT==1,1, if_else(K_18TOT==0.75,0.5,0)),
       # 
       # scoreK_19_1 =if_else(K_19_1==antworten$K_19_1,0.25,0),
       # scoreK_19_2 =if_else(K_19_2==antworten$K_19_2,0.25,0),
       # scoreK_19_3 =if_else(K_19_3==antworten$K_19_3,0.25,0),
       # scoreK_19_4 =if_else(K_19_4==antworten$K_19_4,0.25,0),
       # K_19TOT = scoreK_19_1+scoreK_19_2+scoreK_19_3+scoreK_19_4,
       # scoreK_19=if_else(K_19TOT==1,1, if_else(K_19TOT==0.75,0.5,0)),
       # 
       # scoreK_20_1 =if_else(K_20_1==antworten$K_20_1,0.25,0),
       # scoreK_20_2 =if_else(K_20_2==antworten$K_20_2,0.25,0),
       # scoreK_20_3 =if_else(K_20_3==antworten$K_20_3,0.25,0),
       # scoreK_20_4 =if_else(K_20_4==antworten$K_20_4,0.25,0),
       # K_20TOT = scoreK_20_1+scoreK_20_2+scoreK_20_3+scoreK_20_4,
       # scoreK_20=if_else(K_20TOT==1,1, if_else(K_20TOT==0.75,0.5,0)),
       # 
       # scoreK_21_1 =if_else(K_21_1==antworten$K_21_1,0.25,0),
       # scoreK_21_2 =if_else(K_21_2==antworten$K_21_2,0.25,0),
       # scoreK_21_3 =if_else(K_21_3==antworten$K_21_3,0.25,0),
       # scoreK_21_4 =if_else(K_21_4==antworten$K_21_4,0.25,0),
       # K_21TOT = scoreK_21_1+scoreK_21_2+scoreK_21_3+scoreK_21_4,
       # scoreK_21=if_else(K_21TOT==1,1, if_else(K_21TOT==0.75,0.5,0)),
       # 
       # scoreK_22_1 =if_else(K_22_1==antworten$K_22_1,0.25,0),
       # scoreK_22_2 =if_else(K_22_2==antworten$K_22_2,0.25,0),
       # scoreK_22_3 =if_else(K_22_3==antworten$K_22_3,0.25,0),
       # scoreK_22_4 =if_else(K_22_4==antworten$K_22_4,0.25,0),
       # K_22TOT = scoreK_22_1+scoreK_22_2+scoreK_22_3+scoreK_22_4,
       # scoreK_22=if_else(K_22TOT==1,1, if_else(K_22TOT==0.75,0.5,0)),
       # 
       # scoreK_23_1 =if_else(K_23_1==antworten$K_23_1,0.25,0),
       # scoreK_23_2 =if_else(K_23_2==antworten$K_23_2,0.25,0),
       # scoreK_23_3 =if_else(K_23_3==antworten$K_23_3,0.25,0),
       # scoreK_23_4 =if_else(K_23_4==antworten$K_23_4,0.25,0),
       # K_23TOT = scoreK_23_1+scoreK_23_2+scoreK_23_3+scoreK_23_4,
       # scoreK_23=if_else(K_23TOT==1,1, if_else(K_23TOT==0.75,0.5,0)),
       # 
       # scoreK_24_1 =if_else(K_24_1==antworten$K_24_1,0.25,0),
       # scoreK_24_2 =if_else(K_24_2==antworten$K_24_2,0.25,0),
       # scoreK_24_3 =if_else(K_24_3==antworten$K_24_3,0.25,0),
       # scoreK_24_4 =if_else(K_24_4==antworten$K_24_4,0.25,0),
       # K_24TOT = scoreK_24_1+scoreK_24_2+scoreK_24_3+scoreK_24_4,
       # scoreK_24=if_else(K_24TOT==1,1, if_else(K_24TOT==0.75,0.5,0)))


#Punkte berechnen mit richtiger Auswertung KPrim
results <- results %>% 
  mutate(final_score=scoreA_1+scoreA_2+scoreA_3+scoreA_4+scoreA_5+
           scoreA_6+scoreA_7+scoreA_8+scoreA_9+scoreA_10+
           scoreA_11+scoreA_12+scoreA_13+scoreA_14+
           #scoreA_15+
           # scoreA_16+scoreA_17+scoreA_18+scoreA_19+scoreA_20+
           # scoreA_21+scoreA_22+scoreA_23+scoreA_24+
           scoreK_1+scoreK_2+scoreK_3+scoreK_4+scoreK_5+
           scoreK_6+scoreK_7+scoreK_8+scoreK_9+scoreK_10+
           scoreK_11+scoreK_12+scoreK_13+scoreK_14
         #+scoreK_15
           # scoreK_16+scoreK_17+scoreK_18+scoreK_19+scoreK_20+
           # scoreK_21+scoreK_22+scoreK_23+scoreK_24
         )

  
### Alles Abspeichern in einem separaten Sheet
Alleresultate <- results %>% 
  select(Matrikel,StudisID,Nachname, Vorname, scoreA_1,scoreA_2,scoreA_3,scoreA_4,scoreA_5,
           scoreA_6,scoreA_7,scoreA_8,scoreA_9,scoreA_10,
           scoreA_11,scoreA_12,scoreA_13,scoreA_14,
           #scoreA_15,
           # scoreA_16,scoreA_17,scoreA_18,scoreA_19,scoreA_20,
           # scoreA_21,scoreA_22,scoreA_23,scoreA_24,
           scoreK_1,scoreK_2,scoreK_3,scoreK_4,scoreK_5,
           scoreK_6,scoreK_7,scoreK_8,scoreK_9,scoreK_10,
           scoreK_11,scoreK_12,scoreK_13,scoreK_14
         #+scoreK_15,
         # scoreK_16,scoreK_17,scoreK_18,scoreK_19,scoreK_20,
         # scoreK_21,scoreK_22+,scoreK_23,scoreK_24
  ,final_score)
write.xlsx(Alleresultate, file="HS2019-1 Emot Ergebnisse.xlsx",
           sheetName="AlleAntworten", append=TRUE)


### nur Final Score speichern in einem separaten Sheet
 nur_score <- results %>% 
   select(Matrikel,StudisID,Nachname, Vorname, final_score)
 write.xlsx(nur_score, file="HS2019-1 Emot Ergebnisse.xlsx",
            sheetName="PunkteNeu", append=TRUE)

# Check summary
summary(nur_score)
