library(tidyverse)


## read data ----
results <- readxl::read_excel("HS2019-1 Emot Ergebnisse.xlsx",
                              sheet = "Antworten") %>%
    select(Matrikel,
           StudisID,
           Nachname,
           Vorname,
           starts_with("A_"),
           starts_with("K_"))

antworten <- readxl::read_excel("HS2019-1 Emot Ergebnisse.xlsx",
                                sheet = "Loesung")%>%
    select(starts_with("A_"), starts_with("K_"))

answers <- antworten %>%
    pivot_longer(everything(), names_to = "question", values_to = "value")


## take 5 students at random ----
results <- results %>%
    slice_sample(n = 2)

res <- results %>%
    pivot_longer(!c(Matrikel, StudisID,
                    Nachname, Vorname),
                 names_to = "question",
                 values_to = "value")



test <- left_join(res, answers)



## score A questions ----

results <- results  %>%
    mutate(scoreA_1 = if_else(A_1 == antworten$A_1, 1, 0),
           scoreA_2 = if_else(A_2 == antworten$A_2, 1, 0),
           scoreA_3 = if_else(A_3 == antworten$A_3, 1, 0),
           scoreA_4 = if_else(A_4 == antworten$A_4, 1, 0),
           scoreA_5 = if_else(A_5 == antworten$A_5, 1, 0),
           scoreA_6 = if_else(A_6 == antworten$A_6, 1, 0),
           scoreA_7 = if_else(A_7 == antworten$A_7, 1, 0),
           scoreA_8 = if_else(A_8 == antworten$A_8, 1, 0),
           scoreA_9 = if_else(A_9 == antworten$A_9, 1, 0),
           scoreA_10 = if_else(A_10 == antworten$A_10, 1, 0),
           scoreA_11 = if_else(A_11 == antworten$A_11, 1, 0),
           scoreA_12 = if_else(A_12 == antworten$A_12, 1, 0),
           scoreA_13 = if_else(A_13 == antworten$A_13, 1, 0),
           scoreA_14 = if_else(A_14 == antworten$A_14, 1, 0))


## score K questions ----

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



## final score ----
results <- results %>%
    mutate(final_score = scoreA_1+scoreA_2+scoreA_3+scoreA_4+scoreA_5+
               scoreA_6+scoreA_7+scoreA_8+scoreA_9+scoreA_10+
               scoreA_11+scoreA_12+scoreA_13+scoreA_14+
               scoreK_1+scoreK_2+scoreK_3+scoreK_4+scoreK_5+
               scoreK_6+scoreK_7+scoreK_8+scoreK_9+scoreK_10+
               scoreK_11+scoreK_12+scoreK_13+scoreK_14
    )


nur_score <- results %>%
    select(Matrikel, StudisID, Nachname, Vorname, final_score)
