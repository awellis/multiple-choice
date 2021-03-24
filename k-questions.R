library(tidyverse)

## read data ----
results_wide <- readxl::read_excel("HS2019-1 Emot Ergebnisse.xlsx",
                                   sheet = "Antworten") %>%
    select(Matrikel, StudisID, Nachname, Vorname, starts_with("K_"))


## take 5 students at random (for testing) ----
# results_wide <- results_wide %>%
#     slice_sample(n = 2)


answers_wide <- readxl::read_excel("HS2019-1 Emot Ergebnisse.xlsx",
                                   sheet = "Loesung") %>%
    select(starts_with("K_"))


## convert to long ----
answers <- answers_wide %>%
    pivot_longer(everything(),
                 names_to = "question",
                 values_to = "correct_response")

results <- results_wide %>%
    pivot_longer(!c(Matrikel, StudisID, Nachname, Vorname),
                 names_to = "question",
                 values_to = "response")


joined <- results %>%
    left_join(answers)


joined <- joined %>%
    mutate(question = str_remove(question, "K_")) %>%
    separate(question,
             into = c("question_num", "item_num"),
             sep = "_")


joined <- joined %>%
    mutate(item = if_else(response == correct_response, 1/4, 0))


final_K_score <- joined %>%
    group_by(Matrikel, StudisID, Nachname, Vorname, question_num) %>%
    summarise(total = sum(item))

final_K_score <- final_K_score %>%
    mutate(score = case_when(
        total == 1 ~ 1,
        total == 0.75 ~ 0.5,
        total < 0.75 ~ 0))

k_score <- final_K_score %>%
    group_by(Matrikel, StudisID, Nachname, Vorname) %>%
    summarise(score_k = sum(score))



total <- a_score %>%
    inner_join(k_score) %>%
    rename_with(~str_remove(.x, pattern = "score_"),
                starts_with("score_")) %>%
    mutate(score = a + k)
