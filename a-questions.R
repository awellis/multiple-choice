library(tidyverse)

## read data ----
results_wide <- readxl::read_excel("HS2019-1 Emot Ergebnisse.xlsx",
                              sheet = "Antworten") %>%
    select(Matrikel, StudisID, Nachname, Vorname, starts_with("A_"))


## take 5 students at random (for testing) ----
# results_wide <- results_wide %>%
#     slice_sample(n = 5)


answers_wide <- readxl::read_excel("HS2019-1 Emot Ergebnisse.xlsx",
                                sheet = "Loesung") %>%
    select(starts_with("A_"))


## convert to long ----
answers <- answers_wide %>%
    pivot_longer(everything(),
                 names_to = "A_question",
                 values_to = "A_correct_response")

results <- results_wide %>%
    pivot_longer(!c(Matrikel, StudisID, Nachname, Vorname),
                 names_to = "A_question",
                 values_to = "A_response")


## join results and answers, add indicator column ----
joined <- results %>%
    left_join(answers) %>%
    mutate(correct = if_else(A_response == A_correct_response, 1, 0))


## count correct responses ----
a_score <- joined %>%
    group_by(Matrikel, StudisID, Nachname, Vorname) %>%
    summarise(score_a = sum(correct))
