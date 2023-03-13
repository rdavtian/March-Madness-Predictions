mens_preds <- kaggle_preds_men %>% dplyr::select(ID, Pred)
womens_preds <- kaggle_preds_women %>% dplyr::select(ID, Pred)

kaggle_mens_submission_2023 <- kaggle_mens_submission_2023 %>% 
  left_join(mens_preds, by = "ID") %>% 
  tidyr::replace_na(list(Pred = 0.5))

kaggle_womens_submission_2023 <- kaggle_womens_submission_2023 %>% 
  left_join(womens_preds, by = "ID") %>% 
  tidyr::replace_na(list(Pred = 0.5))

final_kaggle_preds <- rbind(kaggle_mens_submission_2023, kaggle_womens_submission_2023)
