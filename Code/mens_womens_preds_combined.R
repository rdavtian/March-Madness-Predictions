kaggle_preds_mens_glmnet
kaggle_preds_mens_gbm
kaggle_preds_mens_xgbtree
kaggle_preds_mens_rf




mens_preds <- kaggle_preds_mens_gbm %>% dplyr::select(ID, Pred)
womens_preds <- kaggle_preds_womens_gbm %>% dplyr::select(ID, Pred)

kaggle_mens_submission_2023_2 <- kaggle_mens_submission_2023 %>% 
  left_join(mens_preds, by = "ID") %>% 
  tidyr::replace_na(list(Pred = 0.5))

kaggle_womens_submission_2023_2 <- kaggle_womens_submission_2023 %>% 
  left_join(womens_preds, by = "ID") %>% 
  tidyr::replace_na(list(Pred = 0.5))

final_kaggle_preds <- rbind(kaggle_mens_submission_2023_2, kaggle_womens_submission_2023_2)

write.csv(final_kaggle_preds, "kaggle_predictions_gbm.csv", row.names = F)
