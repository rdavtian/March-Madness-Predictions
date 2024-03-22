slot_order <- c("R1W1","R1W8","R1W5","R1W4","R1W6","R1W3","R1W7","R1W2",
                "R1X1","R1X8","R1X5","R1X4","R1X6","R1X3","R1X7","R1X2",
                "R1Y1","R1Y8","R1Y5","R1Y4","R1Y6","R1Y3","R1Y7","R1Y2",
                "R1Z1","R1Z8","R1Z5","R1Z4","R1Z6","R1Z3","R1Z7","R1Z2",
                "R2W1","R2W4","R2W3","R2W2","R2X1","R2X4","R2X3","R2X2",
                "R2Y1","R2Y4","R2Y3","R2Y2","R2Z1","R2Z4","R2Z3","R2Z2",
                "R3W1","R3W2","R3X1","R3X2","R3Y1","R3Y2","R3Z1","R3Z2",
                "R4W1","R4X1","R4Y1","R4Z1","R5WX","R5YZ","R6CH")
kaggle2 <- rbind(kaggle_mens, kaggle_womens_2)
kaggle2 <- kaggle2[order(kaggle2$Bracket,kaggle2$Tournament,match(kaggle2$Slot, slot_order)),]
kaggle2$RowId <- seq(0, length(kaggle2$RowId)-1)
write.csv(kaggle2, "kaggle_portfolio_final_v2.csv", row.names = F)
#tourney_seeds_2024 <- read.csv("2024_tourney_seeds.csv")





colnames(bracket) <- c("Season","Region","Seed","Team","Top 32","Sweet 16","Elite 8","Final 4","Final","Champion")
bracket <- bracket %>% 
  mutate(`Top 32` = `Top 32` / 100,
         `Sweet 16` = `Sweet 16` / 100,
         `Elite 8` = `Elite 8` / 100,
         `Final 4` = `Final 4` / 100,
         `Final` = `Final` / 100,
         `Champion` = `Champion` / 100,
         )
tb <- datatable(bracket, rownames = FALSE, filter = "top",
          options = list(pageLength = 100, autoWidth = TRUE),
          caption = "Based on 10,000 Tournament Simulations") %>% 
  DT::formatPercentage(c("Top 32","Sweet 16","Elite 8","Final 4", "Final", "Champion"), digits = 1) %>% 
  color_gradient("Top 32") %>% 
  color_gradient("Sweet 16") %>% 
  color_gradient("Elite 8") %>% 
  color_gradient("Final 4") %>%
  color_gradient("Final") %>%
  color_gradient("Champion")
DT::saveWidget(tb, "2024_March_Madness_Mens_Bracket_Sim_Results_RF.html")





colnames(bracket3) <- c("Season","Region","Seed","Team","Top 32","Sweet 16","Elite 8","Final 4","Final","Champion")
bracket3 <- bracket3 %>% 
  mutate(`Top 32` = `Top 32` / 100,
         `Sweet 16` = `Sweet 16` / 100,
         `Elite 8` = `Elite 8` / 100,
         `Final 4` = `Final 4` / 100,
         `Final` = `Final` / 100,
         `Champion` = `Champion` / 100,
  )
tb <- datatable(bracket3, rownames = FALSE, filter = "top",
                options = list(pageLength = 100, autoWidth = TRUE),
                caption = "Based on 10,000 Tournament Simulations") %>% 
  DT::formatPercentage(c("Top 32","Sweet 16","Elite 8","Final 4", "Final", "Champion"), digits = 1) %>% 
  color_gradient("Top 32") %>% 
  color_gradient("Sweet 16") %>% 
  color_gradient("Elite 8") %>% 
  color_gradient("Final 4") %>%
  color_gradient("Final") %>%
  color_gradient("Champion") 
DT::saveWidget(tb, "2024_March_Madness_Womens_Bracket_Sim_Results_RF.html")
  
