library(vcd)
bot_dat <- readRDS("/workspaces/ST201-Work/Last_Years_Exam_Data/bot_datB.rds")
bot_dat
bot_tab <- table(bot_dat)
bot_tab
addmargins(bot_tab)
# What proportion of humans selected “data” as a preference?
51 / 77
# What proportion of bots selected “data” as a preference?
38 / 73
# Create a mosaic plot and for these data.
mosaic(bot_tab, main = "Mosaic Plot of Bot Data", shade = TRUE, color = TRUE)
# Find the expected frequencies for these data.
chisq.test(bot_tab)$expected