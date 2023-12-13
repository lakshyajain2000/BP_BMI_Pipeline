# Units crossover for STEPS


steps <- readRDS("/Volumes/HeightProject/Original dataset/Data/Surveys/STEPS/STEPSdata_GLU_BP_chol_formatted_latest.RDS")  # STEPS dataframe

# Subset dataframe to desired variables

steps_small<-subset(steps, select = c(bmi, height, waist, weight, id_study, unit_height, unit_weight, unit_waist, age))
steps_small$height<- clean_data(steps_small, 'height')
steps_small$weight <- clean_data(steps_small, 'weight')
steps_small$bmi <- ifelse(is.na(steps_small$bmi), steps_small$weight/((steps_small$height/100)^2), steps_small$bmi) # add bmi by calculation
steps_small$bmi <- clean_data(steps_small, 'bmi')
steps_small$waist[which(steps_small$waist<=0|steps_small$waist>200)] <- NA # Make negative values 0
steps_id <- sort(as.character(unique(steps_small$id_study)))
steps_id <- subset(steps_id, steps_id!='MHL_2018_STEPS')

# Calculate percentage of measurements below 50

# waist_percentage <- as.data.frame(cbind(data_id, rep(NA, length(data_id))))
# for (i in 1:length(data_id)){
#   study_i <- data_small %>% filter(id_study==data_id[i])
#   waist_percentage[i, 2] <- (sum(study_i$waist < 50, na.rm = TRUE)/nrow(study_i))*100
# }

# waist_percentage$V2 <- as.numeric(waist_percentage$V2)
# waist_percentage <- waist_percentage[order(waist_percentage[, 2], decreasing=TRUE), ]
# studies_investigate <- waist_percentage[1:200, ]

studies_investigate <- steps_small %>%
  group_by(id_study) %>%
  dplyr::summarise(perc = sum(waist<50, na.rm=TRUE) / sum(!is.na(waist)) * 100) %>%
  ungroup() %>%
  arrange(desc(perc)) %>%
  slice(1:200)

studies_investigate <- subset(studies_investigate, studies_investigate$id_study!='MHL_2018_STEPS')
studies_investigate <- subset(studies_investigate, studies_investigate$id_study!='PLW_2016_STEPS')
'PLW_2016_STEPS'
pdf("waist v bmi STEPS.pdf",   # The directory you want to save the file in
          width = 16, # The width of the plot in inches
          height = 8, onefile = TRUE)

plot_list <- list()
for (i in 1:(floor(nrow(studies_investigate)/8))) {   # plot 8 per page (4 x 2)
  studies <- studies_investigate[(i-1)*8 + 1:8, ]
  ps <- list()
  for (j in 1:nrow(studies)) {
    study_j <- steps_small %>% filter(id_study == studies$id_study[j])
    p <- ggplot(study_j, aes(x=waist, y=bmi)) + theme_bw() +
      geom_point()+
      xlab("Waist Circumference (cm)") +
      ylab("BMI (kg/m\u00b2)") +
      ggtitle(paste0(studies$id_study[j], "\nWC < 50cm: ", round(studies$perc[j], 1), "%\n")) +
      theme(plot.title = element_text(size = 12),
            axis.text = element_text(size = 10))
    ps[[length(ps)+1]] <- p
  }
  
  egg::ggarrange(plots = ps, nrow = 2)
  
  plot_list[[i]] <- ps
}
dev.off()
