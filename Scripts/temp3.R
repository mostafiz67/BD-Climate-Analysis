plot_compare_psn_analysis_Min_temp_monsoon <- ggplot(plot_final_df_PSN_analysis_Min_temp_monsoon, 
                                                     aes(F_Monsoon, avg_Min_temp_year_monsoon_psn_predict))+
  geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.9))+
  theme_bw()+
  facet_wrap(~Monsoon)+
  theme(panel.grid = element_blank())+
  xlab("Monsoon")+
  ylab("Avg Min Temp")+
  ggtitle("Avg Min Temp vs Monsoon")