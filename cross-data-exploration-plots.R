### Site sensitivity model summaries ###
###         Working across sites         ###

## Data manipulation packages
library(tidyverse)
library(googledrive)#read this tutorial to setup the Google account permissions https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package

#Create directory for file download 
# I put this directory in to the .gitignore so it does not affect the repository when changes are pushed
dir.create("raw_data_GDrive", showWarnings = F)

#Create a tibble with all of the files in the LTER transitions directory
#Don't need to change this since this is where all of the raw files are located
files_ls=drive_ls(as_id("https://drive.google.com/drive/folders/1xkxWpWA9NEeEoUmZfkRMsS9s7zodo7xU"))

#Google asks for access to the pre-authorized Google account 
#my account is under "1" yours may be different 


#Download the dataset based on the file name in the directory
drive_download(file = subset(files_ls,name=="Climate sensitivity"),
               path = "raw_data_GDrive/Climate_sensitivity.xlsx",
               overwrite = TRUE,
               type="xlsx")#Overwrite is in included so you can replace older versions


Climate_sensitivity_raw<-read_xl_sheets(file = "raw_data_GDrive/Climate_sensitivity.xlsx")

Climate_sensitivity_scale<-Climate_sensitivity_raw$Scaled

colnames(Climate_sensitivity_scale)<-str_replace_all(colnames(Climate_sensitivity_scale)," ","_")
colnames(Climate_sensitivity_scale)

ggplot(Climate_sensitivity_scale, aes(x = mean_precip, y = coef_precip, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_precip - se_precip, ymax = coef_precip + se_precip), width = 8) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. standardized\n(NPP ~ MAP)") +
  xlab("MAP")+theme(legend.title = element_blank())

ggplot(Climate_sensitivity_scale, aes(x = mean_max_temp, y = coef_max_temp, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_max_temp - se_max_temp, ymax = coef_max_temp + se_max_temp), width = 1) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. standardized\n(NPP ~ Max Temp)") +
  xlab("Mean max temp")+theme(legend.title = element_blank())

ggplot(Climate_sensitivity_scale, aes(x = mean_spei, y = coef_spei, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_spei - se_spei, ymax = coef_spei + se_spei), width = 0.05) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. standardized\n(NPP ~ Max Temp)") +
  xlab("Mean SPEI")+theme(legend.title = element_blank())



#Log-log

Climate_sensitivity_log<-Climate_sensitivity_raw$Log_log

colnames(Climate_sensitivity_log)<-str_replace_all(colnames(Climate_sensitivity_log)," ","_")
colnames(Climate_sensitivity_log)


ggplot(Climate_sensitivity_log, aes(x = mean_precip, y = coef_precip, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_precip - se_precip, ymax = coef_precip + se_precip), width = 8) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. log-log\n(log NPP ~ log MAP)") +
  xlab("MAP")+theme(legend.title = element_blank())

ggplot(Climate_sensitivity_log, aes(x = mean_max_temp, y = coef_max_temp, color = site_name)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = coef_max_temp - se_max_temp, ymax = coef_max_temp + se_max_temp), width = 1) +
  theme_bw(base_size = 24) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab("Coef. log-log\n(log NPP ~ log Max Temp)") +
  xlab("Mean max temp")+theme(legend.title = element_blank())
