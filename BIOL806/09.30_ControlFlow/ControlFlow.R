library(ggplot2)
gapdata <- read.csv("09.16_PlottinginR/data/gapminder_data.csv")
gapdata = data.frame(gapdata)
dir.create("CountryFolder")

for (j in unique(gapdata[,"continent"])){
  
  dir.create(paste0("CountryFolder/", j))
  dir.create(paste0("CountryFolder/", j, "/LifeExpPlots"))
  
  for (i in unique(gapdata[,"country"])){
    df <- subset(gapdata, gapdata$country == i)
    if(df$"continent" == j){
      write.csv(df, paste0("CountryFolder/",j,"/",i,"_Data.csv"),
                row.names = TRUE)
      ggplot(data = df, aes(x = year, y = lifeExp)) +
        geom_point() +
        ggtitle(i) +
        xlab("Year") + ylab("Life Expectancy")
      
      ggsave(paste0(i,"LifeExp.jpeg"),
             plot = last_plot(),
             path =paste0("CountryFolder/",j, "/LifeExpPlots"))
    }
  }
}




# Write an R script (.R) (not a RMarkdown) that pulls in the gapminder data and then subsets the data by country. For each country, produce a subset of the data and export it as a new .csv file. And for each country, also create and export a plot of lifeExp versus year. 
 
# Throughout this assignment, think carefully about where you are saving these files to have good organization and file management.
 
# Useful commands:
# unique()
# paste()
# read.csv()
# write.csv()
# ggplot()
# ggsave()

# 
# for (j in unique(gapdata[,"continent"])){
#   #continent <- toString(gapdata[j,"continent"])
#   dir.create(paste0("CountryFolder/",j))
#   #create a folder for each continent
#   for (i in unique(gapdata[,"country"])){
#     df <- subset(gapdata, gapdata$country == i)
#     #rename df
#     #save csv
#     if(df$"continent" == j){
#     write.csv(df, paste0("CountryFolder/",j,"/",i,"_Data.csv"),
#             #File.path("CountryFolder"),
#                 row.names = TRUE)
#     #plot and then save to same folder
#     ggplot(data = df, aes(x = year, y = lifeExp)) +
#     geom_point() +
#     ggtitle(i) +
#     xlab("Year") + ylab("Life Expectancy")
#     
#     ggsave(paste0(i,"LifeExp.jpeg"),
#          plot = last_plot(),
#          path =paste0("CountryFolder/",j))
#     }
#   }  
# }
  
  
 



#### Below are my failed attempts. What an adventure this was.####

 
#   #toString(gapdata[i,"country"]) <- data.frame()
#   for (j in 1:nrow(gapdata)){
#     if(toString(gapdata[i,"country"]) == toString(gapdata[j, "country"])){
#   #if statement about country
#     df<-append(df, gapdata[j])
#   #paste that row in to data frame if it matches country
#   #make it a dataframe
#     }
#   }
#   #create df
#   #export csv
#   #Make graph
# }






# '%ni%' <- Negate('%in%')
# country_list <- list()
# 
# for (index in 1:nrow(gapdata)){
#   if (toString(gapdata[index, 1]) %ni% country_list){
#     country_list <- append(country_list, toString(gapdata[index, "country"]))
#     
#     for(a in 1:length(country_list)){
#       if(toString(country_list[a]) == toString(gapdata[index, "country"])){
#         country_list <- append(country_list[a], gapdata[index,])
#       }
#     }
#   }else{
#     for(a in 1:length(country_list)){
#       if(toString(country_list[a]) == toString(gapdata[index, "country"])){
#         country_list <- append(country_list[a], gapdata[index,])
#       }
#     }
#   }
# }
# 
# head(country_list)
# View(country_list)
# 
# #df <- data.frame(matrix(unlist(country_list), nrow=length(country_list), byrow=TRUE))
# data_frame <- as.data.frame(do.call(cbind, country_list))