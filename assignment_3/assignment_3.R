library(tidyverse)
library(Stat2Data)
library(readxl)
# 1

# 2 missing data & imputation
impute_by_mean<-function(x){
  mu<-mean(x,na.rm=1) # first compute the mean of x
  impute_f<-function(z){ # coordinate-wise imputation
    if(is.na(z)){
      return(mu) # if z is na replace with mean
    }else{
      return(z) # otherwise leave in place
    }
  }
  return(map_dbl(x,impute_f)) # apply the map function to impute across vector
}

impute_by_median<-function(x){
  mu<-median(x,na.rm=1) # first compute the mean of x
  impute_f<-function(z){ # coordinate-wise imputation
    if(is.na(z)){
      return(mu) # if z is na replace with mean
    }else{
      return(z) # otherwise leave in place
    }
  }
  return(map_dbl(x,impute_f)) # apply the map function to impute across vector
}

v<-c(1,2,NA,4)
impute_by_median(v)

x<-seq(0,10,0.1)
create_ySeq<-function(n){5*n+1}
y<-create_ySeq(x)
df_xy<-data.frame(x,y)
head(df_xy,5)

# map2() iterates over two variables

sometimes_missing<-function(index, value){
  if(index%%5==0)
    return(NA)
  else
    return(value)
}


df_xy_missing<-df_xy%>%
  mutate(y=map2_dbl(row_number(y),y,sometimes_missing))

head(df_xy_missing, 10)

df_xy_impute<-df_xy_missing%>%
  mutate(y=impute_by_median(y))

head(df_xy_impute, 10)

df_xy<-df_xy%>%
  mutate(source="original")
df_xy_missing<-df_xy_missing%>%
  mutate(source="corrupted")
df_xy_impute<-df_xy_impute%>%
  mutate(source="imputed")
df_combined<-rbind(df_xy,df_xy_missing,df_xy_impute)

ggplot(df_combined,aes(x=x,y=y,color=source))+geom_point()+
  facet_wrap(~source)+geom_smooth(method="lm")

# 2.2
# how to read in a sheet within an excel file as a data frame
# load the readxl library
folder_path<-"C:/Users/Jason/Desktop/R_project/assignment/assignment_3" 
file_name<-"/HockeyLeague.xlsx" 
file_path<-paste(folder_path,file_name,sep="") # create the file_path

# create win_tidy
wins_data_frame<-read_excel(file_path,sheet="Wins") # read of a sheet from an xl file
wins_data_frame%>%
  select(1:10)%>%
  head(3)

# four columns entitled “Team” chr , “Year” int , “Wins” int , “Total” int.
wins_tidy<-wins_data_frame%>%
  rename(Team=...1)%>%
  pivot_longer(cols = !Team, names_to = "Year", values_to = "w_of_t")%>%
  #rename(Team=...1)#%>%
  separate(w_of_t,into = c("Wins", "Total"), sep = " of ", convert = TRUE)%>%
  mutate(Year=as.integer(Year))
  #convert = TRUE, convert string to int 
head(wins_tidy)

# create losses_tidy
losses_data_frame<-read_excel(file_path,sheet="Losses") # read of a sheet from an xl file
losses_tidy<-losses_data_frame%>%
  rename(Team=...1)%>%
  pivot_longer(cols = !Team, names_to = "Year", values_to = "w_of_t")%>%
  #rename(Team=...1)#%>%
  separate(w_of_t,into = c("Losses", "Total"), sep = " of ", convert = TRUE)%>%
  mutate(Year=as.integer(Year))
#convert = TRUE, convert string to int 

# create hockey_df
hockey_df<-inner_join(wins_tidy, losses_tidy)%>%
  mutate(Draws=Total-Wins-Losses, Wins_rt=Wins/Total,
         Losses_rt=Losses/Total, Draws_rt=Draws/Total)
  
head(hockey_df)

hockey_sum_df<-hockey_df%>%
  group_by(Team)%>%
  summarize(win_mean=mean(Wins_rt, na.rm=TRUE), loss_mean=mean(Losses_rt, na.rm=TRUE),
            draw_mean=mean(Draws_rt, na.rm=TRUE), win_mid=median(Wins_rt, na.rm=TRUE), 
            loss_mid=median(Losses_rt, na.rm=TRUE), draw_mid=median(Draws_rt, na.rm=TRUE))%>%
  arrange(desc(win_mid))

head(hockey_sum_df)

# 2.3
top_correlates_by_var<-function(df){
  
  # get numerical features name
  fea_name<-df%>%
    select_if(is.numeric)%>%
    colnames()
  
  max_cor_var<-function(col_name){
    # function to determine the variable with maximal correlation
    v_col<-df%>%select(all_of(col_name))
    # extract variable based on col_name
    df_num<-df%>%
      select_if(is.numeric)%>%
      select(-all_of(col_name))
    # select all numeric variables excluding col_name
    correlations<-unlist(map(df_num,
                             function(x){cor(x,v_col,use="complete.obs")}))
    # compute correlations with all other numeric variables
    max_abs_cor_var<-names(which(abs(correlations)==max(abs(correlations))))
    return(max_abs_cor_var)
  }
  
  top_col<-map_chr(fea_name, max_cor_var)
  top_col_df <- data.frame(matrix(ncol = 5, nrow = 0))
  top_col_df <- rbind(top_col_df, top_col)
  colnames(top_col_df) <- fea_name
  
  return (top_col_df)
} 

library(palmerpenguins)

penguins%>%
  select_if(is.numeric)%>%
  colnames()

penguins%>%
  top_correlates_by_var()

penguins%>%
  group_by(species)%>%
  # nest all of properties of a instance except the species property
  nest()%>%
  mutate(max_cop=map(data, top_correlates_by_var))%>%
  select(-data)%>%
  unnest(cols=max_cop)
  









  



