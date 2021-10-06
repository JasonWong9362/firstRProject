library(tidyverse)
library(Stat2Data)
data("Hawks")
hawksSmall<-drop_na(select(Hawks,Age,Day,Month,Year,CaptureTime,Species,Wing,Weight,Tail))
# 1.1

# Check how many rows and columns
dim(hawksSmall)

# display the top 5 rows of the hawksSmall 
head(hawksSmall)

# ?
# month: discrete
# species: categorical
# age: dicrete
# wing: continuous
# weight: continuous

# 1.2
# so much infor, can not sight clearly

# 1.3 
weight_plot <- ggplot(data=hawksSmall, aes(x=Weight)) + xlab("Wegiht(gm)")
weight_plot + geom_histogram(binwidth = 100) + ylab("Count")
# mapping x with weight
# unimodel

# 1.4
tail_plot <- ggplot(data=hawksSmall, aes(x=Tail)) +xlab("Tail(mm)")
# adjust = 0.5
tail_plot + geom_density(adjust=0.5) + ylab("Density")
# adjust = 1
tail_plot + geom_density(adjust=1) + ylab("Density")
# How many modes does the data distribution of Hawk tail lengths have?

# density
multi_tail_plot <- ggplot(data=hawksSmall, aes(x=Tail, color=Species))
multi_tail_plot + geom_density() + theme_bw() + xlab("Tail(mm)") + ylab("Density") 

# violin
multi_tail_plot <- ggplot(data=hawksSmall, aes(x=Tail, y=Species, fill=Species))
multi_tail_plot + geom_violin() + theme_bw() + xlab("Tail(mm)") + ylab("Density") 

# 1.5 
# 4 aes
multi_weight_tail_plot <- ggplot(data=hawksSmall, aes(x=Tail, y=Weight, color=Species, shape=Species))
multi_weight_tail_plot + geom_point() + theme_bw() + xlab("Tail(mm)") + ylab("Weight(gm)")

# 1.6
weight_tail_facet <- ggplot(data=hawksSmall, aes(x=Tail, y=Weight, color=Species))
weight_tail_facet + geom_point() + theme_bw() + facet_wrap(~Species) + geom_smooth(method="lm") + xlab("Tail(mm)") + ylab("Weight(gm)")

# 2.1
hSF <- hawksSmall %>%
  filter(Species=="RT" &　Weight>=1000) %>%
  select(Wing, Weight, Tail)

# 2.2 
hsF <- arrange(hSF, Wing)

# 2.3
# create df
species_code <- c("CH", "RT", "SS")
species_full_name <- c("Cooper's", "Red-tailed", "Sharp-shinned")
full_name <- data.frame(species_code, species_full_name)
# rename() changes the names of individual variables using new_name = old_name
full_name <- rename(full_name, Species = species_code)
# wrangling
hawksFullName <- left_join(Hawks, full_name) %>%
  select(-Species) %>%
  rename(Species = species_full_name)
# display top 10, 3 features
head(select(hawksFullName, Species, Wing, Weight), 7)

# 2.4
hawksWithBMI <- Hawks %>% 
  mutate(bird_BMI = (1000*Weight)/(Wing^2)) %>%
  select(Species, bird_BMI) %>%
  arrange(desc(bird_BMI))

head(hawksWithBMI, 8)

# violin
multi_tail_plot <- ggplot(data=filter(hawksWithBMI, bird_BMI<=100), aes(x=bird_BMI, y=Species, fill=Species))
multi_tail_plot + geom_violin() + theme_bw() + xlab("Brid BMI") + ylab("Species") + xlim(0,16)

# 2.5
hawksFullName %>%
  group_by(Species) %>%
  summarise(num_rows=n(), mn_wing=mean(Wing, na.rm=1), md_wing=median(Wing, na.rm=1), 
            t_mn_wing=mean(Wing, trim=0.1, na.rm=1), tail_wing_radio=mean(Wing/Tail, na.rm=1)) %>%
  head()

# 2.6
hawksFullName %>%
  group_by(Species) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  select(Wing,Weight,Culmen,Hallux,Tail,StandardTail,Tarsus,Crop)
  head()

# 3.1
Hawks %>%
  summarise(Wing_mean=mean(Wing, na.rm=1), Wing_t_mean=mean(Wing, trim=0.1, na.rm=1), 
            Wing_med=median(Wing, na.rm=1), Weight_mean=mean(Weight, na.rm=1),
            Weight_t_mean=mean(Weight, trim=0.1, na.rm=1), Weight_med=median(Weight, na.rm=1)) %>%
  head()

Hawks %>%
  group_by(Species) %>%
  summarise(Wing_mean=mean(Wing, na.rm=1), Wing_t_mean=mean(Wing, trim=0.1, na.rm=1), 
            Wing_med=median(Wing, na.rm=1), Weight_mean=mean(Weight, na.rm=1),
            Weight_t_mean=mean(Weight, trim=0.1, na.rm=1), Weight_med=median(Weight, na.rm=1)) %>%
  head()

# 3.2


# 3.3

# 3.4
# box plot
weight_box <- ggplot(Hawks, aes(x=Species, y=Weight)) 
weight_box + geom_boxplot() + xlab("Species") + ylab("Weight")
# Note the outliers displayed as individual dots

# function “num_outliers”
num_outliers <- funtion(v){
  
}

# 
Hawks %>% 
  group_by(Species) %>%
  summarise(Species, outliers_weight=)
  







