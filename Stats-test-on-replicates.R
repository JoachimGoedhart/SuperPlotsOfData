
df_raw <- read_excel("Desktop/Toy-data/SuperPlotsOfData_Lior.xlsx")


df <- df_raw %>% group_by(FP, Replicate) %>% summarise(mean=mean(intensity))

df <- df %>% pivot_wider(names_from = "FP", values_from = "mean") %>% pivot_longer(!Replicate, names_to="FP", values_to = "mean")

df_controls <- df %>% filter(FP=='mNg') %>% select(Replica = Replicate, control_value = mean, cond = FP)


df_values <- df %>% 
  filter (FP != 'mNg') %>% 
  select(Condition=FP,Replica = Replicate,Value=mean)


df_diff <- df_values %>% full_join(df_controls, by='Replica') %>% unite('Condition' ,c("cond","Condition"), sep = " vs ")


