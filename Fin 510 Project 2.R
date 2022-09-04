library(tidyverse)
library(rdrobust)
# 1.
df=haven::read_dta("mortality.dta")
df$rod_any=100000*df$cod_any/(df$pop/12)
df$post=df$agemo_mda>0
df1=df%>%
  filter(agemo_mda>=-24 & agemo_mda<=24)
plot(df1$agemo_mda,df1$rod_any,xlab="agemo_mda",ylab="rod_any")
mt1=sum(df$rod_any[1:24])
mt2=sum(df$rod_any[25:49])
d1=mt2-mt1
# 2
df$rod_MVA=100000*df$cod_MVA/(df$pop/12)
filter(df, between(agemo_mda, -24, 24)) %>% 
  ggplot(aes(x = agemo_mda)) + 
  geom_point(aes(y = rod_any), color = "black",pch=15) + 
  geom_point(aes(y = rod_MVA), color = "blue")+
  geom_vline(xintercept = 0)
# 3
rddf=as.data.frame(matrix(nrow=4,ncol=3))
names(rddf)=c("bandwidth","rod_any","rod_MVA")
rddf[,1]=c(48,24,12,6)
l1=c(48,24,12,6)
df2=df[-49,]
for (i in 1:length(l1)){
  rd3=rdrobust(df2$rod_any,df2$agemo_mda,c=0,h=l[i])
  rdplot(df2$rod_any,df2$agemo_mda,c=0,h=48)
  rddf[i,2]=rd3$coef[1]
  rd4=rdrobust(df2$rod_MVA,df2$agemo_mda,c=0,h=l[i])
  rddf[i,3]=rd4$coef[1]
}
# 4
rddf2=as.data.frame(matrix(nrow=4,ncol=3))
names(rddf2)=c("bandwidth","rod_any","rod_MVA")
rddf2[,1]=c(48,24,12,6)
for (i in 1:length(l1)){
  rd <- lm(rod_any ~ post * agemo_mda, 
           data = df, 
           subset = between(agemo_mda, -l1[i], l1[i]) & agemo_mda != 0)
  
  rd2 <- lm(rod_MVA ~ post * agemo_mda, 
            data = df, 
            subset = between(agemo_mda, -l1[i], l1[i]) & agemo_mda != 0)
  broom::tidy(rd)
  df <- df %>% 
    mutate(rod_any_pre = predict(rd, mutate(mortality, post = FALSE)),
           rod_any_post = predict(rd, mutate(mortality, post = TRUE)),
           rod_MVA_pre = predict(rd2, mutate(mortality, post = FALSE)),
           rod_MVA_post = predict(rd2, mutate(mortality, post = TRUE)))
  df2=df %>% 
    select(agemo_mda, rod_any_pre, rod_any_post, rod_MVA_pre, rod_MVA_post) %>% 
    filter(agemo_mda == 0) %>% 
    mutate(rd = rod_any_post - rod_any_pre,
           rd2=rod_any_post - rod_any_pre)
  rddf2[i,2:3]=df2[,6:7]
}
