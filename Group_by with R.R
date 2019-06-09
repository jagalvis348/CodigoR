#install.packages("dplyr")
library(dplyr)

### agrupar mtcars por cilindros y devolver algunos promedios

mtcars<-mtcars

cars <- mtcars %>%
  select(cyl, mpg, hp, qsec) %>%    #se selecciona las variables que van en la columna
  group_by(cyl) %>%
  summarise(mpg = mean(mpg), hp = mean(hp), qsec = mean(qsec))   #se escoge la estadistica

cars





