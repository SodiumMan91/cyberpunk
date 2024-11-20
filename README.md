# Cyberpunk
Cyberpunk / Neon inspired ggplot theme

Install with:
```{r}
devtools::install_github("SodiumMan91/cyberpunk")
```

Sample use-case
```{r}
library(ggplot2)
library(cyberpunk)
ggplot(airquality, aes(x = Day,
                       y = Temp,
                       group = as.factor(Month),
                        color = as.factor(Month))) +
    geom_point(size = 2.5) +
    theme_cpunk()
```
![image](https://github.com/user-attachments/assets/0c295ad2-ea82-4341-bc34-9439659b977b)
