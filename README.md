# ggpirate
pirate plots for ggplot2

# Installation
```
# install.packages("devtools")
devtools::install_github("mikabr/ggpirate")
```

# Usage

```
library(ggpirate)
theme_set(theme_bw())
```

Black and white pirate plot:
```
ggplot(mpg, aes(x = class, y = displ)) +
  geom_pirate()
```

Colour pirate plot:
```
ggplot(mpg, aes(x = class, y = displ)) +
  geom_pirate(aes(colour = class, fill = class))
```
