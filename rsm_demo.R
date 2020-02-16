library(rsm)
library(dplyr)

# datasetの確認
ChemReact
ChemReact1 <- ChemReact %>% filter(Block == "B1") %>% select(-Block)
ChemReact2 <- ChemReact %>% filter(Block == "B2") %>% select(-Block)

# dataのcoding
CR1 <- coded.data(ChemReact1, x1 ~ (Time-85)/5, x2 ~ (Temp-175)/5)
CR1
as.data.frame(CR1)

# experimental design
ccd.pick(2, n.c = c(4, 8), blks.c = c(1, 2, 4), restrict = "N<=60")
CUBE <- cube(2, n0 = 3, 
             coding = list(x1 ~ (Time-85)/5, x2 ~ (Temp-175)/5))
DESIGN <- djoin(CUBE, star(n0 = 3, alpha = "rotatable", reps = 1))
DESIGN

ChemReact

# 応答曲面法
CR1_rsm <- rsm(Yield ~ FO(x1, x2), data = CR1)
summary(CR1_rsm)
steepest(CR1_rsm, dist = c(0, 0.5, 1))
# combined block
CR2 <- djoin(CR1, ChemReact2)
CR2
CR2_rsm <- rsm(Yield ~ Block + SO(x1, x2), data = CR2)
summary(CR2_rsm)

contour(CR2_rsm, ~ x1 + x2, 
        image = TRUE,
        at = summary(CR2_rsm)$canonical$xs)
