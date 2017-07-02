install.packages("qdapRegex")
library(qdapRegex)
x <- "\n      \n        Animal Farm\n      by\n\nGeorge Orwell\n\n\n        \n               3.86 avg rating — 1,919,698 ratings\n        \n                  \n            \n              \n            \n          \n\n\n          "
a <- rm_white(x)
a
