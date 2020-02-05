# Alex Bettinardi
# 1-10-19

# Quick script to tabulate university students

per <- read.csv("output/GP_synthetic_persons.csv",as.is=T)
students <- per[per$SCHG>5&per$WKHP<35,]
print(table(students$AGEP<30) )
