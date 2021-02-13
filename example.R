library(ggplot2)
PimaIndiansDiabets <- read.csv('pima.csv')

str(PimaIndiansDiabets)
summary(PimaIndiansDiabets)

df <- PimaIndiansDiabets
df$diabetes <- as.factor(df$diabetes)
zero_to_na_vars <- c('glucose',
                     'pressure',
                     'triceps',
                     'insulin',
                     'mass')

for (var in zero_to_na_vars) {
  df[[var]] <- ifelse(df[[var]] == 0,
                      NA,
                      df[[var]])
}
df<- na.omit(df)

# ящики с усами
boxplot(df$glucose)
boxplot(df$triceps)

# гистограмма
hist(df$triceps)
hist(df$triceps, breaks=20)


# несколько графиков на одном
def.par <- par(no.readonly = T)
par(mfrow = c(2,1))
hist(df$glucose, xlab = NULL)
boxplot(df$glucose,
        horisontal=TRUE,
        frame=FALSE)


# 6 графиков
layout(matrix(c(c(1, 1, 2), c(3, 3, 4), c(5, 5, 6)),
              nrow = 3,
              ncol = 3,
              byrow = TRUE)
)
hist(df$glucose, xlab = NULL)
boxplot(df$glucose,
        horizontal = T,
        frame = F)
par(def.par)

# стобчатые диаграммы
plot(df$diabetes)
table(df$diabetes)

#ggplot2
ggplot(df, aes(x = mass, y = pressure)) + geom_point()

ggplot(df, aes(x = pregnant, y = age)) + geom_point()

ggplot(df, aes(x=mass, y=pressure)) + geom_point(color='blue', size = 3)+theme_minimal() + ggtitle('Mass vs Pressure')

ggplot(df, aes(x=mass, y=pressure)) + geom_point(color='blue', 
                                                 size = 3,
                                                 fill='yellow',
                                                 shape=23)+theme_minimal() + ggtitle('Mass vs Pressure')


ggplot(df, aes(x=mass, y=triceps)) + geom_point(color='blue', 
                                                 size = 3,
                                                 fill='lightblue',
                                                 shape=21) + geom_smooth(method='lm', color = 'darkred')+theme_minimal() + ggtitle('Mass vs Pressure')


ggplot (df, aes(x=mass)) + geom_density(color = 'blue', fill = 'lightblue', size = 0.5) + theme_classic()

ggplot (df, aes(x=mass)) + geom_boxplot(color = 'darkblue', fill = 'darkblue', alpha = 0.5) + coord_flip() + theme_classic()

ggplot (df, aes(x= diabetes, y = mass)) + geom_boxplot()+ theme_classic()

ggplot (df, aes(x= diabetes, y = mass)) + geom_violin()+ theme_classic()

ggplot (df, aes(x= diabetes, y = mass)) + geom_boxplot() + geom_jitter(width=0.1) +  theme_classic()

ggplot (df, aes(x= diabetes, y = mass)) + geom_boxplot(color = 'blue', fill = 'lightblue') + geom_jitter(color = 'red', width=0.1) +  theme_classic()

ggplot (df, aes(x=diabetes, y = mass)) + geom_violin(color = 'darkgreen', fill = 'lightgreen') + geom_boxplot(fill = 'lightgreen', width=0.5) +  theme_classic()

ggplot (df, aes(x = diabetes)) + geom_bar() + theme_classic()
  
ggplot (df, aes(x=pregnant)) + geom_bar() + theme_classic()

summary(df$age)

df$age_group <- cut(df$age, breaks = 6)

summary(df$age_group)

df$age_group <- cut(df$age, breaks = c(20, 30, 40, 50, 81))

levels(df$age_group) <- c('21-30', '31-40', '41-50', '50+')

ggplot (df, aes(x=age_group)) + geom_bar(fill='white', color = 'black') + theme_classic()

ggplot (df, aes(x = age_group, fill = diabetes)) + geom_bar() + theme_classic()

ggplot (df, aes(x = age_group, fill = diabetes)) + geom_bar(position = 'dodge') + theme_classic()

ggplot (df, aes(x = age_group, fill = diabetes)) + geom_bar(position = 'fill') + theme_classic()

ggplot (df, aes(x = mass, y = glucose,  color = insulin, size = insulin)) + geom_point(alpha = 0.6) + theme_classic()

ggplot (df, aes(x = mass, y = glucose,  shape = diabetes)) + geom_point(alpha = 0.6) + theme_classic()

ggplot (df, aes(x = mass, y = glucose,  color = diabetes)) + geom_point() + theme_classic()

ggplot (df, aes(x= glucose,  fill = diabetes)) + geom_density(alpha = 0.6) + theme_classic()

ggplot (df, aes(y = glucose,  fill = diabetes)) + geom_boxplot(alpha = 0.6) + theme_classic()

ggplot (df, aes(x= glucose)) + geom_density(alpha = 0.6,  fill = 'lightblue') + facet_grid(age_group ~ .) + theme_classic()

ggplot (df, aes(y = glucose)) + geom_boxplot(alpha = 0.6,  fill = 'lightblue') + facet_grid(. ~ age_group) + theme_classic()

ggplot (df, aes(x = glucose, fill = diabetes)) + geom_density(alpha = 0.6) + facet_grid(age_group ~ .) + theme_classic()

ggplot (df, aes(y = glucose, x = diabetes, fill = diabetes)) + geom_boxplot(alpha = 0.6) + facet_grid(. ~ age_group) + theme_classic()

ggplot (df, aes(y = glucose)) + geom_boxplot(alpha = 0.6, fill = 'blue') + facet_grid(age_group ~ diabetes) + theme_classic()

ggplot (df, aes(x = mass, y = glucose,  size = insulin)) + geom_point(color = 'darkblue', alpha = 0.6) + theme_classic()

ggplot (df, aes(x = mass, y = glucose,  color = diabetes, size = insulin)) + geom_point() + theme_classic()

ggplot (df, aes(y = glucose, x = mass,  size = insulin)) + geom_point(color = 'darkblue', alpha = 0.6) + facet_grid(. ~ diabetes)  + theme_classic()

df$age_group=cut(df$age, breaks=c(20, 50, 81))
summary(df$age_group)
levels(df$age_group)=c("21-50", "50+")
summary(df$age_group)

ggplot(df, aes(x=mass, y=glucose,color=age_group))+geom_point( alpha=0.6)+facet_grid(.~diabetes)+theme_classic()

ggplot(df, aes(x=mass, y=glucose,color=diabetes))+geom_point( alpha=0.6)+facet_grid(.~age_group)+theme_classic()

df$age_group <- cut(df$age, breaks = c(20, 30, 40, 50, 81))

levels(df$age_group) <- c('21-30', '31-40', '41-50', '50+')

title_name <- "Indian Woman Diabetes"
x_name <- "2-Hour serum insulin (mu U/ml) [log10]"
y_name <- 'Plasma glucose concentraion'
g_name <- 'Age Groups'
m_name <- 'BMI'
palette_age <- "PuRd"

p <- ggplot(df, aes(x = insulin,
                    y = glucose,
                    fill = age_group,
                    size = mass)) + geom_point(shape = 21,
                                               color = 'black',
                                               alpha = 0.8) + facet_grid(.~diabetes)
p + scale_x_log10() + scale_y_log10() + 
  scale_fill_brewer(palette = palette_age) + 
  ggtitle(title_name) + 
  xlab(x_name) + ylab(y_name) + 
  guides(fill = guide_legend(title=g_name),
         size = guide_legend(title=m_name)) + 
  theme_bw()

ggplot(df, aes(x = pregnant,
               y = mass,
               fill = age_group,
               size = triceps)) + geom_point(shape = 21,
                                          color = 'black',
                                          alpha = 0.3) + facet_grid(diabetes~.) + theme_bw() + scale_fill_brewer(palette = palette_age)
