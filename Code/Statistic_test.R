library(readxl)
library(tidyverse)
library(FSA)
library(patchwork)

# Specify the file path
file_path <- "Adaptive_Sampling/Results/MODEL_METRICS.xlsx"

# Reads the two separate sheets (assuming there are two tables)
df <- read_excel(file_path)


# Test the effect of the ratio (1:1 vs. 1:10) for ROC_AUC
kruskal.test(ROC_AUC ~ Ratio, data = df)

# View data
p1 <- ggplot(df, aes(x = Ratio, y = ROC_AUC)) +
  geom_boxplot(width = 0.5) +
  scale_x_discrete(labels = c("1_1" = "Ratio: 1:1", "1_10" = "Ratio: 1:10")) +
  scale_y_continuous(limits = c(0.75, 1)) +
  labs(y = expression(AUC[ROC]), x = NULL) 


# Calculate the median difference
df %>%
  group_by(Ratio) %>%
  summarise(Mediana = median(ROC_AUC))

# Test the effect of the ratio (1:1 vs. 1:10) for PR_AUC
kruskal.test(PR_AUC ~ Ratio, data = df)

# View data
p2 <- ggplot(df, aes(x = Ratio, y = PR_AUC)) +
  geom_boxplot(width = 0.5) +
  scale_x_discrete(labels = c("1_1" = "Ratio: 1:1", "1_10" = "Ratio: 1:10")) +
  scale_y_continuous(limits = c(0.75, 1)) +
  labs(y = expression(AUC[PR]), x = NULL) 

# Calculate the median difference
df %>%
  group_by(Ratio) %>%
  summarise(Mediana = median(PR_AUC))



# Test the effect of the buffer on ROC_AUC across the entire dataset
kruskal.test(ROC_AUC ~ Buffer, data = df)

# Ensure that Buffer is a factor with the levels in the correct order.
df$Buffer <- as.factor(df$Buffer)


# View data
p3 <- ggplot(df, aes(x = Buffer, y = ROC_AUC)) +
  geom_boxplot(width = 0.5) +
  scale_x_discrete(
    limits = c("Nobuffer", "5km", "10km", "20km"),
    labels = c("Nobuffer" = "No buffer",
               "5km" = "5 km",
               "10km" = "10 km",
               "20km" = "20 km")) +
  scale_y_continuous(limits = c(0.75, 1)) +
  labs(y = expression(AUC[ROC]), x = NULL) 

# Calculate the median difference
df %>%
  group_by(Buffer) %>%
  summarise(Mediana = median(ROC_AUC))

# Test the effect of the buffer for PR_AUC on the entire dataset
kruskal.test(PR_AUC ~ Buffer, data = df)


# View data
p4 <- ggplot(df, aes(x = Buffer, y = PR_AUC)) +
  geom_boxplot(width = 0.5) +
  scale_x_discrete(
    limits = c("Nobuffer", "5km", "10km", "20km"),
    labels = c("Nobuffer" = "No buffer",
               "5km" = "5 km",
               "10km" = "10 km",
               "20km" = "20 km")) +
  scale_y_continuous(limits = c(0.75, 1)) +
  labs(y = expression(AUC[PR]), x = NULL) 


# Calculate the median difference
df %>%
  group_by(Buffer) %>%
  summarise(Mediana = median(PR_AUC))


ptot1 <- p1 + p2  + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, ncol = 2)
ptot2 <- p3 + p4  + plot_annotation(tag_levels = list(c('C','D'))) + plot_layout(nrow = 1, ncol = 2)



# View the overall performance of the models based on ROC_AUC
p5 <- ggplot(df, aes(x = Model, y = ROC_AUC)) +
  geom_boxplot(width = 0.5) +
  scale_y_continuous(limits = c(0.75, 1)) +
  labs(y = expression(AUC[ROC]), x = NULL) 

# Calculate the median difference
df %>%
  group_by(Model) %>%
  summarise(Mediana = median(ROC_AUC))


# View the overall performance of the models based on PR_AUC
p6 <- ggplot(df, aes(x = Model, y = PR_AUC)) +
  geom_boxplot(width = 0.5) +
  scale_y_continuous(limits = c(0.75, 1)) +
  labs(y = expression(AUC[PR]), x = NULL) 


ptot3 <- p5 + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, ncol = 1)
ptot4 <- p6 + plot_annotation(tag_levels = list(c('B'))) + plot_layout(nrow = 1, ncol = 1)




# Calculate the median difference
df %>%
  group_by(Model) %>%
  summarise(Mediana = median(PR_AUC))





