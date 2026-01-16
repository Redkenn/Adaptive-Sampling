library(tibble)
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)

# Predictor Importance Data
df1 <- data.frame(
  Variable = factor(c(
    "Slope", "Bio13", "Bio9", "Bio8", "AI", "Bio5", "Silt", "DNI", "Bio15",
    "GHI", "Dist_c", "Bio3", "Bio18", "Bio14"
  ), levels = c(
    "Slope", "Bio13", "Bio9", "Bio8", "AI", "Bio5", "Silt", "DNI", "Bio15",
    "GHI", "Dist_c", "Bio3", "Bio18", "Bio14"
  )),
  Scenario = rep(c(
    "No buffer", "5 km", "10 km", "20 km"
  ), each = 14),
  MDG = c(
    # 1:1_NoBuffer
    24.35, 11.49, 13.05, 8.96, 6.80, 7.45, 8.53, 6.81, 6.48, 5.76, 5.56, 4.97, 3.45, 4.16,
    # 1:1_Buffer5km
    23.47, 15.40, 10.34, 5.15, 10.11, 7.91, 10.21, 6.06, 9.12, 5.42, 4.02, 3.70, 4.73, 3.38,
    # 1:1_Buffer10km
    24.08, 8.51, 13.93, 7.42, 9.90, 7.79, 8.62, 5.11, 7.90, 5.80, 7.19, 4.25, 4.52, 4.99,
    # 1:1_Buffer20km
    13.96, 13.12, 13.56, 11.27, 9.03, 10.43, 5.80, 4.82, 6.57, 5.79, 9.68, 3.56, 6.89, 5.32
  )
)



# Calculation of MDG mean to order variables
variable_order <- df1 %>%
  group_by(Variable) %>%
  summarise(mean_mdg = mean(MDG)) %>%
  arrange(desc(mean_mdg)) %>%
  pull(Variable)

# Sorting scenarios (for each ratio: NoBuffer → Buffer5km → Buffer10km → Buffer20km)
scenario_order <- c(
  "No buffer", "5 km", "10 km", "20 km"
)

# We apply the new ordered factors
df1$Variable <- factor(df1$Variable, levels = rev(variable_order))  # Alto → Basso
df1$Scenario <- factor(df1$Scenario, levels = scenario_order)

# Heat Map
p1 <- ggplot(df1, aes(x = Scenario, y = Variable, fill = MDG)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1f", MDG)), size = 3, color = "black") +
  #scale_fill_viridis(name = "MDG", option = "D", direction = -1) +
  scale_fill_gradient(low = "white", high = "red", name = "MDG") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(face = "bold")
  ) +
  labs(
    title = "",
    x = "Ratio 1:1", y = ""
  )


# Predictor Importance Data
df2 <- data.frame(
  Variable = factor(c(
    "Slope", "Bio13", "Bio9", "Bio8", "AI", "Bio5", "Silt", "DNI", "Bio15",
    "GHI", "Dist_c", "Bio3", "Bio18", "Bio14"
  ), levels = c(
    "Slope", "Bio13", "Bio9", "Bio8", "AI", "Bio5", "Silt", "DNI", "Bio15",
    "GHI", "Dist_c", "Bio3", "Bio18", "Bio14"
  )),
  Scenario = rep(c(
    "No buffer", "5 km", "10 km", "20 km"
  ), each = 14),
  MDG = c(
    # 1:10_NoBuffer
    36.69, 24.43, 14.03, 8.20, 17.45, 12.49, 8.99, 16.64, 13.22, 16.39, 19.96, 9.59, 8.44, 8.03,
    # 1:10_Buffer5km
    39.48, 23.66, 21.57, 19.31, 17.07, 7.41, 14.54, 12.70, 10.22, 14.54, 13.42, 8.92, 6.11, 7.73,
    # 1:10_Buffer10km
    35.38, 27.44, 15.18, 14.48, 15.59, 9.46, 11.40, 17.90, 14.45, 14.68, 13.41, 10.33, 11.49, 9.01,
    # 1:10_Buffer20km
    37.64, 26.34, 15.70, 12.00, 19.78, 8.26, 16.32, 11.31, 13.95, 13.94, 15.03, 8.03, 9.86, 11.07
  )
)



# Calculation of MDG mean to order variables
variable_order <- df2 %>%
  group_by(Variable) %>%
  summarise(mean_mdg = mean(MDG)) %>%
  arrange(desc(mean_mdg)) %>%
  pull(Variable)

# Sorting scenarios (for each ratio: NoBuffer → Buffer5km → Buffer10km → Buffer20km)
scenario_order <- c(
  "No buffer", "5 km", "10 km", "20 km"
)

# We apply the new ordered factors
df2$Variable <- factor(df2$Variable, levels = rev(variable_order))  # Alto → Basso
df2$Scenario <- factor(df2$Scenario, levels = scenario_order)




# Heat Map
p2 <- ggplot(df2, aes(x = Scenario, y = Variable, fill = MDG)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1f", MDG)), size = 3, color = "black") +
  #scale_fill_viridis(name = "MDG", option = "D", direction = -1) +
  scale_fill_gradient(low = "white", high = "red", name = "MDG") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(face = "bold")
  ) +
  labs(
    title = "",
    x = "Ratio 1:10", y = ""
  )


ptot1 <- p1 + p2 + plot_layout(nrow = 1, ncol = 2) 
