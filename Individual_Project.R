# Run this script with the submission folder as your working directory.
# Import libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Load data
excel_path <- 'data/Chicago_Food_Inspections_Cleaned.xlsx'
inspections <- read_excel(excel_path, sheet = 'Inspections')
violation <- read_excel(excel_path, sheet = 'Violation Details')
violation_code <- read_excel(excel_path, sheet = 'Inspection Violation Codes')

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Map inspection groups to purpose and initial/re-inspection stage
group_lookup <- tibble::tribble(
  ~`Inspection Group`,                         ~Purpose,                   ~Stage,
  "Routine Inspection",                        "Routine",                  "Initial",
  "Routine Re-Inspection",                     "Routine",                  "Re-inspection",
  "License Inspection",                        "License",                  "Initial",
  "License Re-Inspection",                     "License",                  "Re-inspection",
  "Complaint Inspection",                      "Complaint",                "Initial",
  "Complaint Re-Inspection",                   "Complaint",                "Re-inspection",
  "Suspected Food Poisoning",                  "Suspected food poisoning", "Initial",
  "Suspected Food Poisoning Re-Inspection",    "Suspected food poisoning", "Re-inspection"
)


# Join the eight inspection groups used in this figure
# Keep Pass, Pass w/ Conditions and Fail outcomes only
plot_data <- inspections %>%
  inner_join(group_lookup, by = "Inspection Group") %>%
  filter(Results %in% c("Pass", "Pass w/ Conditions", "Fail"))


# Count completed and failed inspections by purpose and stage; calculate failure rates
inspection_summary <- plot_data %>%
  group_by(Purpose, Stage) %>%
  summarise(Completed = n_distinct(`Inspection ID`),  Failed = n_distinct(`Inspection ID`[Results == "Fail"]), .groups = "drop") %>%
  mutate(Failure_rate = Failed / Completed,
    
    Purpose = factor(Purpose, levels = c("Suspected food poisoning", "Complaint", "License", "Routine")),
    
    Stage = factor(Stage, levels = c("Initial", "Re-inspection")),
    
    Label = paste0(percent(Failure_rate, accuracy = 0.1), "  (n=", comma(Completed), ")"))


# Figure 5
# Food Inspection Failure Rates by Purpose and Stage
dodge <- position_dodge(width = 0.75)
p5 <- ggplot(inspection_summary, aes(x = Purpose, y = Failure_rate, fill = Stage, group = Stage)) +
  geom_col(position = dodge, width = 0.65) +
  geom_text(aes(label = Label), position = dodge, hjust = -0.08, size = 3.8) +
  coord_flip() +
  scale_fill_manual(values = c("Initial" = "#D66B66", "Re-inspection" = "#F2B8B5")) +
  scale_y_continuous(labels = label_percent(accuracy = 1), expand = expansion(mult = c(0, 0.35))) +
  labs(title = "Food Inspection Failure Rates by Purpose and Stage",
        x = "Inspection Purpose",
        y = "Inspection Failure Rate (%)",
        fill = "Inspection Stage",) +
  theme_minimal(base_size = 12, base_family = 'Times New Roman') +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        text = element_text(family = "Times New Roman"))

p5
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Aggregate heatmap data, excluding Unknown, Unclassified and Other facility groups
# Use Pass, Pass w/ Conditions and Fail outcomes
# Count completed and failed inspections and calculate failure rates by risk level
heatmap_data <- inspections %>%
  filter(!`Facility Group` %in% c("Unknown", "Unclassified", "Other"),
    Risk %in% c("Risk 1 (High)", "Risk 2 (Medium)", "Risk 3 (Low)"),
    Results %in% c("Pass", "Pass w/ Conditions", "Fail")) %>%
  group_by(`Facility Group`, Risk) %>%
  summarise(Completed = n_distinct(`Inspection ID`), Failed = n_distinct(`Inspection ID`[Results == "Fail"]), .groups = "drop") %>%
  mutate(Failure_rate = Failed / Completed,
        Risk_level = factor(
          Risk,
          levels = c("Risk 1 (High)", "Risk 2 (Medium)", "Risk 3 (Low)"),
          labels = c("High", "Medium", "Low")))

# Display rates only for groups with at least 50 eligible inspections
heatmap_data <- heatmap_data %>%
  mutate(Rate_display = if_else(Completed >= 50, Failure_rate, NA_real_),
        Label = if_else(Completed >= 50, paste0(percent(Failure_rate, accuracy = 0.1), "\nn=", comma(Completed)), paste0("Small sample\nn=", comma(Completed))))



# Use the facility order from Figure 4
facility_order <- c(
  "Bar / Liquor",
  "School",
  "Bakery",
  "Grocery / Food Retail",
  "Healthcare / Adult Care",
  "Restaurant / Cafe",
  "Mixed Facility",
  "Mobile Food",
  "Childcare / Children's Services",
  "Events / Hospitality",
  "Catering / Shared Kitchen")

heatmap_data <- heatmap_data %>%
  mutate(`Facility Group` = factor(`Facility Group`, levels = rev(facility_order)))

# Draw the figure
p6_1 <- ggplot(heatmap_data, aes(x = Risk_level, y = `Facility Group`, fill = Rate_display)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = Label), size = 3.5) +
      scale_fill_gradient(low = "#FDEDEC", high = "#C94C4C", na.value = "#E2E8F0", labels = label_percent(accuracy = 1),
                          name = "Failure rate", limits = c(0, max(heatmap_data$Rate_display, na.rm = TRUE))) +
      scale_x_discrete(drop = FALSE) +
      labs(title = "Food Inspection Failure Rates by Facility Group and Risk Level",
          x = "Facility Risk Level",
          y = "Facility Group") +
      theme_minimal(base_size = 12, base_family = 'Times New Roman') +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(face = "bold"))

p6_1

heatmap_data <- heatmap_data %>%
  mutate(`Facility Group` = factor(`Facility Group`, levels = rev(facility_order)))


dotplot_data <- heatmap_data %>%
  mutate(Label = if_else(Completed >= 50, paste0(scales::percent(Failure_rate, accuracy = 0.1),
        " (n=", scales::comma(Completed), ")"
      ),
      NA_character_
    ))


plot_data <- dotplot_data %>%
  mutate(y_position = as.numeric(`Facility Group`) +
      case_when(
        Risk_level == "High"   ~  0.23,
        Risk_level == "Medium" ~  0,
        Risk_level == "Low"    ~ -0.23),
    Label = paste0(percent(Rate_display, accuracy = 0.1), " (n=", comma(Completed), ")")) %>%
  filter(!is.na(Rate_display))


# Keep the original facility order
facility_labels <- levels(dotplot_data$`Facility Group`)


# Shorten the children's services label
facility_labels[
  facility_labels == "Childcare / Children's Services"
] <- "Children's Services"

p6_2 <- ggplot(
  plot_data,
  aes(
    x = Rate_display,
    y = y_position,
    colour = Risk_level
  )
) +
  geom_point(size = 3.2) +
  geom_text(
    aes(label = Label),
    hjust = -0.15,
    size = 3.2,
    family = "Times New Roman",
    show.legend = FALSE
  ) +
  scale_colour_manual(
    name = "Facility Risk Level",
    values = c(
      "High"   = "#D66B66",
      "Medium" = "#3577A5",
      "Low"    = "#1B9E91"
    ),
    breaks = c("High", "Medium", "Low"),
    drop = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, 0.40),
    breaks = seq(0, 0.40, 0.10),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq_along(facility_labels),
    labels = facility_labels,
    limits = c(0.5, length(facility_labels) + 0.5),
    expand = expansion(mult = 0)
  ) +
  labs(
    title = "Food Inspection Failure Rates by Facility Group and Risk Level",
    x = "Inspection Failure Rate (%)",
    y = NULL,
    caption = paste0(
      "Failure rate = Fail / (Pass + Pass w/ Conditions + Fail).\n",
      "Only combinations with at least 50 eligible inspections are shown. ",
      "Labels show failure rate and sample size."
    )
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Times New Roman"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      margin = margin(b = 14)
    ),
    legend.position = "top",
    legend.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "#E5E7EB",
      linewidth = 0.4
    ),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      colour = "#475569",
      margin = margin(t = 14)
    ),
    plot.margin = margin(15, 20, 10, 10)
  )

p6_2




# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Count distinct violation codes per inspection
p10_code_counts <- violation_code %>%
                    distinct(`Inspection ID`, `Violation Code`) %>%
                    count(`Inspection ID`, name = "Recorded_code_count")

# Join inspection outcomes
p10_data <- inspections %>%
            select('Inspection ID', 'Results') %>%
            inner_join(p10_code_counts, by = 'Inspection ID') %>%
            filter(Results %in% c("Pass", "Pass w/ Conditions", "Fail")) %>%
            mutate(Results = factor(Results, levels = c('Pass', 'Pass w/ Conditions', 'Fail')))
            
# Summarise count, median, first quartile and third quartile by outcome
p10_summary <- p10_data %>%
               group_by(Results) %>%
               summarise(n = n(), Median = median(Recorded_code_count), Q1 = quantile(Recorded_code_count, 0.25), Q3 = quantile(Recorded_code_count, 0.75), .groups = "drop")


# Legend
p10_labels <- setNames(paste0(p10_summary$Results, "\n(n = ", format(p10_summary$n, big.mark = ",", trim = TRUE), ")"), as.character(p10_summary$Results))


p10 <- ggplot(p10_data, aes(x = Results, y = Recorded_code_count, fill = Results)) +
        geom_boxplot(width = 0.55, outlier.alpha = 0.15, outlier.size = 0.8) +
        scale_fill_manual(values = c("Pass" = "#4C9F95", "Pass w/ Conditions" = "#E9B44C", "Fail" = "#D66B66")) +
        scale_x_discrete(labels = p10_labels) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 8)) +
        labs(title = "Recorded Violation Code Counts by Inspection Outcome",
              x = "Inspection Outcome",
              y = "Number of Distinct Recorded Violation Codes") +
        theme_minimal(base_size = 12, base_family = 'Times New Roman') +
        theme(legend.position = "none",
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face = "bold"),
              plot.caption = element_text(hjust = 0))

p10
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p13_top_codes <- c(55, 47, 49, 51, 10, 56, 38, 53, 58, 57)

p13_labels <- c(
  "55. Facilities upkeep",
  "47. Surface design",
  "49. Surface cleanliness",
  "51. Plumbing / backflow",
  "10. Handwashing sinks",
  "56. Ventilation / lighting",
  "38. Pest control",
  "53. Toilet facilities",
  "58. Allergen training",
  "57. Food handler training"
)


p13_records <- violation_code %>%
  select(`Inspection ID`, `Violation Code`) %>%
  filter(`Violation Code` %in% p13_top_codes) %>%
  distinct()

# Record 1 when an inspection contains a top-10 code; otherwise record 0
p13_wide <- p13_records %>%
  mutate(Present = 1L) %>%
  pivot_wider(
    names_from = `Violation Code`,
    values_from = Present,
    values_fill = 0L)

p13_matrix <- p13_wide %>%
  select(all_of(as.character(p13_top_codes))) %>%
  as.matrix()


# Count inspections recording both codes
p13_both <- crossprod(p13_matrix)

# Count inspections recording each code
p13_code_n <- diag(p13_both)

# Count inspections recording at least one code
p13_either <- outer(p13_code_n, p13_code_n, "+") - p13_both

# Calculate Jaccard co-occurrence proportions
p13_jaccard <- p13_both / p13_either


# Draw the figure
p13_plot_data <- as.data.frame(as.table(p13_jaccard), responseName = "Jaccard") %>%
  rename(Code_A = Var1, Code_B = Var2) %>%
  mutate(
    Code_A = as.character(Code_A),
    Code_B = as.character(Code_B),
    Row = match(Code_A, as.character(p13_top_codes)),
    Col = match(Code_B, as.character(p13_top_codes)),
    Both = as.vector(p13_both)) %>%
  filter(Row > Col) %>%
  mutate(
    Label = sprintf("%.1f%%", 100 * Jaccard),
    Code_A = factor(Code_A, levels = rev(as.character(p13_top_codes))),
    Code_B = factor(Code_B, levels = as.character(p13_top_codes)))

p13_label_lookup <- setNames(p13_labels, as.character(p13_top_codes))

p13 <- ggplot(p13_plot_data, aes(x = Code_B, y = Code_A, fill = Jaccard)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = Label), size = 3) +
  scale_fill_gradient(low = "#FDEDEC", high = "#C94C4C", limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1), labels = scales::label_percent(accuracy = 1), name = "Jaccard\nco-occurrence") +
  scale_x_discrete(limits = as.character(p13_top_codes), labels = p13_label_lookup, drop = FALSE) +
  scale_y_discrete(limits = rev(as.character(p13_top_codes)), labels = p13_label_lookup, drop = FALSE) +
  coord_fixed() +
  labs(
    title = "Co-occurrence of Common Recorded Violation Codes",
    x = NULL,
    y = NULL) +
  theme_minimal(base_size = 11, base_family = 'Times New Roman') +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45,hjust = 1), plot.title = element_text(face = "bold"), plot.caption = element_text(hjust = 0))

p13
