# ===========================================
# 1. Install & Load Package
# ===========================================
library(anthro)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(readxl)
library(openxlsx)

# ===========================================
# 2. Siapkan Sheet & Workbook
# ===========================================
file_path <- "Dataset.xlsx"
sheet_names <- excel_sheets(file_path)

if (!dir.exists("plots")) dir.create("plots")
wb <- createWorkbook()
wb_lolos <- createWorkbook()

# Buat dataframe kosong untuk rekap persentase lolos
rekap_lolos <- data.frame(
  Sheet = character(),
  Total_Anak = numeric(),
  Lolos_WAZ = numeric(),
  Lolos_HAZ = numeric(),
  Lolos_WHZ = numeric(),
  Persen_WAZ = numeric(),
  Persen_HAZ = numeric(),
  Persen_WHZ = numeric(),
  stringsAsFactors = FALSE
)

# ===========================================
# 3. Fungsi: Proses Setiap Sheet
# ===========================================
proses_sheet <- function(sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name)
  
  df_long <- df %>%
    mutate(row_id = row_number()) %>%
    pivot_longer(cols = matches("^(U|BB|TB)[0-9]+$"),
                 names_to = c(".value", "Bulan"),
                 names_pattern = "(U|BB|TB)([0-9]+)") %>%
    mutate(Bulan = as.numeric(Bulan)) %>%
    select(Nama, JK, Bulan, U, BB, TB)
  
  z_scores <- anthro_zscores(
    sex = df_long$JK,
    age = df_long$U,
    is_age_in_month = TRUE,
    weight = df_long$BB,
    lenhei = df_long$TB
  )
  
  df_long <- cbind(df_long, z_scores[, c("zlen", "zwei", "zwfl")])
  df_long <- df_long %>% rename(HAZ = zlen, WAZ = zwei, WHZ = zwfl)
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, df_long)
  
  max_bulan <- max(df_long$Bulan, na.rm = TRUE)
  bulan_labels <- paste("Bulan", 0:max_bulan)
  
  # ===================
  # Plot Semua Anak
  # ===================
  for (indikator in c("HAZ", "WAZ", "WHZ")) {
    p <- ggplot(df_long, aes_string(x = "Bulan", y = indikator, group = "Nama", color = "Nama")) +
      geom_line(size = 1.2) + geom_point(size = 2) +
      geom_hline(yintercept = -2, linetype = "dashed", color = "red") +
      scale_y_continuous(limits = c(-3, 3)) +
      scale_x_continuous(breaks = 0:max_bulan, labels = bulan_labels) +
      labs(x = "Bulan", y = indikator) +
      theme(legend.position = "none")
    
    ggsave(filename = paste0("plots/ZScore_", indikator, "_", sheet_name, ".png"),
           plot = p, width = 10, height = 6, dpi = 300)
  }
  
  # ===================
  # Ringkasan & Lolos
  # ===================
  df_summary <- df_long %>%
    arrange(Nama, Bulan) %>%
    group_by(Nama) %>%
    summarise(
      WAZ_first = first(WAZ), WAZ_last = last(WAZ),
      HAZ_first = first(HAZ), HAZ_last = last(HAZ),
      WHZ_first = first(WHZ), WHZ_last = last(WHZ),
      .groups = "drop"
    ) %>%
    mutate(
      lolos_WAZ = WAZ_first < -2 & WAZ_last > -2,
      lolos_HAZ = HAZ_first < -2 & HAZ_last > -2,
      lolos_WHZ = WHZ_first < -2 & WHZ_last > -2
    )
  
  total_anak <- nrow(df_summary)
  total_lolos_WAZ <- sum(df_summary$lolos_WAZ, na.rm = TRUE)
  total_lolos_HAZ <- sum(df_summary$lolos_HAZ, na.rm = TRUE)
  total_lolos_WHZ <- sum(df_summary$lolos_WHZ, na.rm = TRUE)
  
  # Tambahkan ke rekap
  rekap_lolos <<- rbind(rekap_lolos, data.frame(
    Sheet = sheet_name,
    Total_Anak = total_anak,
    Lolos_WAZ = total_lolos_WAZ,
    Lolos_HAZ = total_lolos_HAZ,
    Lolos_WHZ = total_lolos_WHZ,
    Persen_WAZ = round(100 * total_lolos_WAZ / total_anak, 1),
    Persen_HAZ = round(100 * total_lolos_HAZ / total_anak, 1),
    Persen_WHZ = round(100 * total_lolos_WHZ / total_anak, 1)
  ))
  
  df_lolos_semua <- df_summary %>%
    filter(lolos_WAZ | lolos_HAZ | lolos_WHZ)
  
  df_lolos_long <- df_long %>% filter(Nama %in% df_lolos_semua$Nama)
  addWorksheet(wb_lolos, sheet_name)
  writeData(wb_lolos, sheet = sheet_name, df_lolos_long)
  
  # Plot Anak Lolos per Indikator
  for (indikator in c("WAZ", "HAZ", "WHZ")) {
    nama_lolos <- df_summary$Nama[df_summary[[paste0("lolos_", indikator)]]]
    df_lolos_indikator <- df_long %>% filter(Nama %in% nama_lolos)
    
    if (nrow(df_lolos_indikator) > 0) {
      p <- ggplot(df_lolos_indikator, aes_string(x = "Bulan", y = indikator, group = "Nama", color = "Nama")) +
        geom_line(size = 1.2) + geom_point(size = 2) +
        geom_hline(yintercept = -2, linetype = "dashed", color = "red") +
        scale_y_continuous(limits = c(-3, 3)) +
        scale_x_continuous(breaks = 0:max_bulan, labels = bulan_labels) +
        labs(x = "Bulan", y = indikator) +
        theme(legend.position = "bottom")
      
      ggsave(filename = paste0("plots/ZScore_Lolos_", indikator, "_", sheet_name, ".png"),
             plot = p, width = 10, height = 6, dpi = 300)
    }
  }
}

# ===========================================
# 4. Jalankan Semua Sheet
# ===========================================
for (sheet in sheet_names) {
  cat("Memproses sheet:", sheet, "\n")
  proses_sheet(sheet)
}

# ===========================================
# 5. Rekapitulasi Keseluruhan
# ===========================================
rekap_total <- data.frame(
  Sheet = "TOTAL",
  Total_Anak = sum(rekap_lolos$Total_Anak),
  Lolos_WAZ = sum(rekap_lolos$Lolos_WAZ),
  Lolos_HAZ = sum(rekap_lolos$Lolos_HAZ),
  Lolos_WHZ = sum(rekap_lolos$Lolos_WHZ)
)

rekap_total$Persen_WAZ <- round(100 * rekap_total$Lolos_WAZ / rekap_total$Total_Anak, 1)
rekap_total$Persen_HAZ <- round(100 * rekap_total$Lolos_HAZ / rekap_total$Total_Anak, 1)
rekap_total$Persen_WHZ <- round(100 * rekap_total$Lolos_WHZ / rekap_total$Total_Anak, 1)

rekap_lolos <- rbind(rekap_lolos, rekap_total)

# Simpan ke Excel
write.xlsx(rekap_lolos, "Persentase_Lolos.xlsx", overwrite = TRUE)
saveWorkbook(wb, "ZScore_Export.xlsx", overwrite = TRUE)
saveWorkbook(wb_lolos, "ZScore_Lolos_Only.xlsx", overwrite = TRUE)

# Tampilkan ke console
print(rekap_lolos)
