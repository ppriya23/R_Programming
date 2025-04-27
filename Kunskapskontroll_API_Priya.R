library(pxweb)
library(tidyverse)
library(ggplot2)
library(dplyr)

#Latest DATA API
# PXWEB query 
pxweb_query_list <- 
  list("Fordonsslag"=c("PERS"),
       "Bestand"=c("ITRAF"),
       "ContentsCode"=c("TK1001A1"),
       "Tid"=c("2021M03","2021M04","2021M05","2021M06","2021M07","2021M08","2021M09","2021M10","2021M11","2021M12","2022M01","2022M02","2022M03","2022M04","2022M05","2022M06","2022M07","2022M08","2022M09","2022M10","2022M11","2022M12","2023M01","2023M02","2023M03","2023M04","2023M05","2023M06","2023M07","2023M08","2023M09","2023M10","2023M11","2023M12","2024M01","2024M02","2024M03","2024M04","2024M05","2024M06","2024M07","2024M08","2024M09","2024M10","2024M11","2024M12","2025M01","2025M02","2025M03"))

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/en/ssd/TK/TK1001/TK1001A/Fordon",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
head(px_data_frame)

summary(px_data_frame)
colnames(px_data_frame)
str(px_data_frame)


# Rename and clean
colnames(px_data_frame) <- c("VehicleType", "Status", "Month", "Count")
px_data_frame$Count <- as.numeric(px_data_frame$Count)

# Convert Month to ordered factor
px_data_frame$Month <- factor(px_data_frame$Month, levels = unique(px_data_frame$Month), ordered = TRUE)

# Plot
ggplot(px_data_frame, aes(x = Month, y = Count, group = 1)) +
  geom_line(color = "Blue", linewidth = 1.2) +
  labs(title = "Passenger Cars in Use in Sweden (SCB API)",
       x = "Month",
       y = "Number of Cars") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


