

files <- list.files("wc2.1_cruts4.09_5m_tmin_2020-2024")
files

commands <- paste0("r.import input='C:\\Users\\Nietolaj\\OneDrive - Mondigroup\\NC State\\Spring 2026\\Geospatial Computation and Simulation\\wc2.1_cruts4.09_5m_tmin_2020-2024\\",
                   files, "' output=", gsub(".tif", "", gsub("wc2.1_cruts4.09_5m_", "", gsub("-", "_", files))))

writeLines(commands, "commands.txt")




