
displacement_data_ps <- readRDS(file = here::here("data", "displacement_data_ps.rds"))
displacement_data_ll <- readRDS(file = here::here("data", "displacement_data_ll.rds"))

displacement_data <- rbind(displacement_data_ps, displacement_data_ll)

# Save them

saveRDS(object = displacement_data,
        file = here::here("data", "displacement_data.rds"))

