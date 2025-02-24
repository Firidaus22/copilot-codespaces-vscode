library(raster)
library(rgdal)
library(xgboost)
library(dplyr)

# Load training and validation data
# Training data includes burnt area points from 2010, 2014, and 2018
training_data <- readOGR("burnt_area_2010_2014_2018.shp")  # Replace with actual file path
# Validation data includes burnt area points from 2022
validation_data <- readOGR("burnt_area_2022.shp")  # Replace with actual file path

# Load raster datasets
ndvi_raster <- raster("ndvi_2010_2018.tif")  # NDVI average from 2010-2018
lulc_raster <- raster("lulc_2010_2018.tif")  # LULC from 2010-2018
temp_max_raster <- raster("temp_max_2010_2018.tif")  # Maximum temperature average from 2010-2018
temp_min_raster <- raster("temp_min_2010_2018.tif")  # Minimum temperature average from 2010-2018
wind_speed_raster <- raster("wind_speed_2010_2018.tif")  # Wind speed average from 2010-2018
rel_humidity_raster <- raster("rel_humidity_2010_2018.tif")  # Relative humidity average from 2010-2018

# Function to extract raster values at point locations
extract_raster_values <- function(points, raster_layer) {
  extract(raster_layer, points)
}

# Extract features for training data
training_data$NDVI <- extract_raster_values(training_data, ndvi_raster)
training_data$LULC <- extract_raster_values(training_data, lulc_raster)
training_data$Temp_Max <- extract_raster_values(training_data, temp_max_raster)
training_data$Temp_Min <- extract_raster_values(training_data, temp_min_raster)
training_data$Wind_Speed <- extract_raster_values(training_data, wind_speed_raster)
training_data$Rel_Humidity <- extract_raster_values(training_data, rel_humidity_raster)

# Extract features for validation data
# Using 2022-specific raster data for validation
validation_data$NDVI <- extract_raster_values(validation_data, raster("ndvi_2022.tif"))
validation_data$LULC <- extract_raster_values(validation_data, raster("lulc_2022.tif"))
validation_data$Temp_Max <- extract_raster_values(validation_data, raster("temp_max_2022.tif"))
validation_data$Temp_Min <- extract_raster_values(validation_data, raster("temp_min_2022.tif"))
validation_data$Wind_Speed <- extract_raster_values(validation_data, raster("wind_speed_2022.tif"))
validation_data$Rel_Humidity <- extract_raster_values(validation_data, raster("rel_humidity_2022.tif"))

# Prepare features (X) and labels (y)
X_train <- training_data %>% select(NDVI, LULC, Temp_Max, Temp_Min, Wind_Speed, Rel_Humidity) %>% as.matrix()
y_train <- training_data$burnt  # Replace with the actual column indicating burnt area

X_val <- validation_data %>% select(NDVI, LULC, Temp_Max, Temp_Min, Wind_Speed, Rel_Humidity) %>% as.matrix()
y_val <- validation_data$burnt  # Replace with the actual column indicating burnt area

# Train XGBoost model
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval <- xgb.DMatrix(data = X_val, label = y_val)
params <- list(objective = "binary:logistic", eval_metric = "logloss", eta = 0.1, max_depth = 6, nrounds = 100)
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100)

# Predict probabilities on validation data
val_probs <- predict(xgb_model, dval)
val_preds <- ifelse(val_probs > 0.5, 1, 0)

# Evaluate the model
accuracy <- sum(val_preds == y_val) / length(y_val)
cat("Validation Accuracy:", accuracy, "\n")

# Predict probabilities for the entire raster grid
raster_stack <- stack(raster("ndvi_2022.tif"), raster("lulc_2022.tif"), raster("temp_max_2022.tif"), 
                      raster("temp_min_2022.tif"), raster("wind_speed_2022.tif"), raster("rel_humidity_2022.tif"))
names(raster_stack) <- c("NDVI", "LULC", "Temp_Max", "Temp_Min", "Wind_Speed", "Rel_Humidity")

raster_values <- as.matrix(as.data.frame(values(raster_stack)))
raster_values[is.na(raster_values)] <- 0  # Handle NA values
raster_probs <- predict(xgb_model, xgb.DMatrix(data = raster_values))
probability_map <- raster_stack[[1]]
values(probability_map) <- raster_probs

# Save the probability map
writeRaster(probability_map, "wildfire_probability_map_2022_xgb.tif", format = "GTiff", overwrite = TRUE)

  Add a link to get support, GitHub status page, code of conduct, license link.
-->

---

Get help: [Post in our discussion board](https://github.com/orgs/skills/discussions/categories/code-with-copilot) &bull; [Review the GitHub status page](https://www.githubstatus.com/)

&copy; 2023 GitHub &bull; [Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/code_of_conduct.md) &bull; [MIT License](https://gh.io/mit)

</footer>
