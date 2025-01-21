rm(list = ls())
load(url("https://drive.google.com/uc?export=download&id=1mlJAYmo9TszSJsbYSWhhOY1a3fTJB_Ko"))

# Q1 Booking #
listing_2016Q1_booking <- listing_2016Q1[listing_2016Q1$Status=="R",]
agg_booking_Q1 <- aggregate(Status~PropertyID, data=listing_2016Q1_booking, FUN=length)
colnames(agg_booking_Q1)[2] <- "BookingQ1"
property_info <- merge(x=property_info, y=agg_booking_Q1, by="PropertyID", all.x=TRUE)
property_info$BookingQ1[is.na(property_info$BookingQ1)] <- 0

# Q1 Blocked #
listing_2016Q1_blocked <- listing_2016Q1[listing_2016Q1$Status=="B",]
agg_blocked_Q1 <- aggregate(Status~PropertyID, data=listing_2016Q1_blocked, FUN=length)
colnames(agg_blocked_Q1)[2] <- "BlockedQ1"
property_info <- merge(x=property_info, y=agg_blocked_Q1, by="PropertyID", all.x=TRUE)
property_info$BlockedQ1[is.na(property_info$BlockedQ1)] <- 0

# Q2 Booking #
listing_2016Q2_booking <- listing_2016Q2[which(listing_2016Q2$Status=='R'),] 
agg_booking_Q2 <- aggregate(Status~PropertyID, data=listing_2016Q2_booking, FUN=length)
colnames(agg_booking_Q2)[2] <- "BookingQ2"
property_info <- merge(x=property_info, y=agg_booking_Q2, by='PropertyID', all.x=TRUE)
property_info$BookingQ2[which(is.na(property_info$BookingQ2))] <- 0


# Q2 Blocked #
listing_2016Q2_blocked <- listing_2016Q2[listing_2016Q2$Status=="B",]
agg_blocked_Q2 <- aggregate(Status~PropertyID, data=listing_2016Q2_blocked, FUN=length)
colnames(agg_blocked_Q2)[2] <- "BlockedQ2"
property_info <- merge(x=property_info, y=agg_blocked_Q2, by="PropertyID", all.x=TRUE)
property_info$BlockedQ2[is.na(property_info$BlockedQ2)] <- 0


### NO NAs ###
mean(is.na(property_info$NumberofReviews))
mean(is.na(property_info$MaxGuests))
mean(is.na(property_info$PublishedNightlyRate))
mean(is.na(property_info$MinimumStay))
mean(is.na(property_info$NumberofPhotos))
mean(is.na(property_info$BusinessReady))
mean(is.na(property_info$CancellationPolicy))

### Categorical NAs ###
mean(is.na(property_info$Neighborhood))  
mean(is.na(property_info$Superhost))   
mean(is.na(property_info$HostVerified))

#Fixing Categorical NAs
tb_neighborhood = table(property_info$Neighborhood)
rare_neighborhood = names(tb_neighborhood[tb_neighborhood<=20])
property_info$Neighborhood[property_info$Neighborhood %in% rare_neighborhood] = "rare neighborhood"
property_info$Neighborhood[is.na(property_info$Neighborhood)] = "unknown neighborhood"
property_info$Neighborhood[property_info$Neighborhood=="unknown neighborhood"] = "rare neighborhood"

property_info$Superhost[is.na(property_info$Superhost)] = "unknown host type"

property_info$HostVerified[is.na(property_info$HostVerified)] = "unknown host verified"


### Infrequent NAs ###
mean(is.na(property_info$Bedrooms)) 
mean(is.na(property_info$Bathrooms))   
mean(is.na(property_info$HostListings))
mean(is.na(property_info$PublishedWeeklyRate))
mean(is.na(property_info$PublishedMonthlyRate))

# Fixing Infrequent NAs
property_info$Bedrooms[is.na(property_info$Bedrooms)] = mean(property_info$Bedrooms,na.rm=TRUE)
property_info$Bathrooms[is.na(property_info$Bathrooms)] = mean(property_info$Bathrooms,na.rm=TRUE)
property_info$HostListings[is.na(property_info$HostListings)] = mean(property_info$HostListings,na.rm=TRUE)
property_info$PublishedWeeklyRate[is.na(property_info$PublishedWeeklyRate)] = mean(property_info$PublishedWeeklyRate,na.rm=TRUE)
property_info$PublishedMonthlyRate[is.na(property_info$PublishedMonthlyRate)] = mean(property_info$PublishedMonthlyRate,na.rm=TRUE)


# Train & Test #
property_info_train <- property_info[which(property_info$PropertyID %in% reserve_2016Q3_train$PropertyID),]
property_info_test <- property_info[which(property_info$PropertyID %in% PropertyID_test),]

property_info_train <- merge(property_info_train,reserve_2016Q3_train,on="PropertyID")


### Frequent NAs ###
mean(is.na(property_info$SecurityDeposit)) 
mean(is.na(property_info$OverallRating))
mean(is.na(property_info$AccuracyRating))
mean(is.na(property_info$CleanRating))
mean(is.na(property_info$CheckinRating))
mean(is.na(property_info$CommunicationRating))
mean(is.na(property_info$LocationRating))
mean(is.na(property_info$ValueRating))
mean(is.na(property_info$ResponseRate))
mean(is.na(property_info$ResponseTimemin))
mean(is.na(property_info$CleaningFee))
mean(is.na(property_info$ExtraPeopleFee))

#Fixing Frequent NAs
# Security Deposit #
lm_reg_w_secdep = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                       PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                       Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                       PublishedMonthlyRate + SecurityDeposit, data = property_info_train)
pred_secdep = predict(lm_reg_w_secdep, property_info)
lm_reg_wo_secdep = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                        PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                        Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                        PublishedMonthlyRate, data = property_info_train)
pred_wo_security_deposit = predict(lm_reg_wo_secdep, property_info)
mean(is.na(pred_wo_security_deposit))
pred_secdep[is.na(pred_secdep)] = pred_wo_security_deposit[is.na(pred_secdep)]
property_info$SecurityDeposit[is.na(property_info$SecurityDeposit)] = pred_secdep


# Overall Rating #
lm_reg_w_overall = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                        PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                        Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                        PublishedMonthlyRate + OverallRating, data = property_info_train)
pred_overall = predict(lm_reg_w_overall, property_info)
lm_reg_wo_overall = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                         PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                         Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                         PublishedMonthlyRate, data = property_info_train)
pred_wo_overall = predict(lm_reg_wo_overall, property_info)
mean(is.na(pred_wo_overall))
pred_overall[is.na(pred_overall)] = pred_wo_overall[is.na(pred_overall)]
property_info$OverallRating[is.na(property_info$OverallRating)] = pred_overall

# Accuracy Rating #
lm_reg_w_acc = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                    PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                    Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                    PublishedMonthlyRate + AccuracyRating, data = property_info_train)
pred_acc = predict(lm_reg_w_acc, property_info)
lm_reg_wo_acc = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                     PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                     Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                     PublishedMonthlyRate, data = property_info_train)
pred_wo_acc = predict(lm_reg_wo_acc, property_info)
mean(is.na(pred_wo_acc))
pred_acc[is.na(pred_acc)] = pred_wo_acc[is.na(pred_acc)]
property_info$AccuracyRating[is.na(property_info$AccuracyRating)] = pred_acc

# Clean Rating #
lm_reg_w_clean = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                      PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                      Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                      PublishedMonthlyRate + CleanRating, data = property_info_train)
pred_clean = predict(lm_reg_w_clean, property_info)
lm_reg_wo_clean = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                       PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                       Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                       PublishedMonthlyRate, data = property_info_train)
pred_wo_clean = predict(lm_reg_wo_clean, property_info)
mean(is.na(pred_wo_clean))
pred_clean[is.na(pred_clean)] = pred_wo_clean[is.na(pred_clean)]
property_info$CleanRating[is.na(property_info$CleanRating)] = pred_clean

# Checkin Rating #
lm_reg_w_checkin = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                        PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                        Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                        PublishedMonthlyRate + CheckinRating, data = property_info_train)
pred_checkin = predict(lm_reg_w_checkin, property_info)
lm_reg_wo_checkin = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                         PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                         Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                         PublishedMonthlyRate, data = property_info_train)
pred_wo_checkin = predict(lm_reg_wo_checkin, property_info)
mean(is.na(pred_wo_checkin))
pred_checkin[is.na(pred_checkin)] = pred_wo_checkin[is.na(pred_checkin)]
property_info$CheckinRating[is.na(property_info$CheckinRating)] = pred_checkin

# Communication Rating #
lm_reg_w_comm = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                     PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                     Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                     PublishedMonthlyRate + CommunicationRating, data = property_info_train)
pred_comm = predict(lm_reg_w_comm, property_info)
lm_reg_wo_comm = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                      PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                      Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                      PublishedMonthlyRate, data = property_info_train)
pred_wo_comm = predict(lm_reg_wo_comm, property_info)
mean(is.na(pred_wo_comm))
pred_comm[is.na(pred_comm)] = pred_wo_comm[is.na(pred_comm)]
property_info$CommunicationRating[is.na(property_info$CommunicationRating)] = pred_comm

# Location Rating #
lm_reg_w_loc = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                    PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                    Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                    PublishedMonthlyRate + LocationRating, data = property_info_train)
pred_loc = predict(lm_reg_w_loc, property_info)
lm_reg_wo_loc = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                     PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                     Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                     PublishedMonthlyRate, data = property_info_train)
pred_wo_loc = predict(lm_reg_wo_loc, property_info)
mean(is.na(pred_wo_loc))
pred_loc[is.na(pred_loc)] = pred_wo_loc[is.na(pred_loc)]
property_info$LocationRating[is.na(property_info$LocationRating)] = pred_loc

# Value Rating #
lm_reg_w_value = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                      PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                      Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                      PublishedMonthlyRate + ValueRating, data = property_info_train)
pred_value = predict(lm_reg_w_value, property_info)
lm_reg_wo_value = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                       PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                       Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                       PublishedMonthlyRate, data = property_info_train)
pred_wo_value = predict(lm_reg_wo_value, property_info)
mean(is.na(pred_wo_value))
pred_value[is.na(pred_value)] = pred_wo_value[is.na(pred_value)]
property_info$ValueRating[is.na(property_info$ValueRating)] = pred_value

# Cleaning Fee Rating #
lm_reg_w_cleaning = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                         PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                         Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                         PublishedMonthlyRate + CleaningFee, data = property_info_train)
pred_cleaning = predict(lm_reg_w_cleaning, property_info)
lm_reg_wo_cleaning = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 +  NumberofReviews + MaxGuests + 
                          PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                          Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                          PublishedMonthlyRate, data = property_info_train)
pred_wo_cleaning = predict(lm_reg_wo_cleaning, property_info)
mean(is.na(pred_wo_cleaning))
pred_cleaning[is.na(pred_cleaning)] = pred_wo_cleaning[is.na(pred_cleaning)]
property_info$CleaningFee[is.na(property_info$CleaningFee)] = pred_cleaning

# Extra People Fee Rating #
lm_reg_w_extra = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                      PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                      Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                      PublishedMonthlyRate + ExtraPeopleFee, data = property_info_train)
pred_extra = predict(lm_reg_w_extra, property_info)
lm_reg_wo_extra = lm(NumReserveDays2016Q3 ~ BlockedQ1 + BlockedQ2 + BookingQ1 + BookingQ2 + NumberofReviews + MaxGuests + 
                       PublishedNightlyRate + MinimumStay + NumberofPhotos + BusinessReady  + Neighborhood + 
                       Superhost + HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + 
                       PublishedMonthlyRate, data = property_info_train)
pred_wo_extra = predict(lm_reg_wo_extra, property_info)
mean(is.na(pred_wo_extra))
pred_extra[is.na(pred_extra)] = pred_wo_extra[is.na(pred_extra)]
property_info$ExtraPeopleFee[is.na(property_info$ExtraPeopleFee)] = pred_extra

# Linear Regression RMSE = 15.6997
property_info <- merge(x=property_info, y=reserve_2016Q3_train, by="PropertyID", all.x=TRUE)

all_lm = lm(NumReserveDays2016Q3 ~ NumberofReviews + MaxGuests + PublishedNightlyRate + Neighborhood + 
              MinimumStay + NumberofPhotos + BusinessReady + CancellationPolicy + Superhost + 
              HostVerified + Bedrooms + HostListings + PublishedWeeklyRate + PublishedMonthlyRate + Bathrooms + 
              SecurityDeposit + OverallRating + AccuracyRating + CommunicationRating + LocationRating + 
              ValueRating + CleanRating + CheckinRating + CleaningFee + ExtraPeopleFee, data = property_info)
pred_lm = predict(all_lm, data=property_info)
rmse_lm = sqrt(mean((property_info_train$NumReserveDays2016Q3-pred_lm)^2,na.rm=TRUE))
rmse_lm

## GBM - RMSE = 15.2
library(gbm)
library(mlbench)

set.seed(123)

property_info_train <- property_info[which(property_info$PropertyID %in% reserve_2016Q3_train$PropertyID),]
property_info_test <- property_info[which(property_info$PropertyID %in% PropertyID_test),]

property_info_train <- merge(property_info_train,reserve_2016Q3_train,on="PropertyID")


train_indices <- sample(1:nrow(property_info_train), 0.7*nrow(property_info_train))
boost_property_info_train <- property_info_train[train_indices, ]
boost_property_info_test <- property_info_train[-train_indices, ]


gbm <- gbm(NumReserveDays2016Q3 ~ BookingQ2 + BookingQ1 + BlockedQ1 + BlockedQ2 + SecurityDeposit + NumberofReviews,
          data = boost_property_info_train,
          distribution = "gaussian",  
          n.trees = 200,             # Number of trees
          shrinkage = 0.07,          # Learning rate
          interaction.depth = 15,      # Interaction depth
          cv.folds = 1,               # Cross-validation     
          n.minobsinnode = 7,        # Minimum observations per node 
          verbose = TRUE)


pred <- predict(gbm, boost_property_info_test, n.trees = gbm$n.trees)
#save(pred, file = "AAS.rdata")

# Calculate RMSE for the test data
gbm_rmse <- sqrt(mean((boost_property_info_test$NumReserveDays2016Q3 - pred)^2, na.rm = TRUE))
print(paste("RMSE of gbm: ", gbm_rmse))

property_info$NumReserveDays2016Q3[is.na(property_info$NumReserveDays2016Q3)] = pred
summary(gbm)
length(pred)


library(gbm)
library(caret)


n.trees_options <- c(100, 200, 300, 400, 500, 1000)
depth_options <- c(3, 5, 7, 10, 15, 20)
shrinkage_options <- c(0.005, 0.01, 0.03, 0.07, 0.1)
minobsnode_options <- c(5, 7, 10, 15, 20, 30)

# Result DF
results <- expand.grid(n.trees = n.trees_options,
                       depth = depth_options,
                       shrinkage = shrinkage_options,
                       minobsnode = minobsnode_options)
results$RMSE <- numeric(length(n.trees_options) * length(depth_options) * length(shrinkage_options) * length(minobsnode_options))

# Grid Search
row_number <- 1
for (n.trees in n.trees_options) {
  for (depth in depth_options) {
    for (shrinkage in shrinkage_options) {
      for (minobsnode in minobsnode_options) {
        set.seed(123)
        model <- gbm(NumReserveDays2016Q3 ~ BookingQ2 + BookingQ1 + BlockedQ1 + BlockedQ2 + SecurityDeposit + NumberofReviews,
                     data = boost_property_info_train,
                     distribution = "gaussian",
                     n.trees = n.trees,
                     interaction.depth = depth,
                     shrinkage = shrinkage,
                     n.minobsinnode = minobsnode,
                     cv.folds = 1,
                     verbose = TRUE)
        
        # Predict
        predictions <- predict(model, boost_property_info_test, n.trees = n.trees)
        actual <- boost_property_info_test$NumReserveDays2016Q3
        RMSE <- sqrt(mean((predictions - actual)^2))
        
        # Results number
        results[row_number, "RMSE"] <- RMSE
        row_number <- row_number + 1
      }
    }
  }
}

# Get the best parameters
best_params <- results[which.min(results$RMSE), ]
print(best_params)
results

model2 <- gbm(NumReserveDays2016Q3 ~ BookingQ2 + BookingQ1 + BlockedQ1 + BlockedQ2 + SecurityDeposit + NumberofReviews,
             data = boost_property_info_train,
             distribution = "gaussian",
             n.trees = results[which.min(results$RMSE), 1],
             interaction.depth = results[which.min(results$RMSE), 2],
             shrinkage = results[which.min(results$RMSE), 3],
             n.minobsinnode = results[which.min(results$RMSE), 4],
             cv.folds = 1,
             verbose = TRUE)
predictions2 <- predict(model2, boost_property_info_test, n.trees = results[which.min(results$RMSE), 1])
actual2 <- boost_property_info_test$NumReserveDays2016Q3
RMSE2 <- sqrt(mean((predictions2 - actual2)^2))
RMSE2
