# WiFi Indoor Positioning
Evaluate techniques for indoor user localization using WiFi Fingerprints

## Overview
In this project we are working for a client that is developing a indoor positioning system 
to be deployed on large industrial campuses, in shopping malls etc. to help people navigate
complex, unfamiliar interior space without getting lost. Our client would like us to to investigate
the feasibility to use "WiFi Fingerprinting" to determine a person's location in indoor spaces

Our goal is to apply machine learning techniques to predict the location of a user in a campus,
based on the intensity of the signals received by different wireless access points (WAPs),
obtained from users' mobile devices.

## Dataset
The dataset in this project is the [UJIIndoorLoc](https://archive.ics.uci.edu/ml/datasets/ujiindoorloc) 
dataset provided by UC Irvine Machine Learning Repository. This dataset covers three buildings of 
Universitat Jaume I, with 4 or 5 floor depending on the building and covers a surface of almost 110 000 m<sup>2</sup>.

The database consists of 2 files, one containing 19 937 training/reference records ```trainingData.csv``` and other containing 1111 validation/test records ```validationData.csv```. Both sets are composed of 529 attributes and are described below.

Each WiFi fingerprint can be characterized by the detected Wireless Access Points (WAPs) and the corresponding Received Signal Strength Intensity (RSSI). The intensity values are represented as negative integer values ranging from -104 dBm (extremely poor signal) to 0 dBm. The unit **dBm** stands for **decibels relative to a milliwatt**, and is a power ratio that is expressed on a logaritmic scale. The positive value 100 is used to denote when a WAP was not detected. During the database creation, 520 different WAPs were detected. Thus, the WiFi fingerprint is composed by 520 intensity values.

#### Attribute information:

| | |
|-|-|
<sub> Attribute 001 (WAP001): </sub> | <sub> Intensity value for WAP001. Negative integer values from -104 to 0 and +100. <br /> Positive value 100 used if WAP001 was not detected. </sub>|
<br />...<br />
<sub> Attribute 520 (WAP520):</sub> | <sub> Intensity value for WAP520. Negative integer values from -104 to 0 and +100. <br /> Positive Value 100 used if WAP520 was not detected. </sub>|
<sub> Attribute 521 (Longitude): </sub>| <sub> Longitude. Negative real values from -7695.9387549299299000 to -7299.786516730871000 </sub>|
<sub> Attribute 522 (Latitude): </sub> | <sub> Latitude. Positive real values from 4864745.7450159714 to 4865017.3646842018. </sub> |
<sub> Attribute 523 (Floor): </sub> | <sub> Altitude in floors inside the building. Integer values from 0 to 4. </sub> |
<sub> Attribute 524 (BuildingID): </sub> | <sub> ID to identify the building. Measures were taken in three different buildings. Categorical integer values from 0 to 2. </sub> |
<sub> Attribute 525 (SpaceID): </sub>| <sub> Internal ID number to identify the Space (office, corridor, classroom) where the capture was taken. Categorical integer values. </sub>|
<sub> Attribute 526 (RelativePosition): </sub> | <sub> Relative position with respect to the Space (1 - Inside, 2 - Outside in Front of the door). Categorical integer values. </sub>|
<sub> Attribute 527 (UserID): </sub> | <sub> User identifier (see below). Categorical integer values. </sub> |
<sub> Attribute 528 (PhoneID):</sub> | <sub> Android device identifier (see below). Categorical integer values. </sub>|
<sub> Attribute 529 (Timestamp): </sub> | <sub> UNIX Time when the capture was taken. Integer value. </sub> |

<br />

Below you can see the distribution of the received signal strengths for detected records in the training set.

<img src="https://github.com/EirikEspe/WiFi-Indoor-Positioning/blob/master/plot_distribution_of_signal_strength.png" width = "600">

## Data preparation 

To get accurate predictive models, we have to get our data in good shape for analysis. In order to get the data more
consistent and remove noise, I have applied the following steps:

- **Remove WAPs with no detected signal:** Columns where no signals were detected, i.e. columns where all signal strengths displayed a positive value of 100, were removed. When all values in the column has the same value, this makes the predictor uninformative and not useful to make predictions.
- **Remove records with no detected signal:** Similarly, it is hard to learn from records where no signals where detected. Therefore, rows with no detected signals were removed.
- **Convert values for signals that were not detected:** In the dataset a positive value of 100 was used if a WAP was not detected. When the WiFi signal strengths are given with values between 0 and -104, this will confuse the model. The values for no detected signals were converted from +100 to -105.
- **Remove outliers:** Records(rows) with signal strength higher than -30 dBm were removed from the dataset. In general a signal strength of -30 dBm is considered a max achievable signal strength. With this signal the user can only be a few feet from the AP to achieve this. Not typical or desirable in the real world. Based on this information, I have removed records that contained signal strengths higher than -30 dBm, as I do not want to train my model on these records.
- **Normalize by row:**  In order to make a prediction model, it is preferable to have input that are on the same scale. 
In this case all the signal strengths are measured on the same scale, buth the magnitude ranges from -30 dBm to -105dBm. To prevent these magnitude differences biasing our model, I have applied normalization by row (min-max scaling).
- **Remove WAPs that were creating interference:** In cases where WAPs were providing signals to several buildings, these WAPs were removed from the training model. 


---
## Results

### Building
The first step in the creation of training models was to predict which building a user was located in, based on the signal strength received by the WAPs. With this input my training models achieved these performance metrics on the validation set: 

| Algorithm | Accuracy | Kappa  |
| --------- | :------: | :----: |
| 1st kNN   | 97.48 %  | 0.9604 |
| 2nd kNN   | 99.28 %  | 0.9886 |
| Best SVM  | 100 %    |   1    |

### Floors

For the predictive models for floors, I started out with creating models that looked at all buildings in the prediction of which floor a user was located in. These models got the following performance metrics on the validation set:

| Algorithm | Accuracy | Kappa  |
| --------- | :------: | :----: |
| 1st kNN   | 69.31 %  | 0.5919 |
| Best kNN  | 90.91 %  | 0.8733 |
| Best SVM  | 92,71 %  | 0.8984 |
| Best GBM  | 93.88 %  | 0.9137 |

The 1st kNN model was created on un-processed data. The other models were created with the pre-processing outlined in the *Data Preparation* section. As you can see, the  predictions were not horrible, with almost 94 % correct classifications with the gradient boosting machine (GBM) model. However, it is possible to improve our results by zooming in on the individual buildings.

Using the predictions for building, we could filter by building and optimize our model per building. With predictive models for floor per building, the following performance metrics was obtained:
#### Floors - per building

|     Algorithm        | Accuracy | Kappa  |
|    -----------       | :------: | :----: |
|**Floor - Building 0**|          |        |
| Best RF              | 96.45 %  | 0.9498 |
| Best kNN             | 97.39 %  | 0.9631 |
| Best SVM             | 97,76 %  | 0.9683 |
| Best GBM             | 96.08 %  | 0.9445 |
|                      |          |        |
|**Floor - Building 1**|          |        |
| Best kNN             | 78.50 %  | 0.6910 |
| Best SVM             | 89.25 %  | 0.8409 |
| Best GBM             | 93.16 %  | 0.8983 |
|                      |          |        |
|**Floor - Building 2**|          |        |
| Best kNN             | 92.54 %  | 0.8986 |
| Best SVM             | 94.40 %  | 0.9238 |
| Best GBM             | 96.64 %  | 0.9543 |

<br />

|   Combining the best models for floor  per building        | Accuracy | Kappa  |
|  ---------------------------------------------------       | :------: | :----: |
|SVM for Building 0 + GBM for Building 1 + GBM for Building 2| 96.22 %  |  0.947 |

Zooming in on the individual buildings I got 96.22 % correct classifications for floors.

### Longitude

To locate a user we also need to find their position inside the building. To do this, we can use longitude and latitude.

In the prediction of longitude, our performance measures changes from classification based metrics to regression based metrics. This follows from that we now will focus on the position of the users inside the campus. We are moving from measuring the percentage of correct classifications, to measure how far our predicted positions differ from the actual position of the users.  
I use MAE (mean absolute error) as the main performance metric, as I am interested in the average magnitude of the errors to get a picture of their size. Using MAE, the individual errors get an equal weight, in contrast to RMSE where larger errors are penalized.

The following metrics were obtained for longitude:

| Algorithm |    MAE   | R<sup>2<sup/>  |  RMSE  |
| --------- | :------: |     :----:     | :----: |
| 1st kNN   |  13.561  |     94.30 %    | 28.929 |
| Best RF   |   6.692  |     99.40 %    |  9.456 |
| Best GBM  |   9.331  |     98.96 %    | 12.349 |
| Best kNN  |   5.467  |     99.56 %    |  8.033 |
  
As we can see, I got the best performance with the kNN algorithm.

### Latitude

To locate a user we also need to find their latitude. Combining longitude and latitude, we can pinpoint a user's position inside the building. In the table below you can find my performance metrics for latitude.

| Algorithm |    MAE   | R<sup>2<sup/>  |  RMSE  |
| --------- | :------: |     :----:     | :----: |
| 1st kNN   |  10.799  |     91.36 %    | 20.819 |
| Best RF   |   6.105  |     98.51 %    |  8.989 |
| Best GBM  |   8.637  |     97.49 %    | 11.715 |
| Best kNN  |   5.127  |     98.83 %    |  7.645 |
  
The best performing algorithm for my latitude prediction was kNN.

Now we can plot the predictions and compare against the actual positions.

<img src="https://github.com/EirikEspe/WiFi-Indoor-Positioning/blob/master/plot_predicted_vs_actual_long_lat.png" width = "700">
