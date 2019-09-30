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

Each WiFi fingerprint can be characterized by the detected Wireless Access Points (WAPs) and the corresponding Received Signal Strength Intensity (RSSI). The intensity values are represented as negative integer values ranging from -104 dBm (extremely poor signal) to 0 dBm. The positive value 100 is used to denote when a WAP was not detected. During the database creation, 520 different WAPs were detected. Thus, the WiFi fingerprint is composed by 520 intensity values.

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
