# Covid-projection

This model is designed to facilitate hospital planning with estimates of the daily number of Intensive Care (IC) beds, Acute Care (AC) beds, and ventilators necessary to accommodate patients who require hospitalization for COVID-19 and how these compare to the available resources. To use the model, input estimates of the characteristics of the patient population and hospital capacity.

The first day of the simulation (Day 0) is fixed. For each subsequent day the model uses the projected number of new COVID-19 patients, partitions the patients into different cohorts, and updates the number of COVID-19 patients requiring IC and AC beds as follows:

COVID-19 Admissions are projected with exponential growth based on the inputs of the doubling time (the time it takes for the cumulative number of patients to double) and the initial number of patients. The patients and their length of stay are partitioned into 5 care cohorts each defined by the patient path through the hospital. These are based on inputs into the Length of Stay Parameters. The results are given as follows. The number of:

- IC beds required each day is the sum of the number of COVID+ and COVID- IC patients

- Patients to be cared for by the Medical Service each day is the sum of the number of COVID+

- AC patients and COVID- patients being cared for by the Medicine Service.

- AC patients and COVID- patients being cared for by the Medicine Service.

Ventilators required is estimated as the sum of 50% of non-COVID IC patients and 100% of COVID IC patients.

Definitions
Doubling time is defined by the amount of time it takes a population to double in size. In this case, assuming exponential growth in the number of COVID-19 cases, we are defining the doubling time as the number of days it takes for cases to double.


### How to run locally



### Install the following packages before running 




Please cite as Teng Zhang et al. A model to estimate bed demand for COVID-19 related hospitalization. medRxiv doi:https://doi.org/10.1101/2020.03.24.20042762

[Paper link](https://www.medrxiv.org/content/10.1101/2020.03.24.20042762v1)
