import pandas as pd
import numpy as np
import datetime

'''
To do
 - initialze the model with csv from Sedj
 - random parameters (los)
'''

# column name
COL_PARAMETER_NAME = 'Parameter'
COL_PARAMETER_VALUE = 'Value'
COL_DAY = 'Day'
COL_DATE = 'Date'
COL_UNIT = 'Unit'
COL_CENSUS = 'Census'
COL_CENSUS_ICU_COVID = 'ICU COVID Census'
COL_CENSUS_ICU_NONCOVID = 'ICU Non-COVID Census'
COL_CENSUS_FLOOR_COVID = 'Floor COVID Census'
COL_CENSUS_FLOOR_NONCOVID = 'Floor Non-COVID Census'
COL_ICU_CAP_DAYS = 'ICU_Cap_Days'
COL_FLOOR_CAP_DAYS = 'Floor_Cap_Days'
COL_VENTILATOR_CAP_DAYS = 'Ventilator_Cap_Days'
COL_DOUBLING_TIME = 'Doubling_Time'
COL_DAILY_NEW_COVID_ADMISSION = 'New COVID Admit'
COL_CUM_COVID_ADMISSION = 'Total COVID Admit'
COL_GEN_FLOOR_CAPACITY = 'Gen Med Capacity'

# input datafram columns
INPUT_COL_DATE = 'Date'
INPUT_COL_ICU_COVID = 'ICU_COVID_Census'
INPUT_COL_ICU_NONCOVID = 'ICU_non_COVID_Census'
INPUT_COL_FLOOR_COVID = 'Floor_COVID_Census'
INPUT_COL_FLOOR_NONCOVID = 'Floor_non_COVID_Census'

# define the parametrized model input

# define the patient characterstics

# define the state variable


# add the loop of state transition,
# '''
# infection
# admission
# transition
# discharge
# '''


# within the loop, update the census and other monitors

'''
Patient cohort
floor	                  70.4%	5			5
floor -> icu -> Floor	  13.0%	4	9	4	17
floor -> icu	          1.8%	6	9		15
icu -> floor	          13.0%		9	4	13
icu	                      1.8%		11		11
'''

COHORT_PATHS = {0 : ['floor'],
                1: ['floor_1', 'icu', 'floor_2'],
                2: ['floor', 'icu'],
                3: ['icu', 'floor'],
                4: ['icu']
                }

MAX_SIMULATION_DAYS = 100
# MAX_REMAINING_LOS = 20
STARTING_DAY = datetime.date(year = 2020, month = 3, day = 13)



class DES_Simulator():
    '''
    Discrete Event Simulation main body
    '''
    def __init__(self, n_days = 10,
                    starting_total = 10,
                    doubling_time = 7,
                    cohort_fraction = [0.704, 0.13, 0.018, 0.13, 0.018],
                    los_matrix = [[5, 0, 0],
                                  [4, 9, 4],
                                  [6, 9, 0],
                                  [0, 9, 4],
                                  [0, 11, 0]],
                    gen_med_delta_capacity = 10):
        # number of total days in the simulation, all starting from STARTING_DAY
        self.n_input_days = 1
        self.n_days = n_days + self.n_input_days

        # census tracker
        self.icu_covid_census = None
        self.icu_noncovid_census = None
        self.floor_covid_census = None
        self.floor_noncovid_census = None

        # capcacity ch_tracker
        self.gen_med_capacity = None
        self.gen_med_delta_capacity = gen_med_delta_capacity

        # cohort tracker, ch = cohort
        # e.g. sh_tracker[(i, 'icu')] is a list of patient type i in icu, with ind:value = remain_los: n_patients
        self.ch_tracker = {}

        # parameters for new admission generator
        self.starting_total = starting_total
        self.doubling_time = doubling_time

        # parameters for patient cohort
        self.cohort_fraction = cohort_fraction
        self.num_cohorts = len(self.cohort_fraction)

        # parameters for los
        self.los = los_matrix
        self.num_stays = len(self.los[0])
        self.max_los = np.max(self.los)

        # parameter for simulation start day index


    def state_init(self, icu_census_covid_0 = 1, floor_census_covid_0 = 1,
                        icu_census_noncovid_mean = 67, floor_census_noncovid_mean = 86,
                        df_input_census = None,
                        gen_med_capacity_0 = 100 # starting capcacity of the floor
                        ):

        # df_input_census = pd.read_csv('input_census.csv')
        # # print(df_input_census.shape)
        # # print(df_input_census.columns)
        if df_input_census is not None:
            # initialze the census given the input
            df_input_census[COL_DAY] = np.arange(len(df_input_census))
            # only select the non empty rows
            df_input_census = df_input_census.loc[~df_input_census[INPUT_COL_ICU_COVID].isna()]
            self.n_input_days = len(df_input_census)
            self.n_days += (self.n_input_days-1) # current days + projected days
            # print(self.n_input_days, self.n_days)
            self.icu_covid_census = np.zeros(self.n_days)
            self.floor_covid_census = np.zeros(self.n_days)
            self.icu_noncovid_census = np.ones(self.n_days) * icu_census_noncovid_mean
            self.floor_noncovid_census = np.ones(self.n_days) * floor_census_noncovid_mean
            self.gen_med_capacity = np.ones(self.n_days) * gen_med_capacity_0

            for _, row in df_input_census.iterrows():
                day = row[COL_DAY]
                self.icu_covid_census[day] = row[INPUT_COL_ICU_COVID]
                self.floor_covid_census[day] = row[INPUT_COL_FLOOR_COVID]
                self.icu_noncovid_census[day] = row[INPUT_COL_ICU_NONCOVID]
                self.floor_noncovid_census[day] = row[INPUT_COL_FLOOR_NONCOVID]

            # up the the initial census by the last row
            icu_census_covid_0, floor_census_covid_0 = row[INPUT_COL_ICU_COVID], row[INPUT_COL_FLOOR_COVID]
        else:
            self.icu_covid_census = np.zeros(self.n_days)
            self.floor_covid_census = np.zeros(self.n_days)
            self.icu_noncovid_census = np.ones(self.n_days) * icu_census_noncovid_mean
            self.floor_noncovid_census = np.ones(self.n_days) * floor_census_noncovid_mean
            self.gen_med_capacity = np.ones(self.n_days) * gen_med_capacity_0
            self.icu_covid_census[0] = icu_census_covid_0
            self.floor_covid_census[0] = floor_census_covid_0

        # initialized by evenly distribution
        # calculate the allocation fraction according to los and fraction
        all_fractions = np.array(self.los)
        all_fractions = np.array(self.cohort_fraction).reshape([len(all_fractions), 1]) * all_fractions
        all_fractions = all_fractions / np.sum(all_fractions, axis = 0)
        # print(all_fractions)

        for pa_ind in range(self.num_cohorts):
            counters = np.zeros([self.max_los + 1, self.num_stays])
            for col_ind in range(2):
                fraction = all_fractions[pa_ind, col_ind]
                if fraction > 0 and col_ind == 0:
                    los = self.los[pa_ind][col_ind]
                    counters[:los, col_ind] = floor_census_covid_0 * fraction / los
                elif fraction > 0 and col_ind == 1:
                    los = self.los[pa_ind][col_ind]
                    counters[:los, col_ind] = icu_census_covid_0 * fraction / los
            # print(pa_ind, counters)
            # print(np.sum(counters, axis = 0))
            self.ch_tracker[pa_ind] = counters


    def run(self):

        for day in range(self.n_input_days, self.n_days): # start from the next input day

            # update each cohort tracker
            self.update_ch_tracker()

            # generate new arrivals
            new_patients = self.generate_new_admission(day)
            self.patient_admission(new_patients)

            # update the census tracker
            self.update_census(day)

            # update the capcacity
            self.update_capacity(day)

            # print(day, self.ch_tracker[0])

        return None

    def run_till_cap(self, icu_cap, floor_cap, vent_cap, vent_percent):

        is_icu_cap_hit = False
        is_floor_cap_hit = False
        is_vent_cap_hit = False

        days_icu = self.n_days
        days_floor = self.n_days
        days_vent = self.n_days

        for day in range(self.n_input_days, self.n_days):

            # update each cohort tracker
            self.update_ch_tracker()

            # generate new arrivals
            new_patients = self.generate_new_admission(day)
            self.patient_admission(new_patients)

            # update the census tracker
            self.update_census(day)

            if self.icu_covid_census[day] + self.icu_noncovid_census[day] > icu_cap:
                if not is_icu_cap_hit: # only update when first time
                    is_icu_cap_hit = True
                    days_icu = day
            if self.floor_covid_census[day] + self.floor_noncovid_census[day] > floor_cap:
                if not is_floor_cap_hit:
                    is_floor_cap_hit = True
                    days_floor = day
            if self.icu_covid_census[day] + self.icu_noncovid_census[day] * vent_percent > vent_cap:
                if not is_vent_cap_hit:
                    is_vent_cap_hit = True
                    days_vent = day
            if is_icu_cap_hit and is_floor_cap_hit and is_vent_cap_hit:
                # print([days_icu, days_floor])
                return [days_icu, days_floor, days_vent]
                # days_icu = min(days_icu)

            # print(day, self.ch_tracker[0])
            # print(day)
        # print(is_icu_cap_hit, is_floor_cap_hit)
        return [days_icu, days_floor, days_vent]


    def update_ch_tracker(self):
        # # patient remaininig los minus 1
        # for pa_ind, counts in self.ch_tracker.items():
        #     self.ch_tracker[pa_ind] = np.concatenate((counts[1:,:], [[0]*len(counts[0])]), axis = 0)

        # transfers and dispatches
        for pa_ind, counts in self.ch_tracker.items():
            for col_ind in range(self.num_stays - 1):
                counts[self.los[pa_ind][col_ind+1], col_ind+1] += counts[0, col_ind]
                counts[0, col_ind] = 0
                # counts[0, col_ind], counts[self.los[pa_ind][col_ind+1], col_ind+1] = 0, counts[0, col_ind]
            # discharge from the last unit
            counts[0, col_ind+1] = 0
            self.ch_tracker[pa_ind] = counts

        # patient remaininig los minus 1
        for pa_ind, counts in self.ch_tracker.items():
            self.ch_tracker[pa_ind] = np.concatenate((counts[1:,:], [[0]*len(counts[0])]), axis = 0)
        return None

    def generate_new_admission(self, day):
        # assume day > 0
        total_admission = self.starting_total * 2**((day - 1)/self.doubling_time) * (2**(1/self.doubling_time)-1)
        # print('total patient day', day, total_admission)
        new_patients = {}
        for pa_ind in range(self.num_cohorts):
            new_patients[pa_ind] = total_admission * self.cohort_fraction[pa_ind]
        return new_patients

    def patient_admission(self, new_patients):
        for pa_ind in range(self.num_cohorts):
            # print(new_patients)
            counts, new_admission, los_list = self.ch_tracker[pa_ind], new_patients[pa_ind], self.los[pa_ind]
            for j in range(self.num_stays):
                if los_list[j] > 0:
                    break # find the first unit that has 0 los
            counts[los_list[j]-1, j] += new_admission
            self.ch_tracker[pa_ind] = counts
        return None

    def update_census(self, day):
        for pa_ind in range(self.num_cohorts):
            self.icu_covid_census[day] += np.sum(self.ch_tracker[pa_ind][:, 1])
            self.floor_covid_census[day] += np.sum(self.ch_tracker[pa_ind])
        self.floor_covid_census[day] -= self.icu_covid_census[day]
        return None

    def update_capacity(self, day):
        gen_med_cap_current = self.gen_med_capacity[day]
        if self.floor_covid_census[day] + self.floor_noncovid_census[day] >= gen_med_cap_current - 1:
            self.gen_med_capacity[day+1:] = gen_med_cap_current + self.gen_med_delta_capacity
        return None

    def save_census(self, dir_name = 'result_table'):
        df_result = pd.DataFrame()
        df_result[COL_DAY] = range(self.n_days)
        df_result[COL_CENSUS_ICU_COVID] = self.icu_covid_census
        df_result[COL_CENSUS_ICU_NONCOVID] = self.icu_noncovid_census
        df_result[COL_CENSUS_FLOOR_COVID] = self.floor_covid_census
        df_result[COL_CENSUS_FLOOR_NONCOVID] = self.floor_noncovid_census
        df_result[COL_GEN_FLOOR_CAPACITY] = self.gen_med_capacity
        print(df_result)
        # df_result.to_csv(dir_name + '.csv')
        return df_result


def run_simulation(n_days = 10,
                   df_input_census = None,
                   icu_census_covid_0 = 2,
                   floor_census_covid_0 = 2,
                   icu_census_noncovid_mean = 61,
                   floor_census_noncovid_mean = 70,
                   starting_total = 11,
                   doubling_time = 6,
                   cohort_fraction_0 = 0.704,
                   cohort_fraction_1 = 0.13,
                   cohort_fraction_2 = 0.018,
                   cohort_fraction_3 = 0.13,
                   cohort_fraction_4 = 0.018,
                   los_matrix_0_0 = 5, los_matrix_0_1 = 0, los_matrix_0_2 = 0,
                   los_matrix_1_0 = 4, los_matrix_1_1 = 9, los_matrix_1_2 = 4,
                   los_matrix_2_0 = 6, los_matrix_2_1 = 9, los_matrix_2_2 = 0,
                   los_matrix_3_0 = 0, los_matrix_3_1 = 9, los_matrix_3_2 = 4,
                   los_matrix_4_0 = 0, los_matrix_4_1 = 11, los_matrix_4_2 = 0,
                   gen_med_capacity_0 = 100, # starting capcacity of the floor
                   gen_med_delta_capacity = 10 # when the the census hits capacity, how much capcacity is added
                   # cohort_fraction = [0.704, 0.13, 0.018, 0.13, 0.13],
                   # los_matrix = [[5, 0, 0],
                   #               [4, 9, 4],
                   #               [6, 9, 0],
                   #               [0, 9, 4],
                   #               [0, 11, 0]]
                   ):
    cohort_fraction = [cohort_fraction_0, cohort_fraction_1, cohort_fraction_2, cohort_fraction_3, cohort_fraction_4]
    los_matrix = [[los_matrix_0_0, los_matrix_0_1, los_matrix_0_2],
                  [los_matrix_1_0, los_matrix_1_1, los_matrix_1_2],
                  [los_matrix_2_0, los_matrix_2_1, los_matrix_2_2],
                  [los_matrix_3_0, los_matrix_3_1, los_matrix_3_2],
                  [los_matrix_4_0, los_matrix_4_1, los_matrix_4_2]]

    simulator = DES_Simulator(n_days,
                              starting_total,
                              doubling_time,
                              cohort_fraction,
                              los_matrix,
                              gen_med_delta_capacity)

    # print(icu_census_noncovid_mean)
    simulator.state_init(icu_census_covid_0, floor_census_covid_0,
                         icu_census_noncovid_mean, floor_census_noncovid_mean,
                         df_input_census,
                         gen_med_capacity_0)
    simulator.run()
    df_result = simulator.save_census()

    return df_result


def sensitivity_calculation(
                    # n_days = 10,
                   df_input_census = None,
                   icu_census_covid_0 = 1, floor_census_covid_0 = 2,
                   icu_census_noncovid_mean = 61, floor_census_noncovid_mean = 70,
                   starting_total = 10,
                   doubling_time = 6,
                   cohort_fraction_0 = 0.704,
                   cohort_fraction_1 = 0.13,
                   cohort_fraction_2 = 0.018,
                   cohort_fraction_3 = 0.13,
                   cohort_fraction_4 = 0.018,
                   los_matrix_0_0 = 5, los_matrix_0_1 = 0, los_matrix_0_2 = 0,
                   los_matrix_1_0 = 4, los_matrix_1_1 = 9, los_matrix_1_2 = 4,
                   los_matrix_2_0 = 6, los_matrix_2_1 = 9, los_matrix_2_2 = 0,
                   los_matrix_3_0 = 0, los_matrix_3_1 = 9, los_matrix_3_2 = 4,
                   los_matrix_4_0 = 0, los_matrix_4_1 = 9, los_matrix_4_2 = 0,
                   icu_capacity = 100,
                   floor_capacity = 220,
                   ventilator_capacity = 75,
                   icu_non_covid_ventilator_percentage = 0.5,
                   # gen_med_capacity_0 = 100, # starting capcacity of the floor
                   # gen_med_delta_capacity = 10 # when the the census hits capacity, how much capcacity is added
                   ):

    '''
    Output: list of dataframes in the order:
        [df_base, df_doubling_time,
        df_los_minus_icu, df_los_minus_floor, df_los_minus_vent,
        df_los_plus_icu, df_los_plus_floor, df_los_plus_vent]
    '''
    # parameter merge
    cohort_fraction = [cohort_fraction_0, cohort_fraction_1, cohort_fraction_2, cohort_fraction_3, cohort_fraction_4]
    los_matrix = [[los_matrix_0_0, los_matrix_0_1, los_matrix_0_2],
                  [los_matrix_1_0, los_matrix_1_1, los_matrix_1_2],
                  [los_matrix_2_0, los_matrix_2_1, los_matrix_2_2],
                  [los_matrix_3_0, los_matrix_3_1, los_matrix_3_2],
                  [los_matrix_4_0, los_matrix_4_1, los_matrix_4_2]]

    # base case
    simulator = DES_Simulator(MAX_SIMULATION_DAYS,
                              starting_total,
                              doubling_time,
                              cohort_fraction,
                              los_matrix)
    simulator.state_init(icu_census_covid_0, floor_census_covid_0,
                         icu_census_noncovid_mean, floor_census_noncovid_mean,
                         df_input_census)
    cap_days = simulator.run_till_cap(icu_capacity, floor_capacity, ventilator_capacity,
                                        icu_non_covid_ventilator_percentage)
    df_base = pd.DataFrame([cap_days], columns = [COL_ICU_CAP_DAYS, COL_FLOOR_CAP_DAYS, COL_VENTILATOR_CAP_DAYS])
    # print(df_base)
    # df_base.to_csv('sensitivity_base.csv')

    # sensitivity of doubling time
    df_doubling_time = pd.DataFrame(columns = [COL_DOUBLING_TIME, COL_ICU_CAP_DAYS, COL_FLOOR_CAP_DAYS])
    dt_set = [doubling_time/2, doubling_time, doubling_time*2]
    df_doubling_time[COL_DOUBLING_TIME] = np.array(dt_set)
    for dt in dt_set:
        simulator = DES_Simulator(MAX_SIMULATION_DAYS,
                                  starting_total,
                                  dt,
                                  cohort_fraction,
                                  los_matrix)
        simulator.state_init(icu_census_covid_0, floor_census_covid_0,
                             icu_census_noncovid_mean, floor_census_noncovid_mean,
                             df_input_census)
        cap_days = simulator.run_till_cap(icu_capacity, floor_capacity, ventilator_capacity,
                                            icu_non_covid_ventilator_percentage)
        df_doubling_time.loc[df_doubling_time[COL_DOUBLING_TIME] == dt, COL_ICU_CAP_DAYS] = cap_days[0]
        df_doubling_time.loc[df_doubling_time[COL_DOUBLING_TIME] == dt, COL_FLOOR_CAP_DAYS] = cap_days[1]
        df_doubling_time.loc[df_doubling_time[COL_DOUBLING_TIME] == dt, COL_VENTILATOR_CAP_DAYS] = cap_days[2]
    # print(df_doubling_time)
    # df_doubling_time.to_csv('sensitivity_dt.csv')

    # sensitivity los
    perturbations_set = [-1, +1]
    file_name_set = ['minus', 'plus']
    result_list = [df_base, df_doubling_time]
    for perturb, file_name in zip(perturbations_set, file_name_set):
        los_matrix_array = np.array(los_matrix)
        df_los_icu = np.zeros(los_matrix_array.shape) - 1
        df_los_floor = np.zeros(los_matrix_array.shape) - 1
        df_los_vent = np.zeros(los_matrix_array.shape) - 1
        n_row, n_col = df_los_icu.shape
        for i in range(n_row):
            for j in range(n_col):
                if los_matrix_array[i, j] > 0: # can be modified
                    los_matrix_array[i, j] += perturb
                    simulator = DES_Simulator(MAX_SIMULATION_DAYS,
                                              starting_total,
                                              doubling_time,
                                              cohort_fraction,
                                              los_matrix_array.tolist())
                    simulator.state_init(icu_census_covid_0, floor_census_covid_0,
                                         icu_census_noncovid_mean, floor_census_noncovid_mean,
                                         df_input_census)
                    cap_days = simulator.run_till_cap(icu_capacity, floor_capacity, ventilator_capacity,
                                                        icu_non_covid_ventilator_percentage)
                    df_los_icu[i,j] = cap_days[0]
                    df_los_floor[i,j] = cap_days[1]
                    df_los_vent[i, j] = cap_days[2]
                    los_matrix_array[i, j] -= perturb
        df_los_icu[df_los_icu<0] = None
        df_los_floor[df_los_floor<0] = None
        df_los_vent[df_los_vent<0] = None
        df_los_icu = pd.DataFrame(df_los_icu)
        df_los_floor = pd.DataFrame(df_los_floor)
        df_los_vent = pd.DataFrame(df_los_vent)
        result_list += [df_los_icu, df_los_floor, df_los_vent]
        # df_los_icu.to_csv('sensitivity_los_'+file_name+'_icu.csv')
        # df_los_floor.to_csv('sensitivity_los_'+file_name+'_floor.csv')
        # df_los_vent.to_csv('sensitivity_los_'+file_name+'_ventilator.csv')
        # print(df_los_icu)
        # print(df_los_floor)
        # print(df_los_vent)
    return result_list

def arrival_fitting(df_new = None, fit_threshold = 0, use_past_n_days = 7):
    # input col with data and New COVID Admit
    # takes a table of arrivals generated by the user

    # base = STARTING_DAY
    # df_new = pd.DataFrame()
    # df_new[COL_DATE] = [base + datetime.timedelta(days = i) for i in range(20)]
    # df_new[COL_DAILY_NEW_COVID_ADMISSION] = None
    # df_new.to_csv('daily_new_covid_admission.csv')
    if df_new is not None:
        df_new[COL_DAY] = np.arange(len(df_new))
        df_new = df_new.dropna(subset = [COL_CUM_COVID_ADMISSION])
        df_temp = df_new.loc[(df_new[COL_CUM_COVID_ADMISSION]>fit_threshold) &
                            (df_new[COL_DAY] > df_new[COL_DAY].max() - use_past_n_days)]

        # X = df_temp[COL_DAY].values.reshape(-1, 1)  # values converts it into a numpy array
        # Y = np.log2(df_temp[COL_CUM_COVID_ADMISSION]).values.reshape(-1, 1)
        # linear_regressor = LinearRegression()  # create object for the class
        # linear_regressor.fit(X, Y)  # perform linear regression
        # c_fit = linear_regressor.intercept_[0] # intercenpt term
        # dt_fit = 1 / linear_regressor.coef_[0] # fitted doubling time
        # # a_0_fit = 2**c_fit * (-2**(-1/dt_fit)+1)# fitted first day admissions
        # # Y_fit = a_0_fit * (2**((df_temp['Day']+1)/dt_fit)-1 ) / (2**(1/dt_fit) - 1)
        # # df_new.loc[df_new['Accum']>fit_threshold, 'Fitted'] = Y_fit
        # Y_pred = linear_regressor.predict(X)
        # df_new.loc[(df_new[COL_CUM_COVID_ADMISSION]>fit_threshold) &
        #                     (df_new[COL_DAY] > df_new[COL_DAY].max() - use_past_n_days), 'Fitted'] = 2**(Y_pred)

        X = df_temp[COL_DAY].values  # values converts it into a numpy array
        Y = np.log2(df_temp[COL_CUM_COVID_ADMISSION]).values
        # simple linear regression formula
        # https://en.wikipedia.org/wiki/Simple_linear_regression
        X_bar = np.mean(X)
        Y_bar = np.mean(Y)
        beta = np.sum((X-X_bar)*(Y-Y_bar)) / np.sum((X-X_bar)**2) # slope
        alpha = Y_bar - beta * X_bar# interception
        Y_pred = beta * X + alpha
        dt_fit = 1 / beta

        df_new.loc[(df_new[COL_CUM_COVID_ADMISSION]>fit_threshold) &
                            (df_new[COL_DAY] > df_new[COL_DAY].max() - use_past_n_days), 'Fitted'] = 2**(Y_pred)


        df_fitted = pd.DataFrame([ ['Number of Data points Used', len(df_temp)],
                                   ['Fitted Doubling Time', dt_fit],
                                   ['Fitted First Day Admissions', 2**alpha]]
                                   )

        df_new = df_new.drop(columns = [COL_DAY])

        # print(df_fitted)
        # print(df_temp)
    else:
        df_fitted = pd.DataFrame([ ['Number of Data points Used', 0],
                                   ['Fitted Doubling Time', 0],
                                   ['Fitted First Day Admissions', 0]]
                                   )
    # print(df_fitted)
    # print(df_fitted.shape)
    # print(df_new.shape)

    return [df_fitted, df_new]



def test_df_convert(input_table):
    # test run: df = test_df_convert(input_table)
    # df = input_table
    # df = pd.DataFrame([1,2,3], columns = [1,2,3])
    # df = pd.DataFrame()
    input_table['New COVID Admit'] = input_table['New COVID Admit'] + 1
    df[COL_DAY] = [base + datetime.timedelta(days = i) for i in range(10)]
    df[COL_DAILY_NEW_COVID_ADMISSION] = None
    # df.to_csv('daily_new_covid_admission.csv')
    return input_table

    # print('shape of the dataframe, [n_row, n_col]:', df.shape)

    # return df


def test_null(input):
    if input == None:
        return 1
    else:
        return 0
