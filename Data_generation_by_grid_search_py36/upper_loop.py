import numpy as np
import pandas as pd
import time
from Data_generation_by_grid_search_py36.lower_loop import lower_loop_routine
from io import StringIO
from csv import writer
from datetime import datetime
# import csv

def to_integer(dt_time):
    return 100000000*(dt_time.year-2000) + 1000000*dt_time.month + 10000* dt_time.day + 100* dt_time.hour + dt_time.second

class Data_set_creator_iterator:
    def __init__(self,  negative_omitted_range, c_range, sd_range,ed_range ):
        self.negative_omitted_range = negative_omitted_range
        self.sd_range= sd_range
        self.ed_range = ed_range
        self.c_range = c_range


class Data_set_creator_parameters:
    def __init__(self,  width_in_SDs, points_per_SD, NP, NR):
        self.column_names = [
            'NP', 'NR', 'EDV', 'sd', 'EDApprox', 'sdApprox', 'negative_omitted', 'retail_Rate',
            'c', 'a', 'RA', 'ndraw', 'price_Forward', 'price_Expected', 'varDemand', 'skew', 'skew_Nonstandard_BL',
            'VarPrice', 'profit_Margin', 'profitmarginExPo', 'expectedProfit_PR', 'sdProfit_PR', 'expectedProfit_RE',
            'sdProfit_RE', 'sdReProfitExPo', 'expectedProfit_Total', 'sdProfit_Total','OoptForwPosition_RE',
            'OoptForwPosition_PR','OoptForwPosition_PR_computed', 'startTime', 'covPxP1', 'covPx1P1','amethod']
        self.width_in_SDs = width_in_SDs
        self.points_per_SD = points_per_SD
        self.NP = NP
        self.NR = NR
        # output = BytesIO()
        # self.csv_writer = writer(output)
        self.outcome_list = []


    def set_value_fixed_a(self,dsc_iterator):
        # self.a_fixed_value =  (dsc_iterator.ed_range[0] +dsc_iterator.ed_range[-1])/2
        self.a_fixed_value = 100
        print(f"a_fixed_value :{self.a_fixed_value}")

    def set_ndraw(self):
        self.ndraw = self.points_per_SD * 2 * self.width_in_SDs + 1
    
    def clean_df(self):
        self.df = pd.DataFrame([], columns=self.column_names)
        self.output = StringIO()
        csv_writer = writer(self.output)
        csv_writer.writerow(self.column_names)
        self.csv_writer =csv_writer

    def set_a_fixed_boolean(self):
        self.a_fixed_boolean = True
        # print(f"self.a_fixed_boolean = a_fixed: {a_fixed}")


if __name__ == "__main__":
    if __name__ == "__main__":
        dsc_iterator = Data_set_creator_iterator(
            # a_fixed_range=[False, True],
            negative_omitted_range =[True, ],
            c_range=np.arange(2, 5 + 1, 1),
            sd_range=np.arange(1, 40 + .1, .10),
            ed_range=np.arange(50, 150 + .2, .20)
        )


        dsc_parameters = Data_set_creator_parameters(
            width_in_SDs=5,
            points_per_SD=1000,
            NP=20,
            NR=20,
            )

        # # for quick (reasonably accurate) estimates, uncomment the code below
        # dsc_iterator = Data_set_creator_iterator(
        #     negative_omitted_range =[1,],
        #     c_range=np.arange(2, 5 + 1, 1),
        #     sd_range=np.arange(1, (40+ .1) , 10),
        #     ed_range=np.arange(50, (130+ 20) , 20)
        # )
        #
        # dsc_parameters = Data_set_creator_parameters(
        #     width_in_SDs=2,
        #     points_per_SD=100,
        #     NP=20,
        #     NR=20,
        #     )





        dsc_parameters.set_value_fixed_a(dsc_iterator)
        dsc_parameters.set_ndraw()

        for negative_omitted in dsc_iterator.negative_omitted_range:
            for c in dsc_iterator.c_range:
                dsc_parameters.clean_df()
                csv_writer = dsc_parameters.csv_writer
                dsc_parameters.set_a_fixed_boolean()


                time1 = time.time()
                print("")
                print(f"start for negative_omitted:{int(negative_omitted)}  c:{c}                   at {datetime.now()}")



                timer_total=0
                counter=0
                for sd in dsc_iterator.sd_range:
                    counter += 1
                    if divmod(counter,5)[1]==1:
                        print(f"sd:{sd}")
                    print(f"sd:{sd}")
                    for ed in dsc_iterator.ed_range:
                        timer=time.time()
                        outcome_list = lower_loop_routine(negative_omitted, c, ed, sd, dsc_parameters)
                        timer_total += time.time()-timer

                        csv_writer.writerow(outcome_list)


                print(f"average time per loop: {timer_total/(sd * ed)}")
                time_now =datetime.now()

                print(f"END   for negative_omitted:{int(negative_omitted)}  c:{c}                   at {time_now}")
                print(f"total time negative_omitted:{negative_omitted},c:{c}      {(time.time() - time1)/60} minutes")

                rr = to_integer(time_now)  # just to make
                filename = f"pyt_ negative_omitted_{int(negative_omitted)}. Data c_{c}. Weightmethod_0_ {20}{rr}.csv.gz"
                print(filename)
                import os.path
                filename_with_path = os.path.join("../stata_analysis_home_folder/data/data_in_raw_format_gen_in_python",filename)
                # filename_with_path = os.path.join("/stata_analysis_home_folder/data", filename)

                output = dsc_parameters.output
                print("")
                for row in output:
                    print(output)

                print(f"output: {output}")
                output.seek(0)  # we need to get back to the start of the BytesIO
                df = pd.read_csv(output)
                # print(f"filename:{filename}")
                try:
                    df.to_csv(filename_with_path, encoding='utf-8', index=False,compression='gzip')
                except FileNotFoundError:
                    print(f"the path '{filename_with_path}' could not be found on your system")
                    try:
                        df.to_csv(filename, encoding='utf-8', index=False, compression='gzip')
                        print(f"the file is now saved as '{filename}'")
                    except FileNotFoundError:
                        print("the file was created, but the system failed to save it on the disk")
                else:
                    print(f"the file was saved as '{filename_with_path}'")