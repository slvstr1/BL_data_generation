import numpy as np
import time
from scipy.stats import norm
from Data_generation_by_grid_search_py36.function import get_mean_sd_var, get_mean_sd_var_skew_skewBL

class Outcome:
    def set_ds_ss(self, dsc_parameters, sample):
        ds =np.linspace(sample.begin, sample.end, dsc_parameters.ndraw)
        # print(f"1/length={ dsc_parameters.ndraw/(sample.end-sample.begin) }")
        self.ds= ds.clip(0) # this replaces the negative numbers with zero.
        self.ss = norm.pdf(self.ds, sample.mean_demand, sample.sd_demand)
        # self.ss_corrected = self.ss / self.ss.sum()
        self.ss_corrected = self.ss / sample.correction
        # print(f"self.ss.sum():{self.ss.sum()},    sample.correction:{sample.correction}")
        self.ds_mean, self.ds_sd, self.ds_var = get_mean_sd_var(self.ds, self.ss_corrected)


    def set_NN(self, producer,retailer):
        self.NN =  (producer.number + retailer.number)/producer.RA


    def set_ps(self,producer):
        self.ps = producer.get_mc(self.ds)
        self.ps_mean, self.ps_sd, self.ps_var, self.ps_skew, self.ps_skew_BL = get_mean_sd_var_skew_skewBL(self.ps, self.ss_corrected)


    def get_premium(self, producer, retailer):
        return  (-1 *
                (
                    (producer.number /
                        (self.NN * producer.c * np.power(producer.a, producer.x))) *
                        ( producer.c * retailer.retail_rate * producer.cov_pxs_ps - producer.cov_px1s_ps)
                )
                 )


    def set_fps(self, producer, retailer):
        self.forward_price = self.ps_mean + self.get_premium(producer, retailer)


    def save_outcomes(self, producer, retailer, sample, dsc_parameters, negative_omitted):
        list=[
            producer.number, retailer.number, sample.mean_demand, sample.sd_demand, self.ds_mean, self.ds_sd,int(negative_omitted) ,
            retailer.retail_rate,
            producer.c, producer.a, producer.RA, dsc_parameters.ndraw, self.forward_price, self.ps_mean, self.ds_var,self.ps_skew, self.ps_skew_BL,
            self.ps_var, 0,0,producer.profits_mean,producer.profits_sd,retailer.profits_mean,producer.profits_sd,
            0,0,0,retailer.opt_forw_position, producer.opt_forw_position,producer.opt_forw_position_comp, time.time(),producer.cov_pxs_ps,producer.cov_px1s_ps, int(dsc_parameters.a_fixed_boolean)]
        self.outcome_list = list