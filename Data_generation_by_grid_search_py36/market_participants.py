import numpy as np
from Data_generation_by_grid_search_py36.function import get_mean_sd_var, get_covar, get_eu, get_total_eu
from scipy.optimize import minimize_scalar


class Market_participant(object):
    def get_profits_expost(self, outcome, forward_position):
        raise NotImplementedError


    def set_profits_and_means(self, outcome):
        self.profits = self.get_profits_expost(outcome, 0)
        self.profits_mean, self.profits_sd, self.profits_var = get_mean_sd_var(self.profits, outcome.ss_corrected)
        self.cov_profit_price = get_covar(self.profits, outcome.ps, outcome.ss_corrected)


    def set_profit_expost_and_means(self, outcome, opt_forw_position):
        self.profits_expost = self.get_profits_expost(outcome, opt_forw_position)
        self.profits_expost_mean, self.profits_expost_sd, self.profits_expost_var = get_mean_sd_var(self.profits_expost, outcome.ss_corrected)
        self.eu = get_eu(self.profits_expost_mean, self.profits_expost_sd, self.profits_expost_var, self.RA)



class Producer(Market_participant):
    def get_mc(self, Q):
        return self.a * np.power(Q/self.number, (self.c - 1))

    def get_profits_expost(self, outcome, forward_position_PR):
        # print(f"mp(producer):{self}")
        # print(f"self.costs: {self.costs}")
        return (
                (
                        np.multiply(outcome.ps, (outcome.ds / self.number) - forward_position_PR)
                        - self.costs
                        + (forward_position_PR * outcome.forward_price)
                )

        )

    def get_opt_forward_position(self, outcome, retailer):
        return (
            (outcome.get_premium(self, retailer) / self.RA)
        + ((1/(np.power(self.a,self.x))) * (1-(1/self.c))*self.cov_px1s_ps )
               ) / outcome.ps_var


    def __init__(self, c, F, number, RA=0, a=0):
        self.c = c
        self.a=a
        self.F = F
        self.number = number
        # self.RA = RA
        self.x = 1.0 / (self.c - 1.0)

    def set_a(self,mean_demand):
        self.a = 30 * np.power((self.number / mean_demand), self.c - 1)

    def set_RA(self):
        self.RA = 0.8 / (np.power(2, self.c))


    def set_covs(self, outcome):
        self.pxs = np.power(outcome.ps, self.x)
        self.px1s = np.power(outcome.ps, ((self.x)+1))
        self.cov_pxs_ps = get_covar(self.pxs, outcome.ps,outcome.ss_corrected)
        self.cov_px1s_ps = get_covar(self.px1s, outcome.ps, outcome.ss_corrected)


    def set_costs(self, D):
        self.costs= self.F + (self.a / self.c) * np.power(D/self.number, self.c)

    def set_opt_forw_position(self, outcome, retailer):
        self.opt_forw_position = self.get_opt_forward_position(outcome,  retailer)

    def set_opt_forw_position_comp(self, outcome, retailer):
        def solvethis(x):
            return -1 * get_total_eu(x, self, retailer, outcome)

        self.opt_forw_position = self.get_opt_forward_position(outcome,  retailer)
        res = minimize_scalar(solvethis, bounds=(-100, 100), method='bounded')
        self.opt_forw_position_comp = res.x


class Retailer(Market_participant):

    def get_profits_expost(self, outcome, forward_position_PR):
        return (
                (
                        np.multiply(self.retail_rate , outcome.ds) / self.number
                        -
                        np.multiply(outcome.ps, (outcome.ds/ self.number)  - forward_position_PR)
                        - (forward_position_PR * outcome.forward_price)
                )

        )


    def get_opt_forward_position(self, outcome, producer):
        return (
            (outcome.get_premium(producer, self) / self.RA )
            + ( self.retail_rate *  self.cov_ds_ps)
            - self.cov_pds_ps) / outcome.ps_var


    def __init__(self, retail_uplift, number):
        self.retail_uplift = retail_uplift
        self.number = number

    def set_RA(self, producer_RA):
        self.RA = producer_RA

    def set_retail_rate(self, outcome):
        self.retail_rate = outcome.ps_mean * self.retail_uplift

    def set_covs(self, outcome):
        self.pds = np.multiply(outcome.ps , outcome.ds / self.number)
        self.cov_ds_ps = get_covar(outcome.ds/ self.number, outcome.ps, outcome.ss_corrected)
        self.cov_pds_ps = get_covar(self.pds, outcome.ps, outcome.ss_corrected)


    def set_opt_forw_position(self, outcome, producer):
        self.opt_forw_position = self.get_opt_forward_position(outcome,  producer)