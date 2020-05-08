from Data_generation_by_grid_search_py36.market_participants import Producer, Retailer
from Data_generation_by_grid_search_py36.outcomes import Outcome


class Sample:
    def __init__(self, mean_demand, sd_demand):
        self.mean_demand = mean_demand
        self.sd_demand = sd_demand
        self.correction = None

    def set_d_begin_end(self, dsc_parameters, negative_omitted):
        halflength = dsc_parameters.width_in_SDs * self.sd_demand
        if negative_omitted:
            self.begin = max(self.mean_demand - halflength,0) # negative values are disregarded - start is truncated!
        else:
            self.begin = self.mean_demand - halflength  # self.begin MAY be negative!

        self.end = self.mean_demand + halflength
        # self.correction2 = (self.end-self.begin)/(dsc_parameters.width_in_SDs * N)

    def set_correction(self, dsc_parameters, sd):
        self.correction_wrong = dsc_parameters.points_per_SD/sd
        self.correction = dsc_parameters.ndraw / (self.end - self.begin)


def lower_loop_routine(negative_omitted, c, ed, sd,  dsc_parameters):

    # time1 = time.time()
    sample = Sample(
        mean_demand=ed,
        sd_demand=sd,
    )
    producer = Producer(
        c=c,
        F=0,
        number=dsc_parameters.NP
    )
    retailer = Retailer(
        retail_uplift=1.2,
        number=dsc_parameters.NR
    )

    outcome = Outcome()
    # if a_fixed:
    a_value = dsc_parameters.a_fixed_value
    # else:
    #     a_value = sample.mean_demand

    # initialize values
    producer.set_a(a_value)
    producer.set_RA()
    retailer.set_RA(producer.RA)
    sample.set_d_begin_end(dsc_parameters, negative_omitted)
    sample.set_correction(dsc_parameters, sd)

    # calculate demands and prices,
    outcome.set_ds_ss(dsc_parameters, sample)
    outcome.set_NN(producer, retailer)
    outcome.set_ps(producer)

    # calculate retail rate as uplift on av. price and producer costs
    retailer.set_retail_rate(outcome)
    producer.set_costs(outcome.ds)

    # calculate covariates and forward price
    producer.set_covs(outcome)
    retailer.set_covs(outcome)

    outcome.set_fps(producer, retailer)

    # calculate resulting profits
    producer.set_profits_and_means(outcome)
    retailer.set_profits_and_means(outcome)

    producer.set_opt_forw_position(outcome, retailer)
    retailer.set_opt_forw_position(outcome, producer)
    opt_forw_position_PR = producer.opt_forw_position
    opt_forw_position_RE = retailer.opt_forw_position * -1


    producer.set_profit_expost_and_means(outcome, opt_forw_position_PR)
    retailer.set_profit_expost_and_means(outcome, opt_forw_position_PR)

    producer.set_opt_forw_position_comp(outcome, retailer)


    outcome.save_outcomes(producer, retailer, sample, dsc_parameters, negative_omitted)
    # dsc_parameters.df = dsc_parameters.df.append(outcome.df)
    # print("")
    # print("outcome.outcome_list")
    # print(outcome.outcome_list)

    # solve_optimization(producer, retailer, outcome)

    # csv_writer = dsc_parameters.csv_writer
    # csv_writer.writerow(outcome.outcome_list)
    # dsc_parameters.csv_writer = csv_writer


    return outcome.outcome_list