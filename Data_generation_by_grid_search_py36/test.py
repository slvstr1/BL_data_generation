import unittest
import numpy as np
from Data_generation_by_grid_search_py36.function import get_mean_sd_var, center

class test_functions(unittest.TestCase):
    def test_center(self):
        # print(type(center([0, 40, 100],[.25, .25, .5])[0]))
        # print(type(np.float64(60.0)))
        vvv= np.allclose( center([0, 40, 100], [.25, .25, .5])[0],(60.0))
        self.assertTrue(vvv)
        vv1 = np.allclose(center([0, 40, 100], [.25, .25, .5])[1],(np.array([-60., -20., 40.])))
        self.assertTrue(vv1)


    def test_get_mean_sd_var(self):
        # print(f"get_mean_sd_var([1,2,3,4,5],[.2,.2,.2,.2,.2]):{get_mean_sd_var([1, 2,3,4,5], [.2, .2, .2,.2,.2])}")
        # print(f"get_mean_sd_var([0,40,100],[.25,.25,.5]):{get_mean_sd_var([0,40,100],[.25,.25,.5])}")

        vvv = np.allclose(
            get_mean_sd_var([0,40,100],[.25,.25,.5]),
       (60.0,42.42640687119285, 1800.0))
        self.assertTrue(vvv)

if __name__ == '__main__':
    unittest.main()