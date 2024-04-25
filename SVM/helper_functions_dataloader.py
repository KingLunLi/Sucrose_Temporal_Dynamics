#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import pandas as pd
import numpy as np
import warnings
import os
from typing import Union
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.pipeline import Pipeline
from time_constants import *


# In[ ]:


def addMissingTimesAndMaybeInterpolateDF(df: pd.DataFrame, source: str, interpolated: bool = False) -> pd.DataFrame:
    '''
    Returns a dataframe with missing times added in and (optionally) interpolated. 
    Prints out times that were added. \n
    DOES NOT MODIFY DataFrame IN PLACE. \n
    '''
    maxTime = int(10*max(df.index))
    minTime = int(10*min(df.index))
    allTimes = np.round(np.linspace(minTime/10, maxTime/10, num=maxTime-minTime+1), 1)
    unincludedTimes = set(allTimes).difference(set(df.index))
    df2 = df.copy()
    for t in unincludedTimes:
        df2.loc[t] = np.nan
    print(f"[{source}] Added {len(unincludedTimes)} times: {sorted(unincludedTimes)}")
    df2 = df2.sort_index()
    df2.index.name = 'Time(s)'
    if interpolated:
        df2.interpolate(inplace=True)
    return df2


# In[ ]:


def removeTimesOverlappingWithinMargin(lps: list, label="", margin=10) -> list:
    '''
        Removes times that have LP within Margin seconds. 
        Times that overlap favor removing the later time e.g.
        For times [1, 30, 31, 50] and Margin=20, time 31 will be removed.
    '''
    to_remove_lps = []
    for i in range(len(lps) - 1):
        if len(to_remove_lps) > 0 and to_remove_lps[-1] == lps[i]:
            continue
        if lps[i] + margin > lps[i + 1]:
            to_remove_lps.append(lps[i + 1])
    
    lps = [item for item in lps if item not in to_remove_lps]
    if len(to_remove_lps) > 0:
        print(f"[{label}] Removed {to_remove_lps}")
    return lps


# In[ ]:


def expandTimeWindow(df: pd.DataFrame, times: list, shuffle= False, time_grouped= False) -> Union[pd.DataFrame, np.ndarray]:
    '''
    Creates window around times provided.\n
    Uses TIME["LOWER"]/TIME["UPPER"] for size of window and returns a dataframe 
    with the window-augmented times of shape (#LPs * TIME["TOTAL"], # Cells)

    # FLAGS 

    shuffle: bool, default=False
    \t if true, shuffles result by time and returns it
    
    time_grouped: bool, default=False
    \t if true, returns 3d numpy array of dimensions (#LPs, TIME["TOTAL"], # Cells) 
    '''
    # Create concatenated list of timings with expanded range (TIME["LOWER"], TIME["UPPER"])
    times = np.sort(times)
    all_timing = np.zeros((len(times), TIME["TOTAL"]))
    for i in range(len(times)):
        time = np.round(times[i], decimals=1)
        temp = np.around(np.arange(int((time + TIME["LOWER"])*10), int((time + TIME["UPPER"])*10) + 2, 1)/10, decimals=1)[:TIME["TOTAL"]]
        if len(temp) != TIME["TOTAL"]:
            print(time)
            print(temp)
        all_timing[i] = temp
    assert(all_timing.shape == (len(times), TIME["TOTAL"]))

    # Transform to (#LP, TIME["TOTAL"], #cells) numpy array
    if time_grouped:
        df_ret = np.array([df.loc[t].to_numpy() for t in all_timing])
        if shuffle:
            s1,s2,s3 = df_ret.shape
            df_ret = df_ret.reshape(s1*s2, s3)
            perm_idxs = np.random.permutation(len(df_ret))
            df_ret = df_ret[perm_idxs].reshape(s1, s2, s3)
        return df_ret
    
    # Transform to grouped dataframe
    all_timing_flat = all_timing.flatten()
    df_ret = df.loc[all_timing_flat]
    if shuffle:
        df_ret = df_ret.iloc[np.random.permutation(len(df_ret))]
    df_ret['Group'] = np.tile(np.arange(TIME["TOTAL"]), all_timing.shape[0])
    df_ret = df_ret.set_index([df_ret.index, 'Group'])
    return df_ret


# In[ ]:


def removeOutsideMaxTimeLPs(LP: list, minTime:int, maxTime: int) -> list:
    return [i for i in LP if minTime <= i <= maxTime]


# In[ ]:


class DataLoader:
    def __init__(self, dir: str, animal_num: str, day_num: str, create_baseline_2: bool = False, 
                 use_manifold_transformed: bool = False, use_cut: bool = False, 
                 bl_sampling_lp_width: int = 5, bl_sampling_select_overlap: float = 2.5, bl_sampling_max_attempts: int = 25,
                 interpolated: bool = False, lp_time_overlap_margin: int = 10):
        
        # Create file prefix
        self.file_prefix = animal_num + '_' + day_num
        self.baseline2 = create_baseline_2
        self.empty = False
        if (use_cut):
            print("CUT")

        # Read all times
        standardized_path = os.path.join(dir, self.file_prefix + '_standardized.csv')
        if use_manifold_transformed:
            if use_cut:
                standardized_path = os.path.join(dir, self.file_prefix + 'CUT_manifold_transformed.csv')
                if not os.path.exists(standardized_path):
                    standardized_path = os.path.join(dir, self.file_prefix + '_manifold_transformed.csv')
            else:
                standardized_path = os.path.join(dir, self.file_prefix + '_manifold_transformed.csv')
            
        if not os.path.exists(standardized_path):
            self.empty = True
            self.__times_ALP = np.empty((0))
            self.times_ALP = np.empty((0))
            self.__times_ILP = np.empty((0))
            self.times_BL = np.empty((0))
            self.times_BL2 = np.empty((0))
            if use_manifold_transformed:
                warnings.warn("No manifold-transformed times file exists for " + self.file_prefix)
            else:
                warnings.warn("No standardized times file exists for " + self.file_prefix)
            return
        df_times = pd.read_csv(standardized_path)
        if 'Lever_Press_Timing(s)' in df_times.columns:
            df_times.rename(columns={'Lever_Press_Timing(s)':'Time(s)'}, inplace=True)
        df_times = df_times.set_index('Time(s)')
        if df_times.shape[0] == 0:
            self.empty = True
            self.__times_ALP = np.empty((0))
            self.times_ALP = np.empty((0))
            self.__times_ILP = np.empty((0))
            self.times_BL = np.empty((0))
            self.times_BL2 = np.empty((0))
            warnings.warn("No times exist in times file for " + standardized_path)
            return
        self.df_times = addMissingTimesAndMaybeInterpolateDF(df_times.dropna(), self.file_prefix, interpolated)

        # Read ALP Timing
        alp_timing_path = os.path.join(dir, self.file_prefix + '_Lever_Press_Timing.csv')
        if os.path.exists(alp_timing_path):
            df_ALP = pd.read_csv(alp_timing_path)
            if 'Lever_Press_Timing(s)' in df_ALP.columns:
                df_ALP.rename(columns={'Lever_Press_Timing(s)':'Time(s)'}, inplace=True)
            self.__times_ALP = removeOutsideMaxTimeLPs(df_ALP['Time(s)'].tolist(), -TIME["LOWER"], max(df_times.index) - TIME["UPPER"]) # Keep original as backup
            times_ALP = [x for x in self.__times_ALP]
            times_ALP = removeTimesOverlappingWithinMargin(self.__times_ALP, self.file_prefix + "_ALP", lp_time_overlap_margin)
            self.times_ALP = np.array(removeOutsideMaxTimeLPs(times_ALP, -TIME["LOWER"], max(df_times.index) - TIME["UPPER"]))
            if len(self.times_ALP) == 0:
                warnings.warn("No ALP times provided for " + alp_timing_path)
        else:
            self.__times_ALP = np.empty((0))
            self.times_ALP = np.empty((0))
            warnings.warn("No ALP file exists for " + alp_timing_path)

        # Read ILP Timing
        
        ilp_timing_path = os.path.join(dir, self.file_prefix + '_Inactive_Lever_Press_Timing.csv')
        ilp_timing_path_alt = os.path.join(dir, self.file_prefix + '_Inactive_lever_pressed.csv')
        if os.path.exists(ilp_timing_path):
            df_ILP = pd.read_csv(ilp_timing_path)
        elif os.path.exists(ilp_timing_path_alt):
            df_ILP = pd.read_csv(ilp_timing_path_alt)
        else:
            df_ILP = None
        if df_ILP is not None:
            if 'Lever_Press_Timing(s)' in df_ILP.columns:
                df_ILP.rename(columns={'Lever_Press_Timing(s)':'Time(s)'}, inplace=True)
            self.__times_ILP = removeOutsideMaxTimeLPs(df_ILP['Time(s)'].tolist(), -TIME["LOWER"], df_times.index[-1] - TIME["UPPER"]) # Keep original as backup
            times_ILP = removeTimesOverlappingWithinMargin(self.__times_ILP, self.file_prefix + "_ILP", lp_time_overlap_margin)
            self.times_ILP = removeOutsideMaxTimeLPs(times_ILP, -TIME["LOWER"], df_times.index[-1] - TIME["UPPER"])
        else:
            self.__times_ILP = np.empty((0))
            self.times_ILP = np.empty((0))

        # Create BL
        self.sampleBaseline(bl_sampling_select_overlap, bl_sampling_max_attempts, bl_sampling_lp_width)

    def __getNonLPTimes(self, bl_sampling_lp_width: int) -> list:
        # geterate list of times that can be selected from
        createInterval = lambda y: np.arange(int((y - bl_sampling_lp_width)*10), int((y + bl_sampling_lp_width)*10) + 1, 1)/10
        ALP_intervals = np.round([createInterval(y)[0:20*bl_sampling_lp_width+1] for y in self.__times_ALP], decimals=1).flatten()
        ILP_intervals = np.round([createInterval(y)[0:20*bl_sampling_lp_width+1] for y in self.__times_ILP], decimals=1).flatten()
        LP_intervals = set(np.append(ALP_intervals, ILP_intervals))
        all_times = set(list(self.df_times.index.values))
        return self.df_times.loc[list(all_times.difference(LP_intervals))]

    def __sampleBaselineImpl(self, bl_sampling_select_overlap: float, bl_sampling_max_attempts: int, bl_sampling_lp_width: int) -> list:
        non_LP = self.__getNonLPTimes(bl_sampling_lp_width)

        # Randomly select len(ALP) times (or less if there isn't)
        num_select = len(self.times_ALP)
        if self.baseline2:
            num_select = 2*len(self.times_ALP)
        to_st_filtered = np.array(non_LP.index.values)
        to_st_filtered = to_st_filtered[to_st_filtered < max(to_st_filtered) - TIME["UPPER"]]
        to_st_filtered = to_st_filtered[to_st_filtered > min(to_st_filtered) - TIME["LOWER"] + 1]
        times_BL = []
        max_len_times = []

        while len(times_BL) != num_select and bl_sampling_max_attempts > 0:
            bl_sampling_max_attempts -= 1
            times_BL = []
            for _ in range(num_select):
                if len(to_st_filtered) == 0: 
                    # The points selected don't space out well
                    break
                time = np.random.choice(to_st_filtered, 1)[0]
                to_st_filtered = [t for t in to_st_filtered if not(t - bl_sampling_select_overlap <= time <= t + bl_sampling_select_overlap)]
                times_BL.append(time)
            # Check for larger time selection and keep the bigger one, reset to pick again
            if len(times_BL) > len(max_len_times):
                max_len_times = times_BL
            to_st_filtered = np.array(non_LP.index.values)
            to_st_filtered = to_st_filtered[to_st_filtered < max(to_st_filtered) - TIME["UPPER"]]
            to_st_filtered = to_st_filtered[to_st_filtered > min(to_st_filtered) - TIME["LOWER"] + 1]

        times_BL = max_len_times
        if len(times_BL) != num_select:
            print(f"NOT ENOUGH RANDOM TIME POINTS FOR {self.file_prefix}! Selected {len(times_BL)} when we want {num_select}")
        times_BL.sort()
        return times_BL

    def sampleBaseline(self, bl_sampling_select_overlap: float, bl_sampling_max_attempts: int, bl_sampling_lp_width: int):
        self.times_BL = self.__sampleBaselineImpl(bl_sampling_select_overlap, bl_sampling_max_attempts, bl_sampling_lp_width)
        if self.baseline2:
            np.random.shuffle(self.times_BL)
            self.times_BL2 = np.sort(self.times_BL[len(self.times_BL)//2:])
            self.times_BL = np.sort(self.times_BL[:len(self.times_BL)//2])


# %%
