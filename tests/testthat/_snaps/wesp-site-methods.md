# print method works

    Code
      print(site)
    Output
      A wesp_site object
      
      Site:  site_1 
      
      Incomplete Questions:  F43, F44, F50, OF28 
        * Please ensure that it is valid to leave these questions unanswered.
      
      Derived values:
        *  AllWater = 0
        *  NeverWater = 0
        *  NoSeasonal = 0
        *  NoPersis = 0
        *  TempWet = 0
        *  AllPermW = 0
        *  HiFlucW = 0
        *  TooShallow = 0
        *  NoPond = 0
        *  NoDeepPonded = 0
        *  NoOW = 0
        *  NoOutletX = 0
        *  NoOutlet = 0
        *  Inflow = 0
        *  Disturb = 0
        *  FishFound = 0
        *  Moose = 0
        *  Beaver = 0
        *  Muskrat = 0
        *  Bear = 0
        *  Caribou = 0
        *  NoCA = 0
        *  Fishless = 0
        *  GDeco = 0
        *  CMeco = 1
        *  SIMeco = 0
        *  BPeco = 0
        *  TPeco = 0
        *  OutMap = 0
        *  S1_sum = 2
        *  S1_subscore = 0.17
        *  S2_sum = 0
        *  S2_subscore = 0
        *  S3_sum = 0
        *  S3_subscore = 0
        *  S4_sum = 5
        *  S4_subscore = 0.42
        *  S5_sum = 0
        *  S5_subscore = 0
        *  S6_sum = 0
        *  S6_subscore = 0
      
      Indicators:
      All indicators are NULL. Run `calc_indicators()` to calculate them.

---

    Code
      print(calc_indicators(site))
    Output
      A wesp_site object
      
      Site:  site_1 
      
      Incomplete Questions:  F43, F44, F50, OF28 
        * Please ensure that it is valid to leave these questions unanswered.
      
      Derived values:
        *  AllWater = 0
        *  NeverWater = 0
        *  NoSeasonal = 0
        *  NoPersis = 0
        *  TempWet = 0
        *  AllPermW = 0
        *  HiFlucW = 0
        *  TooShallow = 0
        *  NoPond = 0
        *  NoDeepPonded = 0
        *  NoOW = 0
        *  NoOutletX = 0
        *  NoOutlet = 0
        *  Inflow = 0
        *  Disturb = 0
        *  FishFound = 0
        *  Moose = 0
        *  Beaver = 0
        *  Muskrat = 0
        *  Bear = 0
        *  Caribou = 0
        *  NoCA = 0
        *  Fishless = 0
        *  GDeco = 0
        *  CMeco = 1
        *  SIMeco = 0
        *  BPeco = 0
        *  TPeco = 0
        *  OutMap = 0
        *  S1_sum = 2
        *  S1_subscore = 0.17
        *  S2_sum = 0
        *  S2_subscore = 0
        *  S3_sum = 0
        *  S3_subscore = 0
        *  S4_sum = 5
        *  S4_subscore = 0.42
        *  S5_sum = 0
        *  S5_subscore = 0
        *  S6_sum = 0
        *  S6_subscore = 0
      
      Indicators:
        * WS:  
          - fun: 4.61 
          - ben: 10 
        * SR:  
          - fun: 10 
          - ben: 3.68 
        * PR:  
          - fun: 10 
          - ben: 3.46 
        * CP:  
          - fun: 6.86 
        * FR:  
          - fun: 2.73 
          - ben: 8.06 
        * SENS:  
          - fun: 6.46 
        * STR:  
          - fun: 7.62 
        * NR:  
          - fun: 10 
          - ben: 5 
        * APP:  
          - fun: 4.81 
          - ben: 3.82 
        * PD:  
          - fun: 5.25 
          - ben: NA 
        * KMH:  
          - fun: 5.38 
          - ben: 5.3 
        * WB:  
          - fun: 5.04 
          - ben: 7.19 
        * POL:  
          - fun: 5.64 
          - ben: 6 
        * RSB:  
          - fun: NA 
          - ben: 4.7 
        * OE:  
          - fun: 3.4 
        * AM:  
          - fun: 3.06 
          - ben: 6.03 
        * FH:  
          - fun: 3.34 
          - ben: 5.01 
        * SFTS:  
          - fun: 2.76 
          - ben: 13.93 
        * CRI:  
          - ben: 5.39 
      
      * Retrieve indicator scores with `get_indicator_scores()`

