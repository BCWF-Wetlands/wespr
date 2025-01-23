# print method works

    Code
      print(site)
    Output
      A wesp_site object
      
      Site:  site_1 
      
      Incomplete Questions:  F43, F44, F50 
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
      
      Incomplete Questions:  F43, F44, F50 
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
          - ben: 4.06 
        * PR:  
          - fun: 10 
          - ben: 3.18 
        * CP:  
          - fun: 6.86 
        * FR:  
          - fun: 4.39 
          - ben: 8.06 
        * SENS:  
          - fun: 6.24 
        * STR:  
          - fun: 7.96 
        * NR:  
          - fun: 10 
          - ben: 5 
        * APP:  
          - fun: 4.81 
          - ben: 4 
        * PD:  
          - fun: 5.59 
          - ben: 5.45 
        * KMH:  
          - fun: 5.38 
          - ben: 5.3 
        * WB:  
          - fun: 4.95 
          - ben: 7.19 
        * POL:  
          - fun: 5.81 
          - ben: 6 
        * RSB:  
          - fun: 4.96 
          - ben: 5.58 
        * OE:  
          - fun: 0 
        * AM:  
          - fun: 3.17 
          - ben: 6.03 
        * FH:  
          - fun: 2.92 
          - ben: 4.98 
        * SFTS:  
          - fun: 1.81 
          - ben: 2.07 
        * CRI:  
          - ben: 5.35 
      
      * Retrieve indicator scores with `get_indicator_scores()`

