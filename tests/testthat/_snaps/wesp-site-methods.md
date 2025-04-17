# print method works

    Code
      print(site)
    Output
      A wesp_site object
      
      Site:  site_1 
      
      Incomplete Questions:  F25, F32, F33, F34, F35, F36, F43, F44, F52 
        * Please ensure that it is valid to leave these questions unanswered.
      
      Derived values:
        *  AllWater = 1
        *  NeverWater = 0
        *  NoSeasonal = 0
        *  NoPersis = 0
        *  TempWet = 0
        *  AllPermW = 0
        *  HiFlucW = 0
        *  TooShallow = 0
        *  NoPond = 0
        *  NoDeepPonded = 0
        *  NoOW = 1
        *  NoOutletX = 0
        *  NoOutlet = 0
        *  Inflow = 0
        *  Disturb = 0
        *  FishFound = 0
        *  Moose = 0
        *  Beaver = 1
        *  Muskrat = 0
        *  Bear = 0
        *  Caribou = 0
        *  NoCA = 0
        *  Fishless = 0
        *  GDeco = 1
        *  CMeco = 0
        *  SIMeco = 0
        *  BPeco = 0
        *  TPeco = 0
        *  OutMap = 1
        *  S1_sum = 0
        *  S1_subscore = 0
        *  S2_sum = 1
        *  S2_subscore = 0.11
        *  S3_sum = 1
        *  S3_subscore = 0.11
        *  S4_sum = 1
        *  S4_subscore = 0.08
        *  S5_sum = 0
        *  S5_subscore = 0
        *  S6_sum = 6
        *  S6_subscore = 1
      
      Indicators:
      All indicators are NULL. Run `calc_indicators()` to calculate them.

---

    Code
      print(calc_indicators(site))
    Output
      A wesp_site object
      
      Site:  site_1 
      
      Incomplete Questions:  F25, F32, F33, F34, F35, F36, F43, F44, F52 
        * Please ensure that it is valid to leave these questions unanswered.
      
      Derived values:
        *  AllWater = 1
        *  NeverWater = 0
        *  NoSeasonal = 0
        *  NoPersis = 0
        *  TempWet = 0
        *  AllPermW = 0
        *  HiFlucW = 0
        *  TooShallow = 0
        *  NoPond = 0
        *  NoDeepPonded = 0
        *  NoOW = 1
        *  NoOutletX = 0
        *  NoOutlet = 0
        *  Inflow = 0
        *  Disturb = 0
        *  FishFound = 0
        *  Moose = 0
        *  Beaver = 1
        *  Muskrat = 0
        *  Bear = 0
        *  Caribou = 0
        *  NoCA = 0
        *  Fishless = 0
        *  GDeco = 1
        *  CMeco = 0
        *  SIMeco = 0
        *  BPeco = 0
        *  TPeco = 0
        *  OutMap = 1
        *  S1_sum = 0
        *  S1_subscore = 0
        *  S2_sum = 1
        *  S2_subscore = 0.11
        *  S3_sum = 1
        *  S3_subscore = 0.11
        *  S4_sum = 1
        *  S4_subscore = 0.08
        *  S5_sum = 0
        *  S5_subscore = 0
        *  S6_sum = 6
        *  S6_subscore = 1
      
      Indicators:
        * WS:  
          - fun: 1.94 
          - ben: 5.97 
        * SR:  
          - fun: 0 
          - ben: 4.16 
        * PR:  
          - fun: 3.32 
          - ben: 4.14 
        * CP:  
          - fun: 6.56 
        * FR:  
          - fun: 6.58 
          - ben: 8.21 
        * SENS:  
          - fun: 4.92 
        * STR:  
          - fun: 5.31 
        * NR:  
          - fun: 4.16 
          - ben: 4.14 
        * APP:  
          - fun: 4.47 
          - ben: 3.14 
        * PD:  
          - fun: 4.12 
          - ben: 3.55 
        * KMH:  
          - fun: 6.59 
          - ben: 6.37 
        * WB:  
          - fun: 4.71 
          - ben: 4.38 
        * POL:  
          - fun: 5.05 
          - ben: 4.58 
        * RSB:  
          - fun: 0 
          - ben: 5.58 
        * OE:  
          - fun: 3.71 
        * AM:  
          - fun: 4.79 
          - ben: 7.46 
        * FH:  
          - fun: 3.04 
          - ben: 1.57 
        * SFTS:  
          - fun: 3.55 
          - ben: 4 
        * CRI:  
          - ben: 4.13 
      
      * Retrieve indicator scores with `get_indicator_scores()`

