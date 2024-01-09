# print method works

    Code
      print(site)
    Output
      A wesp_site object
      
      Site:  site_1 
      
      Incomplete Questions:  F25, F41, F43, F44, F59, OF28 
      
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
        *  NoOutlet = 1
        *  Inflow = 0
        *  Disturb = 1
        *  FishFound = 0
        *  Moose = 0
        *  Beaver = 1
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
      
      Incomplete Questions:  F25, F41, F43, F44, F59, OF28 
      
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
        *  NoOutlet = 1
        *  Inflow = 0
        *  Disturb = 1
        *  FishFound = 0
        *  Moose = 0
        *  Beaver = 1
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
          - fun: 7.02 
          - ben: 5.89 
        * SR:  
          - fun: 4.12 
          - ben: 3.67 
        * PR:  
          - fun:  
          - ben:  
        * CP:  
          - fun: 8.72 
        * FR:  
          - fun:  
          - ben:  
        * SENS:  
          - fun:  
        * STR:  
          - fun:  
        * NR:  
          - fun:  
          - ben:  
        * APP:  
          - fun:  
          - ben:  
        * PD:  
          - fun:  
          - ben:  
        * KMH:  
          - fun:  
          - ben:  
        * WB:  
          - fun:  
          - ben:  
        * POL:  
          - fun:  
          - ben:  
        * RSB:  
          - fun:  
          - ben:  
        * OE:  
          - fun:  
        * AM:  
          - fun:  
          - ben:  
        * FH:  
          - fun:  
          - ben:  
        * SFTS:  
          - fun:  
          - ben:  
        * CRI:  
          - ben:  
      
      * Retrieve indicator scores with `get_indicator_scores()`

