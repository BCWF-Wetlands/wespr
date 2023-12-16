# update_site_indicator errors correctly

    Code
      update_site_indicator(list(), "ws")
    Condition
      Error:
      ! site must be an object of class 'wesp_site', created with `as.wesp_site`

---

    Code
      update_site_indicator(site, "PEANUT BUTTER COW")
    Condition
      Error:
      ! Invalid indicator: PEANUT BUTTER COW

---

    Code
      update_site_indicator(site, "cs", "ben")
    Condition
      Error:
      ! 'ben' is not a valid type for indicator 'cs'

