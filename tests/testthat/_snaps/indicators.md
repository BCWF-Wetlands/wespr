# update_site_indicator errors correctly

    Code
      update_site_indicator(list(), "ws")
    Condition
      Error:
      ! site must be an object of class 'wesp_site', created with `as.wesp_site`

---

    Code
      update_site_indicator(site, "blah")
    Condition
      Error:
      ! Invalid indicator: blah

---

    Code
      update_site_indicator(site, "cs", "benefit")
    Condition
      Error:
      ! 'benefit' is not a valid type for indicator 'cs'

