.advance_date <- function(adults, developing, adults_cropped_prev_hres, adults_cropped_duration_hres, pr_psyllid_survival, pr_egg_survival){
#browser()
  adults$assign(pr_psyllid_survival * adults)
  developing$assign(pr_egg_survival * developing)
  
  adults_cropped_duration_hres$assign_add(tf$cast(tf$where(tf$greater(adults_cropped_duration_hres, 0), 1, 0), dtype=adults_cropped_duration_hres$dtype))
  
  return(list(adults, developing, adults_cropped_duration_hres))
}

.advance_date_tf <- tf$`function`(.advance_date)

