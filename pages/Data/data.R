

# CDC Places --------------------------------------------------------------
f_places2022 <- if (file.exists("pages/Data/f_places2022.csv")) {
  read.csv("pages/Data/f_places2022.csv")
} else {
  read.csv("./Data/f_places2022.csv")
}

# Public Health Data Access Portal Immunization -----------------------------------------
f_pHDAPImmunization <- if (file.exists("pages/Data/f_pHDAPImmunization.csv")) {
  read.csv("pages/Data/f_pHDAPImmunization.csv")
} else {
  read.csv("./Data/f_pHDAPImmunization.csv")
}


# CDC SVI -----------------------------------------------------------------

# Code is commented out because the final data set is not include in the CHA
# socioeconomicStatus  <- read.csv("pages/Data/CDC_svi.csv") |>
#   janitor::clean_names() |>
#   dplyr::mutate(
#     personPerSqMile = e_totpop/area_sqmi
#     , county = sub(" County.*","",county) #Extract just the county name so everything before space county is pulled
#     , location = sub(" County.*","",location) #Extract just the county name so everything before space county is pulled
#   ) |>
#   dplyr::arrange(desc(area_sqmi))
# 
# 
# state_totalRow <- socioeconomicStatus |>
#   dplyr::summarise(
#     fips = gbVal[2] #State total will be 99 throughout the report
#     , county = "Minnesota"
#     , personPerSqMile = sum(e_totpop, na.rm = TRUE)/ sum(area_sqmi, na.rm = TRUE)
#     , e_totpop = sum(e_totpop, na.rm = TRUE)
#     , area_sqmi = sum(area_sqmi, na.rm = TRUE)
#   ) |>
#   as.data.frame()
# 
# chb_totalRow <- socioeconomicStatus |>
#   gbFun_countyFilter(fips)  |>
#   dplyr::summarise(
#     fips = gbVal[3] # CHB total will be 999 throughout the report
#     , county = "CHS"
#     , personPerSqMile = sum(e_totpop, na.rm = TRUE)/ sum(area_sqmi, na.rm = TRUE)
#     , e_totpop = sum(e_totpop, na.rm = TRUE)
#     , area_sqmi = sum(area_sqmi, na.rm = TRUE)
#   ) |>
#   as.data.frame()
# 
# f_cdcSvi2022 <-
#   state_totalRow |>
#   dplyr::bind_rows(chb_totalRow) |>
#   dplyr::bind_rows(socioeconomicStatus) |>
#   gbFun_countyFilter(fips)
# # 
#  write.csv(f_cdcSvi2022, "f_cdcSvi2022.csv", row.names = FALSE)

f_cdcSvi2022 <- if (file.exists("pages/Data/f_cdcSvi2022.csv")) {
  read.csv("pages/Data/f_cdcSvi2022.csv")
} else {
  f_cdcSvi2022 <- read.csv("./Data/f_cdcSvi2022.csv")
}

f_cdcSvi2022 <- f_cdcSvi2022 |> 
  dplyr::mutate(
    area_sqmi = formattable::comma(area_sqmi,2)
    , personPerSqMile = formattable::comma(personPerSqMile,1)
    , e_pov150 = formattable::comma(e_pov150,0)
    , e_hburd = formattable::comma(e_hburd,0)
    , e_nohsdp = formattable::comma(e_nohsdp,0)
    , e_uninsur = formattable::comma(e_uninsur,0)
    , e_age65 = formattable::comma(e_age65,0)
    , e_crowd = formattable::comma(e_crowd,0)
  ) |> 
  dplyr::arrange(fips)

# County Health Rankings Trend Data --------------------------------------------------------------

f_CHRR2024 <- if (file.exists("pages/Data/f_CHRR2024.csv")) {
  read.csv("pages/Data/f_CHRR2024.csv")
} else {
  f_CHRR2024 <- read.csv("./Data/f_CHRR2024.csv")
}

f_CHRR2024 <- f_CHRR2024 |> 
  dplyr::mutate(
    #The raw value is stored as character with commas, I have to first remove commas and then convert to number
    rawvalue = as.integer(gsub(",", "", rawvalue))
    , cihigh = formattable::comma(as.numeric(gsub(",", "", cihigh)),1)
    , cilow = formattable::comma(as.numeric(gsub(",", "", cilow)),1)
  )

# Food Shelf --------------------------------------------------------------

f_foodShelf <- if (file.exists("pages/Data/f_foodShelf.csv")) {
  read.csv("pages/Data/f_foodShelf.csv")
} else {
  read.csv("./Data/f_foodShelf.csv")
}

# Graduation Rate ---------------------------------------------------------


# Third Grade Proficiency ---------------------------------------------------------

f_mdeThirdGradeProficient <- if (file.exists("pages/Data/mdeThirdGradeEducationStatus.csv")) {
  read.csv("pages/Data/mdeThirdGradeEducationStatus.csv")  
} else {
  read.csv("./Data/mdeThirdGradeEducationStatus.csv")  
}

# Unemployment Rate ---------------------------------------------------------
f_unemploymentRateDEED <- if (file.exists("pages/Data/f_unemploymentRateDEED.csv")) {
  read.csv("pages/Data/f_unemploymentRateDEED.csv")  
} else {
  read.csv("./Data/f_unemploymentRateDEED.csv")  
}

# Child and Teen Checkup ---------------------------------------------------------


# Births to mothers who received late or inadequate prenatal care in Minnesota ---------------------

f_kidsCountMotherPnc <- if (file.exists("pages/Data/f_kidsCountMotherPnc.csv")) {
  read.csv("pages/Data/f_kidsCountMotherPnc.csv")  
} else {
  read.csv("./Data/f_kidsCountMotherPnc.csv")  
}

# Mothers who smoked during pregnancy -------------------------------------------------------

f_kidsCountMotherSmoke <- if (file.exists("pages/Data/f_kidsCountMotherSmoke.csv")) {
  read.csv("pages/Data/f_kidsCountMotherSmoke.csv")  
} else {
  read.csv("./Data/f_kidsCountMotherSmoke.csv")  
}


# Neonatal Abstinence Syndrome ---------------------

f_neonatalAbstinenceSyndrome2016_2022 <- if (file.exists("pages/Data/f_neonatalAbstinenceSyndrome2016_2022.csv")) {
  read.csv("pages/Data/f_neonatalAbstinenceSyndrome2016_2022.csv")  
} else {
  read.csv("./Data/f_neonatalAbstinenceSyndrome2016_2022.csv")  
}

# Nonfatal Drug Overdose ---------------------

f_mdhNonFatalDrugOverdose <- if (file.exists("pages/Data/f_mdhNonFatalDrugOverdose.csv")) {
  read.csv("pages/Data/f_mdhNonFatalDrugOverdose.csv")  
} else {
  read.csv("./Data/f_mdhNonFatalDrugOverdose.csv")  
}

# MSS ---------------------

f_mss <- if (file.exists("pages/Data/f_mss.csv")) {
  read.csv("pages/Data/f_mss.csv")  
} else {
  read.csv("./Data/f_mss.csv")  
}

# STI/HIV ---------------------

f_stiHIV <- if (file.exists("pages/Data/f_stiHIV.csv")) {
  read.csv("pages/Data/f_stiHIV.csv")
} else {
  read.csv("./Data/f_stiHIV.csv")  
}

# Board of Pharmacy PMP ---------------------

f_boardOfPharmacyPmp <- if (file.exists("pages/Data/f_boardOfPharmacyPmp.csv")) {
  read.csv("pages/Data/f_boardOfPharmacyPmp.csv")
} else {
  read.csv("./Data/f_boardOfPharmacyPmp.csv")  
}


# Health Care Quality Measures Downloadable Public Use Files --------------

## AMH = Adolescent Mental Health and/or Ddpression Screening
# The percentage of patients 12-17 years of age who were screened for mental health and/or depression

## ODC = Optimal Diabetes Care
# The percentage of patients 18-75 years of age who had a diagnosis of type 1 or type 2 diabetes and whose diabetes was optimally managed during the measurement period as defined by achieving ALL of the following:
# •	HbA1c less than 8.0 mg/dL
# •	Blood pressure less than 140/90 mmHg
# •	On a statin medication, unless allowed contraindications or exceptions are present
# •	Non-tobacco user
# •	Patient with ischemic vascular disease is on daily aspirin or anti-platelets, unless allowed contraindications or exceptions are present

f_puf <- if (file.exists("pages/Data/f_puf.csv")) {
  read.csv("pages/Data/f_puf.csv")
} else {
  read.csv("./Data/f_puf.csv")  
}

# MDH Fatal Drug Overdose --------------

f_fatalDrugOverdose <- if (file.exists("pages/Data/f_fatalDrugOverdose.csv")) {
  read.csv("pages/Data/f_fatalDrugOverdose.csv")
} else {
  read.csv("./Data/f_fatalDrugOverdose.csv")  
}


# MDH WIC Breastfeeding ---------------------------------------------------

f_wicBreastFeeding <- if (file.exists("pages/Data/f_wicBreastFeeding.csv")) {
  read.csv("pages/Data/f_wicBreastFeeding.csv")
} else {
  read.csv("./Data/f_wicBreastFeeding.csv")  
}

# MDH MDH Public Health Data Access Portal Medicaid Dental Services ------------------------------

f_pHDAPDental <- if (file.exists("pages/Data/f_pHDAPDental.csv")) {
  read.csv("pages/Data/f_pHDAPDental.csv")
} else {
  read.csv("./Data/f_pHDAPDental.csv")  
}

