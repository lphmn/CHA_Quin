
# CDC Places --------------------------------------------------------------
if (file.exists("pages/Data/f_places2022.csv")) {
  f_places2022 <- read.csv("pages/Data/f_places2022.csv")
} else {
  f_places2022 <- read.csv("./Data/f_places2022.csv")
}

# Public Health Data Access Portal Immunization -----------------------------------------
if (file.exists("pages/Data/f_pHDAPImmunization.csv")) {
  f_pHDAPImmunization <- read.csv("pages/Data/f_pHDAPImmunization.csv")
} else {
  f_pHDAPImmunization<- read.csv("./Data/f_pHDAPImmunization.csv")
}

# County Health Rankings Trend Data --------------------------------------------------------------

if (file.exists("pages/Data/f_CHRR2024.csv")) {
  f_CHRR2024 <- read.csv("pages/Data/f_CHRR2024.csv") |> 
    dplyr::mutate(
    #The raw value is stored as character with commas, I have to first remove commas and then convert to number
    rawvalue = as.integer(gsub(",", "", rawvalue))
    , cihigh = formattable::comma(as.numeric(gsub(",", "", cihigh)),0)
    , cilow = formattable::comma(as.numeric(gsub(",", "", cilow)),0)
  )
} else {
  f_CHRR2024 <- read.csv("./Data/f_CHRR2024.csv") |> 
    dplyr::mutate(
      #The raw value is stored as character with commas, I have to first remove commas and then convert to number
      rawvalue = as.integer(gsub(",", "", rawvalue))
      , cihigh = formattable::comma(as.numeric(gsub(",", "", cihigh)),1)
      , cilow = formattable::comma(as.numeric(gsub(",", "", cilow)),1)
    )
}

# Food Shelf --------------------------------------------------------------

if (file.exists("pages/Data/f_foodShelf.csv")) {
  f_foodShelf <- read.csv("pages/Data/f_foodShelf.csv")
} else {
  f_foodShelf <- read.csv("./Data/f_foodShelf.csv")
}

# Graduation Rate ---------------------------------------------------------


# Third Grade Proficiency ---------------------------------------------------------

if (file.exists("pages/Data/mdeThirdGradeEducationStatus.csv")) {
  f_mdeThirdGradeProficient <- read.csv("pages/Data/mdeThirdGradeEducationStatus.csv")  
} else {
  f_mdeThirdGradeProficient <- read.csv("./Data/mdeThirdGradeEducationStatus.csv")  
}

# Unemployment Rate ---------------------------------------------------------
if (file.exists("pages/Data/f_unemploymentRateDEED.csv")) {
  f_unemploymentRateDEED <- read.csv("pages/Data/f_unemploymentRateDEED.csv")  
} else {
  f_unemploymentRateDEED <- read.csv("./Data/f_unemploymentRateDEED.csv")  
}

# Child and Teen Checkup ---------------------------------------------------------
if (file.exists("pages/Data/f_PNMChildTeenCheckup.csv")) {
  f_cTcPnm <- read.csv("pages/Data/f_PNMChildTeenCheckup.csv")  
} else {
  f_cTcPnm <- read.csv("./Data/f_PNMChildTeenCheckup.csv")  
}



# Mothers who smoked during pregnancy -------------------------------------------------------

if (file.exists("pages/Data/f_kidsCountMotherSmoke.csv")) {
  f_kidsCountMotherSmoke <- read.csv("pages/Data/f_kidsCountMotherSmoke.csv")  
} else {
  f_kidsCountMotherSmoke <- read.csv("./Data/f_kidsCountMotherSmoke.csv")  
}


# Births to mothers who received late or inadequate prenatal care in Minnesota ---------------------

if (file.exists("pages/Data/f_kidsCountMotherPnc.csv")) {
  f_kidsCountMotherPnc <- read.csv("pages/Data/f_kidsCountMotherPnc.csv")  
} else {
  f_kidsCountMothersMoke <- read.csv("./Data/f_kidsCountMotherPnc.csv")  
}


# Mothers who smoked during pregnancy -------------------------------------------------------

if (file.exists("pages/Data/f_kidsCountMotherSmoke.csv")) {
  f_kidsCountMotherSmoke <- read.csv("pages/Data/f_kidsCountMotherSmoke.csv")  
} else {
  f_kidsCountMotherSmoke <- read.csv("./Data/f_kidsCountMotherSmoke.csv")  
}


# Neonatal Abstinence Syndrome ---------------------

if (file.exists("pages/Data/f_neonatalAbstinenceSyndrome2016_2022.csv")) {
  f_neonatalAbstinenceSyndrome2016_2022 <- read.csv("pages/Data/f_neonatalAbstinenceSyndrome2016_2022.csv")  
} else {
  f_neonatalAbstinenceSyndrome2016_2022 <- read.csv("./Data/f_neonatalAbstinenceSyndrome2016_2022.csv")  
}

# Nonfatal Drug Overdose ---------------------

if (file.exists("pages/Data/f_mdhNonFatalDrugOverdose.csv")) {
  f_mdhNonFatalDrugOverdose <- read.csv("pages/Data/f_mdhNonFatalDrugOverdose.csv")  
} else {
  f_mdhNonFatalDrugOverdose <- read.csv("./Data/f_mdhNonFatalDrugOverdose.csv")  
}

# MSS ---------------------

if (file.exists("pages/Data/f_mss.csv")) {
  f_mss <- read.csv("pages/Data/f_mss.csv")  
} else {
  f_mss <- read.csv("./Data/f_mss.csv")  
}

# STI/HIV ---------------------

if (file.exists("pages/Data/f_stiHIV.csv")) {
  f_stiHIV <- read.csv("pages/Data/f_stiHIV.csv")
} else {
  f_stiHIV <- read.csv("./Data/f_stiHIV.csv")  
}

# Board of Pharmacy PMP ---------------------

if (file.exists("pages/Data/f_boardOfPharmacyPmp.csv")) {
  f_boardOfPharmacyPmp <- read.csv("pages/Data/f_boardOfPharmacyPmp.csv")
} else {
  f_boardOfPharmacyPmp <- read.csv("./Data/f_boardOfPharmacyPmp.csv")  
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

if (file.exists("pages/Data/f_puf.csv")) {
  f_puf <- read.csv("pages/Data/f_puf.csv")
} else {
  f_puf <- read.csv("./Data/f_puf.csv")  
}

# MDH Fatal Drug Overdose --------------

if (file.exists("pages/Data/f_fatalDrugOverdose.csv")) {
  f_fatalDrugOverdose <- read.csv("pages/Data/f_fatalDrugOverdose.csv")
} else {
  f_fatalDrugOverdose <- read.csv("./Data/f_fatalDrugOverdose.csv")  
}
