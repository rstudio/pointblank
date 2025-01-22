#------------------------------------------------------------------------------#
#
#                 _         _    _      _                _
#                (_)       | |  | |    | |              | |
#   _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
#  | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
#  | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   <
#  | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
#  | |
#  |_|
#
#  This file is part of the 'rstudio/pointblank' project.
#
#  Copyright (c) 2017-2025 pointblank authors
#
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
#
#------------------------------------------------------------------------------#


# nolint start

regex_url <- function() {
  "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$"
}

regex_email <- function() {
  "^\\s*[a-zA-Z0-9._%&'*+`/=?^{}~-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z0-9]{2,}\\s*$"
}

regex_ipv4_address <- function() {
  "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"
}

regex_ipv6_address <- function() {
  "^\\s*((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:)))(%.+)?\\s*$"
}

regex_swift_bic <- function() {
  "^[A-Z]{6,6}[A-Z2-9][A-NP-Z0-9]([A-Z0-9]{3,3}){0,1}$"
}

regex_phone <- function() {
  "^\\+?(\\d{0,3})? ?(?(?=\\()(\\(\\d{1,3}\\) ?((\\d{3,5})[. -]?(\\d{4})|(\\d{2}[. -]?){4}))|([. -]?(\\d{1,3}[. -]*)?((\\d{3,5})[. -]?(\\d{4})|(\\d{2}[. -]?){4})))$"
}

regex_mac <- function() {
  "^(([a-f0-9]{2}-){5}[a-f0-9]{2}|([A-F0-9]{2}-){5}[A-Z0-9]{2}|([a-f0-9]{2}:){5}[a-z0-9]{2}|([A-F0-9]{2}:){5}[A-Z0-9]{2})$"
}


regex_iban <- function(country = NULL) {

  if (is.null(country)) {
    country <- "ZZZ"
  }

  switch(
    country,
    AL =, ALB = "^AL[0-9]{2}[0-9]{8}[0-9A-Z]{16}$",          # Albania
    AD =, AND = "^AD[0-9]{2}[0-9]{8}[0-9A-Z]{12}$",          # Andorra
    AE =, ARE = "^AE[0-9]{2}[0-9]{19}$",                     # The United Arab Emirates
    AT =, AUT = "^AT[0-9]{2}[0-9]{16}$",                     # Austria
    BE =, BEL = "^BE[0-9]{2}[0-9]{12}$",                     # Belgium
    BG =, BGR = "^BG[0-9]{2}[A-Z]{4}[0-9]{6}[0-9A-Z]{8}$",   # Bulgaria
    BA =, BIH = "^BA[0-9]{2}[0-9]{16}$",                     # Bosnia and Herzegovina
    BR =, BRA = "^BR[0-9]{2}[0-9]{8}[0-9]{5}[0-9]{10}[A-Z]{1}[A-Z0-9]{1}$", # Brazil
    CH =, CHE = "^CH[0-9]{2}[0-9]{5}[0-9A-Z]{12}$",          # Switzerland
    CI =, CIV = "^CI[0-9]{2}[0-9A-Z]{2}[0-9]{22}$",          # Cote d'Ivoire
    CY =, CYP = "^CY[0-9]{2}[0-9]{8}[0-9A-Z]{16}$",          # Cyprus
    CZ =, CZE = "^CZ[0-9]{2}[0-9]{20}$",                     # The Czech Republic
    DE =, DEU = "^DE[0-9]{2}[0-9]{18}$",                     # Germany
    DK =, DNK = "^DK[0-9]{2}[0-9]{14}$",                     # Denmark
    ES =, ESP = "^ES[0-9]{2}[0-9]{20}$",                     # Spain
    EE =, EST = "^EE[0-9]{2}[0-9]{16}$",                     # Estonia
    FI =, FIN = "^FI[0-9]{2}[0-9]{14}$",                     # Finland
    FR =, FRA = "^FR[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # France
    PF =, PYF = "^PF[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # French Polynesia (FRA)
    TF =, ATF = "^TF[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # French Southern Territories (FRA)
    GP =, GLP = "^GP[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Guadeloupe (FRA)
    MQ =, MTQ = "^MQ[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Martinique (FRA)
    YT =, MYT = "^YT[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Mayotte (FRA)
    NC =, NCL = "^NC[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # New Caledonia (FRA)
    RE =, REU = "^RE[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Reunion (FRA)
    BL =, BLM = "^BL[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Saint-Barthelemy (FRA)
    MF =, MAF = "^MF[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Saint Martin (FRA)
    PM =, SPM = "^PM[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Saint Pierre et Miquelon (FRA)
    WF =, WLF = "^WF[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Wallis and Futuna Islands (FRA)
    FO =, FRO = "^FO[0-9]{2}[0-9]{14}$",                     # The Faroe Islands (DNK)
    GB =, GBR = "^GB[0-9]{2}[A-Z]{4}[0-9]{14}$",             # The United Kingdom and N. Ireland
    GE =, GEO = "^GE[0-9]{2}[0-9A-Z]{2}[0-9]{16}$",          # Georgia
    GI =, GIB = "^GI[0-9]{2}[A-Z]{4}[0-9A-Z]{15}$",          # Gibraltar (United Kingdom)
    GR =, GRC = "^GR[0-9]{2}[0-9]{7}[0-9A-Z]{16}$",          # Greece
    GL =, GRL = "^GL[0-9]{2}[0-9]{14}$",                     # Greenland (Denmark)
    HR =, HRV = "^HR[0-9]{2}[0-9]{17}$",                     # Croatia
    HU =, HUN = "^HU[0-9]{2}[0-9]{24}$",                     # Hungary
    IE =, IRL = "^IE[0-9]{2}[0-9A-Z]{4}[0-9]{14}$",          # Ireland
    IS =, ISL = "^IS[0-9]{2}[0-9]{22}$",                     # Iceland
    IL =, ISR = "^IL[0-9]{2}[0-9]{19}$",                     # Israel
    IT =, ITA = "^IT[0-9]{2}[A-Z][0-9]{10}[0-9A-Z]{12}$",    # Italy
    KZ =, KAZ = "^KZ[0-9]{2}[0-9]{3}[0-9A-Z]{3}[0-9]{10}$",  # Kazakhstan
    KW =, KWT = "^KW[0-9]{2}[A-Z]{4}[0-9]{22}$",             # Kuwait
    LB =, LBN = "^LB[0-9]{2}[0-9]{4}[0-9A-Z]{20}$",          # Lebanon
    LI =, LIE = "^LI[0-9]{2}[0-9]{5}[0-9A-Z]{12}$",          # Liechtenstein
    LT =, LTU = "^LT[0-9]{2}[0-9]{16}$",                     # Lithuania
    LU =, LUX = "^LU[0-9]{2}[0-9]{3}[0-9A-Z]{13}$",          # Luxembourg
    LV =, LVA = "^LV[0-9]{2}[A-Z]{4}[0-9A-Z]{13}$",          # Latvia
    MC =, MCO = "^MC[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Monaco
    MK =, MKD = "^MK[0-9]{2}[0-9]{3}[0-9A-Z]{10}[0-9]{2}$",  # North Macedonia
    MT =, MLT = "^MT[0-9]{2}[A-Z]{4}[0-9]{5}[0-9A-Z]{18}$",  # Malta
    ME =, MNE = "^ME[0-9]{2}[0-9]{18}$",                     # Montenegro
    MR =, MRT = "^MR[0-9]{2}[0-9]{23}$",                     # Mauritania
    MU =, MUS = "^MU[0-9]{2}[A-Z]{4}[0-9]{19}[A-Z]{3}$",     # Mauritius
    NL =, NLD = "^NL[0-9]{2}[A-Z]{4}[0-9]{10}$",             # Netherlands
    NO =, NOR = "^NO[0-9]{2}[0-9]{11}$",                     # Norway
    PL =, POL = "^PL[0-9]{2}[0-9]{24}$",                     # Poland
    PT =, PRT = "^PT[0-9]{2}[0-9]{21}$",                     # Portugal
    RO =, ROU = "^RO[0-9]{2}[A-Z]{4}[0-9A-Z]{16}$",          # Romania
    SA =, SAU = "^SA[0-9]{2}[0-9]{2}[0-9A-Z]{18}$",          # Saudi Arabia
    SM =, SMR = "^SM[0-9]{2}[A-Z][0-9]{10}[0-9A-Z]{12}$",    # San Marino
    RS =, SRB = "^RS[0-9]{2}[0-9]{18}$",                     # Serbia
    SK =, SVK = "^SK[0-9]{2}[0-9]{20}$",                     # Slovakia
    SI =, SVN = "^SI[0-9]{2}[0-9]{15}$",                     # Slovenia
    SE =, SWE = "^SE[0-9]{2}[0-9]{20}$",                     # Sweden
    TN =, TUN = "^TN[0-9]{2}[0-9]{20}$",                     # Tunisia
    TR =, TUR = "^TR[0-9]{2}[0-9]{5}[0-9A-Z]{17}$",          # Turkey
    "[A-Z]{2}[0-9]{2}[0-9A-Z]+"
  )
}

regex_postal_code <- function(country) {

  code_tbl <-
    dplyr::select(countries, alpha_2, alpha_3, postal_code_format)

  if (!(country %in% code_tbl$alpha_2 || country %in% code_tbl$alpha_3)) {
    return(NA_character_)
  }

  if (country %in% code_tbl$alpha_2) {
    code_tbl <- dplyr::filter(code_tbl, alpha_2 == country)
  } else {
    code_tbl <- dplyr::filter(code_tbl, alpha_3 == country)
  }

  postal_code_format <- paste0("^", code_tbl$postal_code_format, "$")

  alpha_3 <- code_tbl$alpha_3

  if (alpha_3 == "IRL") {
    postal_code_format <- "^[0-9A-Z]{3} ?[0-9dA-Z]{4}$"
  }
  if (alpha_3 == "REU") {
    postal_code_format <- "^9[78]4[0-9]{2}$"
  }

  names(postal_code_format) <- alpha_3

  postal_code_format
}

regex_vin <- function() {
  "^[^\\Wioq]{17}$"
}

regex_credit_card_1 <- function() {
  ".*[1-9].*"
}

regex_credit_card_2 <- function() {
  "^[0-9]*$"
}

# nolint end
