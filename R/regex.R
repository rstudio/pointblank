#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


# nolint start

regex_url <- function() {
  "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$"
}

regex_email <- function() {
  "^\\s*[A-Z0-9._%&'*+`/=?^{}~-]+@[A-Z0-9.-]+\\.[A-Z0-9]{2,}\\s*$"
}

regex_ipv4_address <- function() {
  "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"
}

regex_ipv6_address <- function() {
  "^\\s*((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:)))(%.+)?\\s*$"
}

regex_swiftbic <- function() {
  "^[A-Z]{6,6}[A-Z2-9][A-NP-Z0-9]([A-Z0-9]{3,3}){0,1}$"
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
    ALB = "^AL[0-9]{2}[0-9]{8}[0-9A-Z]{16}$",          # Albania
    AND = "^AD[0-9]{2}[0-9]{8}[0-9A-Z]{12}$",          # Andorra
    ARE = "^AE[0-9]{2}[0-9]{19}$",                     # The United Arab Emirates
    AUT = "^AT[0-9]{2}[0-9]{16}$",                     # Austria
    BEL = "^BE[0-9]{2}[0-9]{12}$",                     # Belgium
    BGR = "^BG[0-9]{2}[A-Z]{4}[0-9]{6}[0-9A-Z]{8}$",   # Bulgaria
    BIH = "^BA[0-9]{2}[0-9]{16}$",                     # Bosnia and Herzegovina
    BRA = "^BR[0-9]{2}[0-9]{8}[0-9]{5}[0-9]{10}[A-Z]{1}[A-Z0-9]{1}$", # Brazil
    CHE = "^CH[0-9]{2}[0-9]{5}[0-9A-Z]{12}$",          # Switzerland
    CIV = "^CI[0-9]{2}[0-9A-Z]{2}[0-9]{22}$",          # Cote d'Ivoire
    CYP = "^CY[0-9]{2}[0-9]{8}[0-9A-Z]{16}$",          # Cyprus
    CZE = "^CZ[0-9]{2}[0-9]{20}$",                     # The Czech Republic
    DEU = "^DE[0-9]{2}[0-9]{18}$",                     # Germany
    DNK = "^DK[0-9]{2}[0-9]{14}$",                     # Denmark
    ESP = "^ES[0-9]{2}[0-9]{20}$",                     # Spain
    EST = "^EE[0-9]{2}[0-9]{16}$",                     # Estonia
    FIN = "^FI[0-9]{2}[0-9]{14}$",                     # Finland
    FRA = "^FR[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # France
    PYF = "^PF[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # French Polynesia (FRA)
    ATF = "^TF[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # French Southern Territories (FRA)
    GLP = "^GP[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Guadeloupe (FRA)
    MTQ = "^MQ[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Martinique (FRA)
    MYT = "^YT[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Mayotte (FRA)
    NCL = "^NC[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # New Caledonia (FRA)
    REU = "^RE[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Reunion (FRA)
    BLM = "^BL[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Saint-Barthelemy (FRA)
    MAF = "^MF[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Saint Martin (FRA)
    SPM = "^PM[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Saint Pierre et Miquelon (FRA)
    WLF = "^WF[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Wallis and Futuna Islands (FRA)
    FRO = "^FO[0-9]{2}[0-9]{14}$",                     # The Faroe Islands (DNK)
    GBR = "^GB[0-9]{2}[A-Z]{4}[0-9]{14}$",             # The United Kingdom and N. Ireland
    GEO = "^GE[0-9]{2}[0-9A-Z]{2}[0-9]{16}$",          # Georgia
    GIB = "^GI[0-9]{2}[A-Z]{4}[0-9A-Z]{15}$",          # Gibraltar (United Kingdom)
    GRC = "^GR[0-9]{2}[0-9]{7}[0-9A-Z]{16}$",          # Greece
    GRL = "^GL[0-9]{2}[0-9]{14}$",                     # Greenland (Denmark)
    HRV = "^HR[0-9]{2}[0-9]{17}$",                     # Croatia
    HUN = "^HU[0-9]{2}[0-9]{24}$",                     # Hungary
    IRL = "^IE[0-9]{2}[0-9A-Z]{4}[0-9]{14}$",          # Ireland
    ISL = "^IS[0-9]{2}[0-9]{22}$",                     # Iceland
    ISR = "^IL[0-9]{2}[0-9]{19}$",                     # Israel
    ITA = "^IT[0-9]{2}[A-Z][0-9]{10}[0-9A-Z]{12}$",    # Italy
    KAZ = "^KZ[0-9]{2}[0-9]{3}[0-9A-Z]{3}[0-9]{10}$",  # Kazakhstan
    KWT = "^KW[0-9]{2}[A-Z]{4}[0-9]{22}$",             # Kuwait
    LBN = "^LB[0-9]{2}[0-9]{4}[0-9A-Z]{20}$",          # Lebanon
    LIE = "^LI[0-9]{2}[0-9]{5}[0-9A-Z]{12}$",          # Liechtenstein
    LTU = "^LT[0-9]{2}[0-9]{16}$",                     # Lithuania
    LUX = "^LU[0-9]{2}[0-9]{3}[0-9A-Z]{13}$",          # Luxembourg
    LVA = "^LV[0-9]{2}[A-Z]{4}[0-9A-Z]{13}$",          # Latvia
    MCO = "^MC[0-9]{2}[0-9]{10}[0-9A-Z]{11}[0-9]{2}$", # Monaco
    MKD = "^MK[0-9]{2}[0-9]{3}[0-9A-Z]{10}[0-9]{2}$",  # North Macedonia
    MLT = "^MT[0-9]{2}[A-Z]{4}[0-9]{5}[0-9A-Z]{18}$",  # Malta
    MNE = "^ME[0-9]{2}[0-9]{18}$",                     # Montenegro
    MRT = "^MR[0-9]{2}[0-9]{23}$",                     # Mauritania
    MUS = "^MU[0-9]{2}[A-Z]{4}[0-9]{19}[A-Z]{3}$",     # Mauritius
    NLD = "^NL[0-9]{2}[A-Z]{4}[0-9]{10}$",             # Netherlands
    NOR = "^NO[0-9]{2}[0-9]{11}$",                     # Norway
    POL = "^PL[0-9]{2}[0-9]{24}$",                     # Poland
    PRT = "^PT[0-9]{2}[0-9]{21}$",                     # Portugal
    ROU = "^RO[0-9]{2}[A-Z]{4}[0-9A-Z]{16}$",          # Romania
    SAU = "^SA[0-9]{2}[0-9]{2}[0-9A-Z]{18}$",          # Saudi Arabia
    SMR = "^SM[0-9]{2}[A-Z][0-9]{10}[0-9A-Z]{12}$",    # San Marino
    SRB = "^RS[0-9]{2}[0-9]{18}$",                     # Serbia
    SVK = "^SK[0-9]{2}[0-9]{20}$",                     # Slovakia
    SVN = "^SI[0-9]{2}[0-9]{15}$",                     # Slovenia
    SWE = "^SE[0-9]{2}[0-9]{20}$",                     # Sweden
    TUN = "^TN[0-9]{2}[0-9]{20}$",                     # Tunisia
    TUR = "^TR[0-9]{2}[0-9]{5}[0-9A-Z]{17}$",          # Turkey
    "[A-Z]{2}[0-9]{2}[0-9A-Z]+"
  )
}

# nolint end
