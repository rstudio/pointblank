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

regex_iban_precheck <- function(country = NULL) {
  
  switch(
    country,
    ALB = "[0-9]{8}[0-9A-Z]{16}",          # Albania
    AND = "[0-9]{8}[0-9A-Z]{12}",          # Andorra
    AUT = "[0-9]{16}",                     # Austria
    BEL = "[0-9]{12}",                     # Belgium
    BIH = "[0-9]{16}",                     # Bosnia and Herzegovina
    BGR = "[A-Z]{4}[0-9]{6}[0-9A-Z]{8}",   # Bulgaria
    HRV = "[0-9]{17}",                     # Croatia
    CYP = "[0-9]{8}[0-9A-Z]{16}",          # Cyprus
    CZE = "[0-9]{20}",                     # The Czech Republic
    DNK = "[0-9]{14}",                     # Denmark
    EST = "[0-9]{16}",                     # Estonia
    FRO = "[0-9]{14}",                     # The Faroe Islands
    FIN = "[0-9]{14}",                     # Finland
    FRA = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # France
    PYF = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # French Polynesia
    ATF = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # French Southern Territories
    GLP = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # Guadeloupe (France)
    MTQ = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # Martinique (France)
    MYT = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # Mayotte (France)
    NCL = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # New Caledonia (France)
    REU = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # Reunion (France)
    BLM = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # Saint-Barthelemy (France)
    MAF = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # Saint Martin (France)
    SPM = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # Saint Pierre et Miquelon (France)
    WLF = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # Wallis and Futuna Islands (France)
    GEO = "[0-9A-Z]{2}[0-9]{16}",          # Georgia
    DEU = "[0-9]{18}",                     # Germany
    GIB = "[A-Z]{4}[0-9A-Z]{15}",          # Gibraltar (United Kingdom)
    GRC = "[0-9]{7}[0-9A-Z]{16}",          # Greece
    GRL = "[0-9]{14}",                     # Greenland (Denmark)
    HUN = "[0-9]{24}",                     # Hungary
    ISL = "[0-9]{22}",                     # Iceland
    IRL = "[0-9A-Z]{4}[0-9]{14}",          # Ireland
    ISR = "[0-9]{19}",                     # Israel
    ITA = "[A-Z][0-9]{10}[0-9A-Z]{12}",    # Italy
    KAZ = "[0-9]{3}[0-9A-Z]{3}[0-9]{10}",  # Kazakhstan
    KWT = "[A-Z]{4}[0-9]{22}",             # Kuwait
    LVA = "[A-Z]{4}[0-9A-Z]{13}",          # Latvia
    LBN = "[0-9]{4}[0-9A-Z]{20}",          # Lebanon
    LIE = "[0-9]{5}[0-9A-Z]{12}",          # Liechtenstein
    LTU = "[0-9]{16}",                     # Lithuania
    LUX = "[0-9]{3}[0-9A-Z]{13}",          # Luxembourg
    MKD = "[0-9]{3}[0-9A-Z]{10}[0-9]{2}",  # North Macedonia
    MLT = "[A-Z]{4}[0-9]{5}[0-9A-Z]{18}",  # Malta
    MRT = "[0-9]{23}",                     # Mauritania
    MUS = "[A-Z]{4}[0-9]{19}[A-Z]{3}",     # Mauritius
    MCO = "[0-9]{10}[0-9A-Z]{11}[0-9]{2}", # Monaco
    MNE = "[0-9]{18}",                     # Montenegro
    NLD = "[A-Z]{4}[0-9]{10}",             # Netherlands
    NOR = "[0-9]{11}",                     # Norway
    POL = "[0-9]{24}",                     # Poland
    PRT = "[0-9]{21}",                     # Portugal
    ROU = "[A-Z]{4}[0-9A-Z]{16}",          # Romania
    SMR = "[A-Z][0-9]{10}[0-9A-Z]{12}",    # San Marino
    SAU = "[0-9]{2}[0-9A-Z]{18}",          # Saudi Arabia
    SRB = "[0-9]{18}",                     # Serbia
    SVK = "[0-9]{20}",                     # Slovakia
    SVN = "[0-9]{15}",                     # Slovenia
    ESP = "[0-9]{20}",                     # Spain
    SWE = "[0-9]{20}",                     # Sweden
    CHE = "[0-9]{5}[0-9A-Z]{12}",          # Switzerland
    TUN = "[0-9]{20}",                     # Tunisia
    TUR = "[0-9]{5}[0-9A-Z]{17}",          # Turkey
    ARE = "[0-9]{19}",                     # The United Arab Emirates
    GBR = "[A-Z]{4}[0-9]{14}",             # The United Kingdom and N. Ireland
    CIV = "[0-9A-Z]{2}[0-9]{22}",          # Cote d'Ivoire
    BRA = "[0-9]{8}[0-9]{5}[0-9]{10}[A-Z]{1}[A-Z0-9]{1}", # Brazil
    NA_character_
  )
}

# nolint end