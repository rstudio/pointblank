skip_on_cran()
skip_on_os(os = "windows")

iban_valid <- 
  list(
    AL = c(
      "AL86751639367318444714198669",
      "AL89515635252277023782748302",
      "AL39153296222641598198140883",
      "AL47907501989147671525950076",
      "AL55398719849655505231753964"
    ),
    AD = c(
      "AD2531377125214715353449",
      "AD9764782778017799549345",
      "AD4079739934060166934190",
      "AD3210446914824799260335",
      "AD1781438353588817727122"
    ),
    AT = c(
      "AT582774098454337653",
      "AT220332087576467472",
      "AT328650112318219886",
      "AT193357281080332578",
      "AT535755326448639816"
    ),
    BE = c(
      "BE16517682243567",
      "BE46943937718104",
      "BE75270187592710",
      "BE58465045170210",
      "BE49149522496291"
    ),
    BA = c(
      "BA534130469841865537",
      "BA515388988295860588",
      "BA182655808222815318",
      "BA105531662061034080",
      "BA198940842595891985"
    ),
    BG = c(
      "BG08NXYF73308507056085",
      "BG22OOPG05631394112384",
      "BG30XCMJ43923350257238",
      "BG66ZKSV96204746173581",
      "BG62TOZJ59790808155256"
    ),
    BR = "BR2301137514445981860933159P9",
    HR = c(
      "HR9118658081801951861",
      "HR7093391174762888131",
      "HR6824554207539191367",
      "HR7069604594001692768",
      "HR4270163171014341308"
    ),
    CY = c(
      "CY48590776872388131193566182",
      "CY57511427289148815512463528",
      "CY10469623011747193079305488",
      "CY86826022479357551507194222",
      "CY65139035183710553510799793"
    ),
    CZ = c(
      "CZ3740083988228867633610",
      "CZ4923390395798905071131",
      "CZ3697747307026104738861",
      "CZ5061223246730267064210",
      "CZ3638452806288471256750"
    ),
    DK = c(
      "DK8387188644726815",
      "DK3068706775436067",
      "DK0697654450063121",
      "DK1099979861456738",
      "DK6988299842527195"
    ),
    EE = c(
      "EE416702219844182076",
      "EE816382882633746409",
      "EE035815981173988529",
      "EE150595733987082728",
      "EE605409451030627522"
    ),
    FO = c(
      "FO1593707486505366",
      "FO5006907768039839",
      "FO9378537341306148",
      "FO4068759083981752",
      "FO0905894981715676"
    ),
    FI = c(
      "FI8709549333658747",
      "FI1518471099159022",
      "FI0589161476500751",
      "FI8433982173935580",
      "FI7954405150189238"
    ),
    FR = c(
      "FR4197944644738285027717680",
      "FR9476231310567227640169067",
      "FR6888474339535547405026268",
      "FR3007344050937354660134854",
      "FR8547764510959591053030050"
    ),
    PF = c(
      "PF8169352568136984283973639",
      "PF1021725003919279759512045",
      "PF9067348885442846702112667",
      "PF2110055440192380776287331",
      "PF4462138104308037716665461"
    ),
    TF = c(
      "TF7369356610212036082878842",
      "TF1699071511365858327828309",
      "TF9287657455706592772258930",
      "TF6983084059527026532259346",
      "TF6320136548014311655407753"
    ),
    DE = c(
      "DE06495352657836424132",
      "DE09121688720378475751",
      "DE88516399675378845887",
      "DE42187384985716759572",
      "DE04399340668928275395"
    ),
    GI = c(
      "GI84YQVE742322843673354",
      "GI50TRZE832226672231136",
      "GI96DQBV940980418323607",
      "GI12MTEJ300936244995281",
      "GI50NKEA869461619367593"
    ),
    GE = c(
      "GE27JX0363248286073573",
      "GE95PE2036699405919650",
      "GE86WI1894058889642409",
      "GE50ZK0956993434292828",
      "GE60VX8276008964044900"
    ),
    GR = c(
      "GR8206922880502260960449182",
      "GR0708312360607104632724143",
      "GR3019549951345337224826989",
      "GR1850333105485787816165828",
      "GR3328425960116597801941217"
    ),
    GL = c(
      "GL3357098231928641",
      "GL7672801402871438",
      "GL8576657033000228",
      "GL7533425696727320",
      "GL3145184332080211"
    ),
    HU = c(
      "HU53165228954563006441576439",
      "HU61442178338678431742505774",
      "HU64774233934011029174507108",
      "HU79689064758089616754511009",
      "HU60873329200412252359645504"
    ),
    IS = c(
      "IS098954934397185843549690",
      "IS367580035808668402924142",
      "IS179684724271989278617740",
      "IS846240236716627404368872",
      "IS521951362135843206164749"
    ),
    IE = c(
      "IE49BENI35972029450251",
      "IE37OGUG54280567980573",
      "IE43DCUZ91231044680662",
      "IE15AAKO11199097933967",
      "IE77PIHS49175290558839"
    ),
    IL = c(
      "IL454322198734138455151",
      "IL839799606658366056087",
      "IL038569613554041996868",
      "IL813919026399312117293",
      "IL654645042217944600527"
    ),
    IT = c(
      "IT85M5508898545109326040966",
      "IT52G4674641537627600627273",
      "IT54K9621595703270001697912",
      "IT02F7240326523239438656917",
      "IT75F6444007486118207984348"
    ),
    LV = c(
      "LV85QMUO0600628590552",
      "LV06FYUQ8115346663782",
      "LV05OXNQ0057259369767",
      "LV22XGHZ6356462010762",
      "LV27LLIK8896580861638"
    ),
    LB = c(
      "LB82586807590631203627574587",
      "LB33405622563828555835796785",
      "LB04715710805951055803616185",
      "LB61420797023022242826619522",
      "LB67864629408749872547678117"
    ),
    LI = c(
      "LI4091221689235313176",
      "LI7615336074136062084",
      "LI3727301137968672218",
      "LI3551318446915634574",
      "LI4705272204109186337"
    ),
    LT = c(
      "LT369967216439021801",
      "LT937444321684957069",
      "LT424971109068400772",
      "LT566572547785167976",
      "LT344806290778854389"
    ),
    LU = c(
      "LU292357816107922497",
      "LU184883493877746720",
      "LU850789684146586224",
      "LU365548629753608759",
      "LU954093702688849179"
    ),
    MK = c(
      "MK72125600332161582",
      "MK11033425562019483",
      "MK28337919411434742",
      "MK22345789402386151",
      "MK82644233974800672"
    ),
    MT = c(
      "MT97ATVB58306859106316239974172",
      "MT74VCFO64435204415027820548935",
      "MT35ITGC82712389863518284695353",
      "MT68DQVR03392795978045273628339",
      "MT29SUTJ80635803074721583494800"
    ),
    MU = c(
      "MU51SJFJ6257989899845328236GLS",
      "MU61KWPF5078030841109086598WUO",
      "MU53JZOY7025842098740945151ZDV",
      "MU33GHPP0512367410476524003SGD",
      "MU47IDNI5979337138037202943JSF"
    ),
    YT = c(
      "YT2364450161155207655772895",
      "YT5387838908762423789181088",
      "YT5732176546694896615831590",
      "YT9824037454721994164038623",
      "YT2841514046462334743686132"
    ),
    MC = c(
      "MC7310021462396304214555821",
      "MC7426943447019580313912629",
      "MC5828214954205338889828744",
      "MC5492313829455176982975920",
      "MC9281452662355894512310924"
    ),
    ME = c(
      "ME60043032533135219382",
      "ME13188638660227646081",
      "ME82608318996043837340",
      "ME15121909794401990976",
      "ME76417412116089156198"
    ),
    NL = c(
      "NL23CGPQ0251595242",
      "NL21FPBW0870850199",
      "NL15TVWK0331902885",
      "NL13RTEF0518590011",
      "NL40SGFW1252215983"
    ),
    NC = c(
      "NC9532788614647022310269396",
      "NC2053292379717332255189037",
      "NC9105701404726570049169877",
      "NC1021801619496974025318651",
      "NC1729434258559239166499130"
    ),
    NO = c(
      "NO5384279272034",
      "NO9739077211102",
      "NO7009234138626",
      "NO4448361377130",
      "NO7962522169141"
    ),
    PL = c(
      "PL58515427093787930748060666",
      "PL22980511341176988398762666",
      "PL08239642036391620641611736",
      "PL67449258602191338152126294",
      "PL82771306277364889467742211"
    ),
    PT = c(
      "PT12014625392693045083592",
      "PT80898047569635824751698",
      "PT12065330847682220414039",
      "PT98681708278396096913836",
      "PT49242951581988705914025"
    ),
    RO = c(
      "RO14JLFB9551925334163469",
      "RO81QBBE5290470985636122",
      "RO11VYHO3215271561449480",
      "RO57EAOP2023783320803714",
      "RO21HNFU2813681045796465"
    ),
    PM = c(
      "PM8059411251360674293481450",
      "PM5203212193960732379060042",
      "PM7466602890486264340672969",
      "PM6367055534424386034425612",
      "PM2060260873302215070303208"
    ),
    SM = c(
      "SM70N8724751165335491824812",
      "SM78X1135489836211118891839",
      "SM97M4888143036388138800185",
      "SM72C9584723533916792029340",
      "SM90M9981908196491432695525"
    ),
    SA = c(
      "SA5591720552379162070001",
      "SA0545544944406431392597",
      "SA1667630781004847967711",
      "SA2589813740329129166910",
      "SA7081962486570441251637"
    ),
    RS = c(
      "RS85033307149788542871",
      "RS52665698845368481211",
      "RS82691654340096587307",
      "RS88844660406878687897",
      "RS55472917853273859291"
    ),
    SK = c(
      "SK4167111421162529673536",
      "SK4589732621505695319336",
      "SK4492457066924445710519",
      "SK9190300791649333346556",
      "SK6835978956449243145407"
    ),
    SI = c(
      "SI26085198624502816",
      "SI85363467889027196",
      "SI93016808632808860",
      "SI45000543512611896",
      "SI14647150971707561"
    ),
    ES = c(
      "ES2364265841767173822054",
      "ES5577644480024527929849",
      "ES7502766977729557202723",
      "ES3282395478259622275430",
      "ES9034258324029250165663"
    ),
    SE = c(
      "SE3159169406714737443256",
      "SE2636432381651868407029",
      "SE3280552515152942260664",
      "SE7905464316022155413548",
      "SE8953084170161031273426"
    ),
    CH = c(
      "CH1987364322975299818",
      "CH4269286867620396437",
      "CH2296292579429731980",
      "CH6518929919723772608",
      "CH9093021641139942126"
    ),
    TN = c(
      "TN9670288139885457943736",
      "TN8738524364626879391407",
      "TN7275949269046889239714",
      "TN4006837077003057397517",
      "TN8683931110271287238460"
    ),
    TR = c(
      "TR493318798613751080384953",
      "TR314256533622834177853745",
      "TR080572402207758013538147",
      "TR489116538521358266645727",
      "TR795585070398853758044433"
    ),
    GB = c(
      "GB39MUJS50172570996370",
      "GB14SIPV86193224493527",
      "GB55ZAFY89851748597528",
      "GB22KVUM18028477988401",
      "GB26JAYK66540091518150"
    ),
    WF = c(
      "WF5664222423044623595985593",
      "WF6125565335534356679570561",
      "WF4041383920092945092359281",
      "WF0721812715683400832634716",
      "WF6876262234744814330049391"
    )
  )

postal_code_valid <-
  list(
    AD = c(
      "AD600",
      "AD400",
      "AD500",
      "AD700",
      "AD600"
    ),
    AF = c(
      "1001",
      "1052",
      "1056",
      "2653",
      "2671"
    ),
    AI = c(
      "AI-2640"
    ),
    AL = c(
      "1000",
      "3000",
      "5000",
      "9700",
      "1800"
    ),
    AM = c(
      "0301",
      "0223",
      "0603",
      "0715",
      "1113",
      "3010"
    ),
    AR = c(
      "C1420",
      "T4000",
      "B8000",
      "U9200",
      "E3100",
      "C1043AAL",
      "C1055AAE"
    ),
    AS = c(
      "96799"
    ),
    AT = c(
      "1010",
      "3002",
      "6080",
      "6380",
      "9620"
    ),
    AU = c(
      "2090",
      "2611",
      "3207",
      "6166",
      "7022",
      "0820"
    ),
    AZ = c(
      "1000",
      "4800",
      "5200",
      "8000"
    ),
    BA = c(
      "76213",
      "76204",
      "71122"
    ),
    BB = c(
      "BB15094",
      "BB25001"
    ),
    BD = c(
      "1340",
      "1450",
      "2280",
      "3116",
      "3450",
      "9207"
    ),
    BE = c(
      "1000",
      "2060",
      "4000",
      "6040",
      "7000",
      "7530"
    ),
    BG = c(
      "1000",
      "2080",
      "2400",
      "3900",
      "8970",
      "9960"
    ),
    BH = c(
      "19010"
    ),
    BL = c(
      "97133"
    ),
    BM = c(
      "HM 08",
      "FL06",
      "FL 08",
      "DD 03",
      "HS 02"
    ),
    BN = c(
      "BE3519",
      "BE 1118",
      "BE3119",
      "BG3122",
      "KA1131"
    ),
    BR = c(
      "05508-010",
      "69057-002",
      "91350-200",
      "20940-040"
    ), 
    BT = c(
      "11001",
      "12002",
      "21005",
      "31002",
      "45001",
      "46002"
    ),
    BY = c(
      "202115",
      "202136",
      "211199",
      "225441",
      "246181",
      "247853"
    ),
    CA = c(
      "L6M 3V5",
      "V7G 1V1",
      "B2X 1R5",
      "E2K 1H3",
      "M4Y 3C1",
      "P7C5L3"
    ),
    CC = c(
      "6799"
    ),
    CH = c(
      "1018",
      "1205",
      "2926",
      "6900",
      "7748",
      "8001"
    ),
    CL = c(
      "9170022",
      "1100000",
      "1240000"
    ),
    CN = c(
      "561000",
      "610000",
      "410000",
      "310000",
      "312000"
    ),
    CO = c(
      "110231",
      "270000",
      "702320"
    ),
    CR = c(
      "10101",
      "10104",
      "20208",
      "51003",
      "70304",
      "70605"
    ),
    CU = c(
      "10400",
      "24150",
      "42110",
      "57100",
      "97100"
    ),
    CV = c(
      "5129",
      "9110",
      "9125"
    ), 
    CX = c(
      "6798"
    ),
    CY = c(
      "1095",
      "2360",
      "2867",
      "4150",
      "8882"
    ),
    CZ = c(
      "101 00",
      "162 00",
      "687 67",
      "41762"
    ),
    DE = c(
      "01945",
      "03119",
      "08393",
      "36457",
      "99996"
    ),
    DK = c(
      "3540",
      "4592",
      "4791",
      "8789",
      "8560"
    ),
    DO = c(
      "10101",
      "10113",
      "11905",
      "11512"
    ),
    DZ = c(
      "01001",
      "01036",
      "48017",
      "42023"
    ),
    EC = c(
      "170515"
    ),
    EE = c(
      "12913",
      "19086",
      "50603",
      "66638",
      "74307"
    ),
    EG = c(
      "11001",
      "11519",
      "12411",
      "83719"
    ),
    ES = c(
      "04003",
      "14970",
      "23650",
      "41849",
      "46950",
      "50300"
    ),
    ET = c(
      "1000",
      "3200",
      "5440",
      "6260",
      "7220"
    ),
    FI = c(
      "00240",
      "10900",
      "38840",
      "47540",
      "51460",
      "99990"
    ),
    FK = c(
      "FIQQ 1ZZ"
    ),
    FM = c(
      "96941",
      "96942",
      "96943",
      "96944"
    ),
    FO = c(
      "240",
      "180",
      "236",
      "480",
      "513",
      "960"
    ),
    FR = c(
      "75000",
      "75009",
      "20600",
      "83990",
      "83560",
      "13170"
    ),
    GB = c(
      "PR1 9LY",
      "GIR 0AA"
    ),
    GF = c(
      "97312",
      "97318",
      "97351",
      "97370",
      "97350"
    ),
    GI = c(
      "GX11 1AA"
    ),
    GL = c(
      "3900",
      "3915",
      "3919",
      "3920",
      "3962",
      "3992"
    ),
    GP = c(
      "97100",
      "97131",
      "97136",
      "97181"
    ),
    GR = c(
      "151 24",
      "15110"
    ),
    GT = c(
      "16016",
      "13034",
      "07023",
      "10000",
      "03009"
    ),
    GU = c(
      "96910",
      "96917",
      "96921",
      "96931",
      "96932"
    ),
    HR = c(
      "35420",
      "43000",
      "43270",
      "47220",
      "52000"
    ),
    HT = c(
      "6120"
    ),
    HU = c(
      "1013",
      "2851",
      "3726",
      "7333",
      "9163"
    ),
    ID = c(
      "40115"
    ),
    IE = c(
      "D14 YD91",
      "D6W 3333"
    ),
    IN = c(
      "744301",
      "532291",
      "533229",
      "508224",
      "509406"
    ),
    IO = c(
      "BBND 1ZZ"
    ),
    IQ = c(
      "54001",
      "46013",
      "10001",
      "10081",
      "62001"
    ),
    IR = c(
      "1193653471"
    ), 
    IS = c(
      "104",
      "108",
      "124",
      "360",
      "520",
      "902"
    ),
    IT = c(
      "06072",
      "30013",
      "37139",
      "45010",
      "67010",
      "85052"
    ),
    JP = c(
      "408-0301",
      "408-0307"
    ),
    KR = c(
      "28579",
      "30153",
      "54862",
      "55801",
      "63000",
      "63589"
    ),
    KY = c(
      "KY1-1102",
      "KY2-2001",
      "KY3-2500"
    ),
    LI = c(
      "9485",
      "9491",
      "9492",
      "9493",
      "9494"
    ),
    LK = c(
      "12564",
      "20000",
      "21504",
      "31300",
      "41000",
      "51412",
      "60000"
    ),
    LT = c(
      "41001",
      "58032",
      "62001",
      "64025",
      "71081",
      "99050"
    ),
    LU = c(
      "4906",
      "4939",
      "4997",
      "8218",
      "9767"
    ),
    LV = c(
      "LV-3284",
      "LV-3931",
      "LV-4301",
      "LV-4350",
      "LV-5440"
    ),
    MC = c(
      "98000"
    ),
    MD = c(
      "2088",
      "2055",
      "3504",
      "4026",
      "4621",
      "4731"
    ),
    MH = c(
      "96960",
      "96970"
    ),
    MK = c(
      "1000",
      "1040",
      "1201",
      "2433",
      "6257",
      "7550"
    ),
    MP = c(
      "96950",
      "96951",
      "96952"
    ),
    MQ = c(
      "97214",
      "97218",
      "97280"
    ),
    MX = c(
      "20000",
      "21320",
      "22839",
      "23893",
      "27944",
      "91788",
      "99828"
    ),
    MY = c(
      "09600",
      "15632",
      "30020",
      "50906",
      "70604",
      "79150"
    ),
    NC = c(
      "98822",
      "98826",
      "98840",
      "98880",
      "98884"
    ),
    NE = c(
      "0102",
      "3101"
    ),
    NF = c(
      "2899"
    ),
    NG = c(
      "100001",
      "300001",
      "440001",
      "800001",
      "962001"
    ), 
    NI = c(
      "11001",
      "21002",
      "39105",
      "44303",
      "92408"
    ),
    NL = c(
      "9401 AB",
      "1012 GX",
      "1012GX",
      "7742 LR",
      "9461 AR",
      "4463 KA",
      "4133 DD"
    ),
    NO = c(
      "0255",
      "4375",
      "6509",
      "6521",
      "6514",
      "8804",
      "9935"
    ),
    NP = c(
      "10500",
      "10700",
      "21403",
      "32700",
      "44408",
      "57012"
    ),
    NZ = c(
      "0614",
      "1315",
      "1446",
      "7049",
      "7781",
      "7783",
      "9356"
    ),
    OM = c(
      "618",
      "512",
      "122",
      "314",
      "712"
    ),
    PE = c(
      "LIMA 01",
      "CALLAO 01",
      "02875",
      "01131",
      "02876"
    ),
    PF = c(
      "98709"
    ),
    PG = c(
      "151",
      "336",
      "534",
      "535",
      "553"
    ),
    PH = c(
      "0560",
      "0870",
      "1447",
      "2433",
      "4713",
      "6540",
      "9801"
    ),
    PK = c(
      "10031",
      "17111",
      "19201",
      "23201",
      "34411",
      "39150",
      "96001"
    ), 
    PL = c(
      "56-730",
      "56-210",
      "67-210",
      "72-600",
      "76-611",
      "90-700"
    ),
    PM = c(
      "97500"
    ),
    PN = c(
      "PCRN 1ZZ"
    ),
    PR = c(
      "00610",
      "00623",
      "00703",
      "00725",
      "00919",
      "00988"
    ),
    PT = c(
      "2985-073",
      "3660-606",
      "3750-019",
      "3750-103",
      "7570-693"
    ),
    PW = c(
      "96940"
    ),
    PY = c(
      "2000",
      "3180",
      "4050",
      "6310",
      "7890",
      "9490"
    ),
    RE = c(
      "97403",
      "97413",
      "97441"
    ),
    RO = c(
      "077185",
      "247532",
      "510001",
      "627136",
      "917091"
    ),
    RS = c(
      "11211",
      "15300",
      "22404",
      "24430",
      "36308",
      "37282"
    ),
    RU = c(
      "165171",
      "385001",
      "385020",
      "452181",
      "658001",
      "676020"
    ), 
    SA = c(
      "11564",
      "21577",
      "36362",
      "47711",
      "73311",
      "76321"
    ),
    SD = c(
      "11111",
      "22214",
      "33319",
      "45512",
      "55512",
      "63314"
    ),
    SE = c(
      "106 88",
      "123 05",
      "134 40",
      "175 44",
      "186 24",
      "622 66",
      "734 94"
    ),
    SG = c(
      "018906",
      "049817",
      "058934",
      "088534",
      "100044",
      "829763",
      "918104"
    ),
    SH = c(
      "STHL 1ZZ",
      "ASCN 1ZZ"
    ),
    SI = c(
      "1231",
      "1000",
      "1500",
      "1543",
      "5600",
      "9263"
    ),
    SJ = c(
      "8099",
      "9171",
      "9175",
      "9173",
      "9176",
      "9178"
    ),
    SK = c(
      "974 01",
      "986 01",
      "071 01",
      "044 81",
      "015 01"
    ),
    SM = c(
      "47890",
      "47891",
      "47892",
      "47893",
      "47894",
      "47895",
      "47899"
    ),
    SN = c(
      "12500",
      "16556",
      "46024"
    ),
    SO = c(
      "JH 09010"
    ), 
    SV = c(
      "CP 1120",
      "CP 2118",
      "CP 1328",
      "CP 3210",
      "CP 2217",
      "CP 3419"
    ),
    SZ = c(
      "H100",
      "H113",
      "M201",
      "S404",
      "L316"
    ),
    TC = c(
      "TKCA 1ZZ"
    ),
    TH = c(
      "10230",
      "37110",
      "43100",
      "57290",
      "84000",
      "96150"
    ),
    TJ = c(
      "735450"
    ),
    TM = c(
      "744000"
    ),
    TN = c(
      "2080",
      "2013",
      "8136",
      "1281",
      "1212",
      "9113",
      "2263",
      "1131"
    ),
    TR = c(
      "02700",
      "03600",
      "78600",
      "81950",
      "99970"
    ),
    TW = c(
      "100",
      "800",
      "730",
      "900",
      "950"
    ),
    TZ = c(
      "12210",
      "25204",
      "39100",
      "55105",
      "71200"
    ),
    UA = c(
      "18000",
      "20430",
      "15447",
      "59422",
      "60446",
      "51226"
    ), 
    UM = c(
      "96898"
    ),
    US = c(
      "99553",
      "36264",
      "71660",
      "85225",
      "90309",
      "82084",
      "54161"
    ),
    UY = c(
      "15200",
      "37000",
      "40200",
      "55000",
      "70100",
      "94000"
    ),
    UZ = c(
      "100123",
      "702100"
    ),
    VA = c(
      "00120"
    ),
    VC = c(
      "VC0100",
      "VC0110",
      "VC0120",
      "VC0130"
    ),
    VE = c(
      "2201",
      "2216",
      "2207",
      "2209"
    ),
    VG = c(
      "VG1110",
      "VG1120",
      "VG1130"
    ),
    VI = c(
      "00804",
      "00830",
      "00841",
      "00851"
    ),
    VN = c(
      "90000",
      "77000",
      "65000"
    ),
    WF = c(
      "98600",
      "98610",
      "98620"
    ), 
    YT = c(
      "97600",
      "97605",
      "97625",
      "97660",
      "97680"
    ),
    ZA = c(
      "0007",
      "0455",
      "2031",
      "3680",
      "4309",
      "9986"
    ),
    ZM = c(
      "10101"
    )
  )

credit_card_valid <-
  c(
    "340000000000009",        # American Express
    "378734493671000",        # American Express Corporate
    "6703444444444449",       # Bancontact (BCMC)
    "6703000000000000003",    # Bancontact (BCMC)
    "4035501000000008",       # Cartes Bancaires
    "5019717010103742",       # Dankort
    "6011000000000004",       # Discover
    "6011000990139424",       # Discover
    "38520000023237",         # Diners
    "5066991111111118",       # Elo
    "201400000000009",        # enRoute
    "6062828888666688",       # Hipercard
    "2131000000000008",       # JCB
    "3530111333300000",       # JCB
    "5500000000000004",       # MasterCard
    "5105105105105100",       # MasterCard
    "6334000000000004",       # Solo
    "4903010000000009",       # Switch
    "4012888888881881",       # Visa
    "4222222222222",          # Visa
    "4484600000000004",       # Visa Fleet Credit
    "6304100000000008",       # Laser
    "4917300800000000",       # Visa Electron
    "4001020000000009",       # Visa Electron
    "6799990100000000019",    # Maestro
    "6759649826438453",       # Maestro
    "6007220000000004",       # Forbrugsforeningen
    "135410014004955",        # UATP
    "6271136264806203568",    # UnionPay
    "4013250000000000006"     # V Pay
  )

credit_card_invalid <-
  c(
    "ABCDEFJHIGK",
    "abcdefghijk",
    "CE1EL2LLFFF",
    "E31DCLLFFF",
    "340000000000000",
    "378734493671001",
    "6703444444444440",
    "6703000000000000004",
    "4035501000000009",
    "5019717010103743",
    "6011000000000005",
    "6011000990139425",
    "38520000023238",
    "5066991111111119",
    "201400000000000",
    "6062828888666689",
    "2131000000000009",
    "3530111333300001",
    "5500000000000005",
    "5105105105105101",
    "6334000000000005",
    "4903010000000000",
    "4012888888881882",
    "4222222222223",
    "4484600000000005",
    "6304100000000009",
    "4917300800000001",
    "4001020000000000",
    "6799990100000000010",
    "6759649826438454",
    "6007220000000005",
    "135410014004956",
    "6271136264806203569",
    "4013250000000000007"
  )

vin_valid <-
  c(
    "1FTEW1E41KKD70581",
    "ZARBAAB46LM355009",
    "JTEBH3FJ60K093139",
    "1HD1FS4178Y631180",
    "WMWXP7C5XF2A33854",
    "2CNFLNEY6A6356759",
    "1GTR2VE78CZ109794",
    "WDBRF64J63F357663",
    "WAUKMAF47JA138978",
    "1FAHP2FW8BG154879",
    "4T1BE32K46U663238",
    "SAJWA82BX9SH30556",
    "JTMBFREV80D193233",
    "1XP5DB9XX1N551710",
    "JTMZD33V585094329",
    "1J4GW48S42C322033",
    "KM8K33A56JU088201",
    "1M8GDM9AXKP042788",
    "1HGBH41JXMN109186",
    "WAUZZZF49HA036784"
  )

vin_invalid <-
  c(
    "7A8GK4M0706100372",
    "WVWZZZ1KZ7U022191",
    "JHLRE48507C210824",
    "JM0BK10F100120307",
    "KL3CD266JDB091630"
  )

isbn_10_valid <-
  c(
    "1101907932",
    "1101907878",
    "1101907967",
    "0375712356",
    "0307957802",
    "0307268217",
    "0307264882",
    "0375407928",
    "0679417214",
    "0679409866",
    "0679409882",
    "0679412697",
    "0679409874",
    "067940581X",
    "0679405828",
    "0679405437",
    "0679405429"
  )

isbn_13_valid <-
  c(
    "978-1101907931",
    "978-1101907870",
    "978-1101907962",
    "978-0375712357",
    "978-0307957801",
    "978-0307268211",
    "978-0307264886",
    "978-0375407925",
    "978-0679417217",
    "978-0679409861",
    "978-0679409885",
    "978-1854223579",
    "978-0679409878",
    "978-0679405818",
    "978-0679405825",
    "978-0679405436",
    "978-0679405429"
  )

isbn_10_invalid <-
  c(
    "1101907931",
    "1101907879",
    "110190796x",
    "0375712358",
    "0307957800",
    "0307268211",
    "0307264881",
    "0375407923",
    "0679417212",
    "0679409865",
    "0679409881",
    "0679412698",
    "0679409875",
    "0679405811",
    "0679405820",
    "067940543X",
    "0679405422"
  )

isbn_13_invalid <-
  c(
    "978-1101907930",
    "978-1101907871",
    "978-1101907963",
    "978-0375712358",
    "978-0307957802",
    "978-0307268213",
    "978-0307264887",
    "978-0375407922",
    "978-0679417214",
    "978-0679409868",
    "978-0679409881",
    "978-1854013579",
    "978-0679409871",
    "978-0679405812",
    "978-0679405823",
    "978-0679405438",
    "978-0679405422"
  )

swift_bic_valid <-
  c(
    "RBOSGGSX",
    "RZTIAT22263",
    "BCEELULL",
    "MARKDEFF",
    "GENODEF1JEV",
    "UBSWCHZH80A",
    "CEDELULLXXX",
    "ABNANL2A"
  )

swift_bic_invalid <-
  c(
    "CE1EL2LLFFF",
    "E31DCLLFFF",
    "ABNANL13"
  )

phone_valid <-
  c(
    "+5-555-555-5555",
    "+5 555 555 5555",
    "+5.555.555.5555",
    "5-555-555-5555",
    "5.555.555.5555",
    "5 555 555 5555",
    "555.555.5555",
    "555 555 5555",
    "555-555-5555",
    "555-5555555",
    "0123456789",
    "5(555)555.5555",
    "+5(555)555.5555",
    "+5(555)555 5555",
    "+5(555)555-5555",
    "+5(555)5555555",
    "(555)5555555",
    "(555)555.5555",
    "(555)555-5555",
    "(555) 555 5555",
    "55555555555",
    "5555555555",
    "+33(1)2222222",
    "+33(1)222 2222",
    "+33(1)222.2222",
    "+33(1)22 22 22 22",
    "33(1)2222222",
    "33(1)22222222",
    "33(1)22 22 22 22",
    "(020) 7476 4026",
    "33(020) 7777 7777",
    "33(020)7777 7777",
    "+33(020) 7777 7777",
    "+33(020)7777 7777",
    "03-6106666",
    "036106666",
    "+33(11) 97777 7777",
    "+3311977777777",
    "11977777777",
    "11 97777 7777",
    "(11) 97777 7777",
    "(11) 97777-7777",
    "555-5555",
    "5555555",
    "555.5555",
    "555 5555",
    "+1 (555) 555 5555"
  )

phone_invalid <-
  c(
    "",
    "123",
    "(11- 97777-7777",
    "-11) 97777-7777",
    "s555-5555",
    "555-555",
    "555555",
    "555+5555",
    "(555)555555",
    "(555)55555",
    "+(555)555 555",
    "+5(555)555 555",
    "+5(555)555 555 555",
    "555)555 555",
    "+5(555)5555 555",
    "(555)55 555",
    "(555)5555 555",
    "+5(555)555555",
    "5(555)55 55555",
    "(5)555555",
    "+55(5)55 5 55 55",
    "+55(5)55 55 55 5",
    "+55(5)55 55 55",
    "+55(5)5555 555",
    "+55()555 5555",
    "03610666-5",
    "text",
    "99999999999999999999999999999999999999999999999999999999",
    "888888888888888888888888888888888888888888888888888",
    "7777777777777777777777777777777777777777777777",
    "66666666666666666666666666666666666666666",
    "555555555555555555555555555555555555",
    "4444444444444444444444444444444",
    "33333333333333333333333333",
    "222222222222222222222",
    "1111111111111111",
    "555\n5555"
  )

mac_valid <-
  c(
    "01-2d-4c-ef-89-ab",
    "01-2D-4C-EF-89-AB",
    "01:2d:4c:ef:89:ab",
    "01:2D:4C:EF:89:AB",
    "01-2d-4c-ef-89-59",
    "ff-2d-4c-ef-89-59"
  )

mac_invalid <-
  c(
    "999999999",
    "9999.9999",
    "01-2d-4c-ef-89-ab-06",
    "01-2d:4c-ef:89-ab",
    "01-2d-4c-EF-89-ab",
    "01-2d-4C-ef-89-ab",
    "01-2dc-4c-ef-89-ab",
    "text"
  )

email_valid <-
  c(
    "test@test.com",
    "mail+mail@example.com",
    "mail.email@e.test.com",
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@letters-in-local.org",
    "01234567890@numbers-in-local.net",
    "&'*+-./=?^_{}~@other-valid-characters-in-local.net",
    "mixed-1234-in-{+^}-local@sld.net",
    "a@single-character-in-local.org",
    "one-character-third-level@a.example.com",
    "single-character-in-sld@x.org",
    "local@dash-in-sld.com",
    "letters-in-sld@123.com",
    "one-letter-sld@x.org",
    "test@test--1.com",
    "uncommon-tld@sld.museum",
    "uncommon-tld@sld.travel",
    "uncommon-tld@sld.mobi",
    "country-code-tld@sld.uk",
    "country-code-tld@sld.rw",
    "local@sld.newTLD",
    "the-total-length@of-an-entire-address.cannot-be-longer-than-two-hundred-and-fifty-four-characters.and-this-address-is-254-characters-exactly.so-it-should-be-valid.and-im-going-to-add-some-more-words-here.to-increase-the-lenght-blah-blah-blah-blah-bla.org",
    "the-character-limit@for-each-part.of-the-domain.is-sixty-three-characters.this-is-exactly-sixty-three-characters-so-it-is-valid-blah-blah.com",
    "local@sub.domains.com",
    "backticks`are`legit@test.com",
    "digit-only-domain@123.com",
    "digit-only-domain-with-subdomain@sub.123.com",
    "`a@a.fr",
    "`aa@fr.com",
    "com@sil.c1m",
    "t119037jskc_ihndkdoz@aakctgajathzffcsuqyjhgjuxnuulgnhxtnbquwtgxljfayeestsjdbalthtddy.lgtmsdhywswlameglunsaplsblljavswxrltovagexhtttodqedmicsekvpmpuu.pgjvdmvzyltpixvalfbktnnpjyjqswbfvtpbfsngqtmhgamhrbqqvyvlhqigggv.nxqglspfbwdhtfpibcrccvctmoxuxwlunghhwacjtrclgirrgppvshxvrzkoifl"
  )

email_invalid <-
  c(
    "",
    "test",
    "@test.com",
    "invalid-characters-in-sld@! \"#$%(),/;<>_[]`|.org",
    "(),:;`|@more-invalid-characters-in-local.org",
    "<>@[]\\`|@even-more-invalid-characters-in-local.org",
    "partially.\"quoted\"@sld.com",
    "missing-dot-before-tld@com",
    "mail.example.com",
    "mail@example",
    "test.test@",
    "test@example.com.",
    "plainaddress",
    "missing-at-sign.net",
    "#@%^%#$@#$@#.com",
    "Joe Smith <email@example.com>",
    "mail@example.com (Joe Smith)",
    "mg@ns.i",
    "double@a@com"
  )

url_valid <-
  c(
    "http://foo.com/blah_blah",
    "http://foo.com/blah_blah/",
    "http://foo.com/blah_blah_(wikipedia)",
    "http://\u2605foo.com/blah_blah_(wikipedia)_(again)",
    "http://www.example.com/wpstyle/?p=364",
    "https://www.example.com/foo/?bar=baz&inga=42&quux",
    "http://userid:password@example.com:8080",
    "http://userid:password@example.com:8080/",
    "http://userid@example.com",
    "http://userid@example.com/",
    "http://userid@example.com:8080",
    "http://userid@example.com:8080/",
    "http://userid:password@example.com",
    "http://userid:password@example.com/",
    "http://foo.com/blah_(wikipedia)#cite-1",
    "http://foo.com/blah_(wikipedia)_blah#cite-1",
    "http://foo.com/(something)?after=parens",
    "http://code.google.com/events/#&product=browser",
    "http://j.mp",
    "ftp://foo.bar/baz",
    "http://foo.bar/?q=Test%20URL-encoded%20stuff",
    "http://-.~_!$&'()*+,;=:%40:80%2f::::::@example.com",
    "http://1337.net",
    "http://a.b-c.de",
    "http://223.255.255.254"
  )

url_invalid <-
  c(
    "http://",
    "http://.",
    "http://..",
    "http://../",
    "http://?",
    "http://??",
    "http://??/",
    "http://#",
    "http://##",
    "http://##/",
    "http://foo.bar?q=Spaces should be encoded",
    "//",
    "//a",
    "///a",
    "///",
    "http:///a",
    "foo.com",
    "rdar://1234",
    "h://test",
    "http:// shouldfail.com",
    ":// should fail",
    "http://foo.bar/foo(bar)baz quux",
    "ftps://foo.bar/",
    "http://-error-.invalid/",
    "http://-a.b.co",
    "http://a.b-.co",
    "http://0.0.0.0",
    "http://3628126748",
    "http://.www.foo.bar/",
    "http://www.foo.bar./",
    "http://.www.foo.bar./"
  )

ipv4_address_valid <-
  c(
    "93.184.220.20",
    "161.148.172.130",
    "161.148.172.130",
    "73.194.66.94",
    "60.92.167.193",
    "92.168.1.1",
    "0.0.0.0",
    "55.255.255.255"
  )

ipv4_address_invalid <-
  c(
    "000.000.000.000",
    "256.255.255.255",
    "2001:0db8:0000:85a3:0000:0000:ac1f:8001",
    "2001:db8:0:85a3:0:0:ac1f:8001",
    ""
  )

ipv6_address_valid <-
  c(
    "2001:0db8:0000:85a3:0000:0000:ac1f:8001",
    "2001:db8:0:85a3:0:0:ac1f:8001"
  )

ipv6_address_invalid <-
  c(
    "0db8:0000:85a3:0000:0000:ac1f:8001",
    "2001:0db8:0000:85a3:0000:0000:ac1f",
    "93.184.220.20",
    "161.148.172.130",
    "161.148.172.130",
    "73.194.66.94",
    "60.92.167.193",
    "92.168.1.1",
    "0.0.0.0",
    "55.255.255.255",
    ""
  )

test_that("IBAN numbers can be successfully validated", {
  
  # Check all IBAN numbers for weak validity (i.e, validity not
  # based on country-specific features)
  expect_true(all(check_iban(unname(unlist(iban_valid)))))
  
  # Check IBAN numbers from different countries
  expect_true(all(check_iban(iban_valid[["AL"]], country = "AL")))
  expect_true(all(check_iban(iban_valid[["AD"]], country = "AD")))
  expect_true(all(check_iban(iban_valid[["AT"]], country = "AT")))
  expect_true(all(check_iban(iban_valid[["BE"]], country = "BE")))
  expect_true(all(check_iban(iban_valid[["BA"]], country = "BA")))
  expect_true(all(check_iban(iban_valid[["BG"]], country = "BG")))
  expect_true(all(check_iban(iban_valid[["BR"]], country = "BR")))
  expect_true(all(check_iban(iban_valid[["HR"]], country = "HR")))
  expect_true(all(check_iban(iban_valid[["CY"]], country = "CY")))
  expect_true(all(check_iban(iban_valid[["CZ"]], country = "CZ")))
  expect_true(all(check_iban(iban_valid[["DK"]], country = "DK")))
  expect_true(all(check_iban(iban_valid[["EE"]], country = "EE")))
  expect_true(all(check_iban(iban_valid[["FO"]], country = "FO")))
  expect_true(all(check_iban(iban_valid[["FI"]], country = "FI")))
  expect_true(all(check_iban(iban_valid[["FR"]], country = "FR")))
  expect_true(all(check_iban(iban_valid[["PF"]], country = "PF")))
  expect_true(all(check_iban(iban_valid[["TF"]], country = "TF")))
  expect_true(all(check_iban(iban_valid[["DE"]], country = "DE")))
  expect_true(all(check_iban(iban_valid[["GI"]], country = "GI")))
  expect_true(all(check_iban(iban_valid[["GE"]], country = "GE")))
  expect_true(all(check_iban(iban_valid[["GR"]], country = "GR")))
  expect_true(all(check_iban(iban_valid[["GL"]], country = "GL")))
  expect_true(all(check_iban(iban_valid[["HU"]], country = "HU")))
  expect_true(all(check_iban(iban_valid[["IS"]], country = "IS")))
  expect_true(all(check_iban(iban_valid[["IE"]], country = "IE")))
  expect_true(all(check_iban(iban_valid[["IL"]], country = "IL")))
  expect_true(all(check_iban(iban_valid[["IT"]], country = "IT")))
  expect_true(all(check_iban(iban_valid[["LV"]], country = "LV")))
  expect_true(all(check_iban(iban_valid[["LB"]], country = "LB")))
  expect_true(all(check_iban(iban_valid[["LI"]], country = "LI")))
  expect_true(all(check_iban(iban_valid[["LT"]], country = "LT")))
  expect_true(all(check_iban(iban_valid[["LU"]], country = "LU")))
  expect_true(all(check_iban(iban_valid[["MK"]], country = "MK")))
  expect_true(all(check_iban(iban_valid[["MT"]], country = "MT")))
  expect_true(all(check_iban(iban_valid[["MU"]], country = "MU")))
  expect_true(all(check_iban(iban_valid[["YT"]], country = "YT")))
  expect_true(all(check_iban(iban_valid[["MC"]], country = "MC")))
  expect_true(all(check_iban(iban_valid[["ME"]], country = "ME")))
  expect_true(all(check_iban(iban_valid[["NL"]], country = "NL")))
  expect_true(all(check_iban(iban_valid[["NC"]], country = "NC")))
  expect_true(all(check_iban(iban_valid[["NO"]], country = "NO")))
  expect_true(all(check_iban(iban_valid[["PL"]], country = "PL")))
  expect_true(all(check_iban(iban_valid[["PT"]], country = "PT")))
  expect_true(all(check_iban(iban_valid[["RO"]], country = "RO")))
  expect_true(all(check_iban(iban_valid[["PM"]], country = "PM")))
  expect_true(all(check_iban(iban_valid[["SM"]], country = "SM")))
  expect_true(all(check_iban(iban_valid[["SA"]], country = "SA")))
  expect_true(all(check_iban(iban_valid[["RS"]], country = "RS")))
  expect_true(all(check_iban(iban_valid[["SK"]], country = "SK")))
  expect_true(all(check_iban(iban_valid[["SI"]], country = "SI")))
  expect_true(all(check_iban(iban_valid[["ES"]], country = "ES")))
  expect_true(all(check_iban(iban_valid[["SE"]], country = "SE")))
  expect_true(all(check_iban(iban_valid[["CH"]], country = "CH")))
  expect_true(all(check_iban(iban_valid[["TN"]], country = "TN")))
  expect_true(all(check_iban(iban_valid[["TR"]], country = "TR")))
  expect_true(all(check_iban(iban_valid[["GB"]], country = "GB")))
  expect_true(all(check_iban(iban_valid[["WF"]], country = "WF")))
})

test_that("Postal codes can be successfully validated", {
  
  expect_true(all(check_postal_code(postal_code_valid[["AD"]], country = "AD")))
  expect_true(all(check_postal_code(postal_code_valid[["AF"]], country = "AF")))
  expect_true(all(check_postal_code(postal_code_valid[["AI"]], country = "AI"))) 
  expect_true(all(check_postal_code(postal_code_valid[["AL"]], country = "AL")))
  expect_true(all(check_postal_code(postal_code_valid[["AM"]], country = "AM")))
  expect_true(all(check_postal_code(postal_code_valid[["AR"]], country = "AR")))
  expect_true(all(check_postal_code(postal_code_valid[["AS"]], country = "AS")))
  expect_true(all(check_postal_code(postal_code_valid[["AT"]], country = "AT")))
  expect_true(all(check_postal_code(postal_code_valid[["AU"]], country = "AU")))
  expect_true(all(check_postal_code(postal_code_valid[["AZ"]], country = "AZ")))
  expect_true(all(check_postal_code(postal_code_valid[["BA"]], country = "BA")))
  expect_true(all(check_postal_code(postal_code_valid[["BB"]], country = "BB")))
  expect_true(all(check_postal_code(postal_code_valid[["BD"]], country = "BD")))
  expect_true(all(check_postal_code(postal_code_valid[["BE"]], country = "BE")))
  expect_true(all(check_postal_code(postal_code_valid[["BL"]], country = "BL")))
  expect_true(all(check_postal_code(postal_code_valid[["BM"]], country = "BM")))
  expect_true(all(check_postal_code(postal_code_valid[["BN"]], country = "BN")))
  expect_true(all(check_postal_code(postal_code_valid[["BR"]], country = "BR")))
  expect_true(all(check_postal_code(postal_code_valid[["BT"]], country = "BT")))
  expect_true(all(check_postal_code(postal_code_valid[["BY"]], country = "BY")))
  expect_true(all(check_postal_code(postal_code_valid[["CA"]], country = "CA")))
  expect_true(all(check_postal_code(postal_code_valid[["CC"]], country = "CC")))
  expect_true(all(check_postal_code(postal_code_valid[["CH"]], country = "CH")))
  expect_true(all(check_postal_code(postal_code_valid[["CL"]], country = "CL")))
  expect_true(all(check_postal_code(postal_code_valid[["CN"]], country = "CN")))
  expect_true(all(check_postal_code(postal_code_valid[["CO"]], country = "CO")))
  expect_true(all(check_postal_code(postal_code_valid[["CR"]], country = "CR")))
  expect_true(all(check_postal_code(postal_code_valid[["CU"]], country = "CU")))
  expect_true(all(check_postal_code(postal_code_valid[["CV"]], country = "CV")))
  expect_true(all(check_postal_code(postal_code_valid[["CX"]], country = "CX")))
  expect_true(all(check_postal_code(postal_code_valid[["CY"]], country = "CY")))
  expect_true(all(check_postal_code(postal_code_valid[["CZ"]], country = "CZ")))
  expect_true(all(check_postal_code(postal_code_valid[["DE"]], country = "DE")))
  expect_true(all(check_postal_code(postal_code_valid[["DK"]], country = "DK")))
  expect_true(all(check_postal_code(postal_code_valid[["DO"]], country = "DO")))
  expect_true(all(check_postal_code(postal_code_valid[["DZ"]], country = "DZ")))
  expect_true(all(check_postal_code(postal_code_valid[["EC"]], country = "EC")))
  expect_true(all(check_postal_code(postal_code_valid[["EE"]], country = "EE")))
  expect_true(all(check_postal_code(postal_code_valid[["EG"]], country = "EG")))
  expect_true(all(check_postal_code(postal_code_valid[["ES"]], country = "ES")))
  expect_true(all(check_postal_code(postal_code_valid[["ET"]], country = "ET")))
  expect_true(all(check_postal_code(postal_code_valid[["FI"]], country = "FI")))
  expect_true(all(check_postal_code(postal_code_valid[["FK"]], country = "FK")))
  expect_true(all(check_postal_code(postal_code_valid[["FM"]], country = "FM")))
  expect_true(all(check_postal_code(postal_code_valid[["FO"]], country = "FO")))
  expect_true(all(check_postal_code(postal_code_valid[["FR"]], country = "FR")))
  expect_true(all(check_postal_code(postal_code_valid[["GB"]], country = "GB")))
  expect_true(all(check_postal_code(postal_code_valid[["GF"]], country = "GF")))
  expect_true(all(check_postal_code(postal_code_valid[["GG"]], country = "GG")))
  expect_true(all(check_postal_code(postal_code_valid[["GI"]], country = "GI")))
  expect_true(all(check_postal_code(postal_code_valid[["GL"]], country = "GL")))
  expect_true(all(check_postal_code(postal_code_valid[["GP"]], country = "GP")))
  expect_true(all(check_postal_code(postal_code_valid[["GR"]], country = "GR")))
  expect_true(all(check_postal_code(postal_code_valid[["GT"]], country = "GT")))
  expect_true(all(check_postal_code(postal_code_valid[["GU"]], country = "GU")))
  expect_true(all(check_postal_code(postal_code_valid[["HR"]], country = "HR")))
  expect_true(all(check_postal_code(postal_code_valid[["HT"]], country = "HT")))
  expect_true(all(check_postal_code(postal_code_valid[["HU"]], country = "HU")))
  expect_true(all(check_postal_code(postal_code_valid[["ID"]], country = "ID")))
  expect_true(all(check_postal_code(postal_code_valid[["IE"]], country = "IE")))
  expect_true(all(check_postal_code(postal_code_valid[["IN"]], country = "IN")))
  expect_true(all(check_postal_code(postal_code_valid[["IO"]], country = "IO")))
  expect_true(all(check_postal_code(postal_code_valid[["IQ"]], country = "IQ")))
  expect_true(all(check_postal_code(postal_code_valid[["IR"]], country = "IR")))
  expect_true(all(check_postal_code(postal_code_valid[["IS"]], country = "IS")))
  expect_true(all(check_postal_code(postal_code_valid[["IT"]], country = "IT")))
  expect_true(all(check_postal_code(postal_code_valid[["JP"]], country = "JP")))
  expect_true(all(check_postal_code(postal_code_valid[["KR"]], country = "KR")))
  expect_true(all(check_postal_code(postal_code_valid[["KY"]], country = "KY")))
  expect_true(all(check_postal_code(postal_code_valid[["LI"]], country = "LI")))
  expect_true(all(check_postal_code(postal_code_valid[["LK"]], country = "LK")))
  expect_true(all(check_postal_code(postal_code_valid[["LT"]], country = "LT")))
  expect_true(all(check_postal_code(postal_code_valid[["LU"]], country = "LU")))
  expect_true(all(check_postal_code(postal_code_valid[["LV"]], country = "LV")))
  expect_true(all(check_postal_code(postal_code_valid[["MC"]], country = "MC")))
  expect_true(all(check_postal_code(postal_code_valid[["MD"]], country = "MD")))
  expect_true(all(check_postal_code(postal_code_valid[["MH"]], country = "MH")))
  expect_true(all(check_postal_code(postal_code_valid[["MK"]], country = "MK")))
  expect_true(all(check_postal_code(postal_code_valid[["MP"]], country = "MP")))
  expect_true(all(check_postal_code(postal_code_valid[["MQ"]], country = "MQ")))
  expect_true(all(check_postal_code(postal_code_valid[["MX"]], country = "MX")))
  expect_true(all(check_postal_code(postal_code_valid[["MY"]], country = "MY")))
  expect_true(all(check_postal_code(postal_code_valid[["NC"]], country = "NC")))
  expect_true(all(check_postal_code(postal_code_valid[["NE"]], country = "NE")))
  expect_true(all(check_postal_code(postal_code_valid[["NF"]], country = "NF")))
  expect_true(all(check_postal_code(postal_code_valid[["NG"]], country = "NG")))
  expect_true(all(check_postal_code(postal_code_valid[["NI"]], country = "NI")))
  expect_true(all(check_postal_code(postal_code_valid[["NL"]], country = "NL")))
  expect_true(all(check_postal_code(postal_code_valid[["NO"]], country = "NO")))
  expect_true(all(check_postal_code(postal_code_valid[["NP"]], country = "NP")))
  expect_true(all(check_postal_code(postal_code_valid[["NZ"]], country = "NZ")))
  expect_true(all(check_postal_code(postal_code_valid[["OM"]], country = "OM")))
  expect_true(all(check_postal_code(postal_code_valid[["PE"]], country = "PE")))
  expect_true(all(check_postal_code(postal_code_valid[["PF"]], country = "PF")))
  expect_true(all(check_postal_code(postal_code_valid[["PG"]], country = "PG")))
  expect_true(all(check_postal_code(postal_code_valid[["PH"]], country = "PH")))
  expect_true(all(check_postal_code(postal_code_valid[["PK"]], country = "PK")))
  expect_true(all(check_postal_code(postal_code_valid[["PL"]], country = "PL")))
  expect_true(all(check_postal_code(postal_code_valid[["PM"]], country = "PM")))
  expect_true(all(check_postal_code(postal_code_valid[["PN"]], country = "PN")))
  expect_true(all(check_postal_code(postal_code_valid[["PR"]], country = "PR")))
  expect_true(all(check_postal_code(postal_code_valid[["PT"]], country = "PT")))
  expect_true(all(check_postal_code(postal_code_valid[["PW"]], country = "PW")))
  expect_true(all(check_postal_code(postal_code_valid[["PY"]], country = "PY")))
  expect_true(all(check_postal_code(postal_code_valid[["RE"]], country = "RE")))
  expect_true(all(check_postal_code(postal_code_valid[["RO"]], country = "RO")))
  expect_true(all(check_postal_code(postal_code_valid[["RS"]], country = "RS")))
  expect_true(all(check_postal_code(postal_code_valid[["RU"]], country = "RU")))
  expect_true(all(check_postal_code(postal_code_valid[["SA"]], country = "SA")))
  expect_true(all(check_postal_code(postal_code_valid[["SD"]], country = "SD")))
  expect_true(all(check_postal_code(postal_code_valid[["SE"]], country = "SE")))
  expect_true(all(check_postal_code(postal_code_valid[["SG"]], country = "SG")))
  expect_true(all(check_postal_code(postal_code_valid[["SH"]], country = "SH")))
  expect_true(all(check_postal_code(postal_code_valid[["SI"]], country = "SI")))
  expect_true(all(check_postal_code(postal_code_valid[["SJ"]], country = "SJ")))
  expect_true(all(check_postal_code(postal_code_valid[["SK"]], country = "SK")))
  expect_true(all(check_postal_code(postal_code_valid[["SM"]], country = "SM")))
  expect_true(all(check_postal_code(postal_code_valid[["SN"]], country = "SN")))
  expect_true(all(check_postal_code(postal_code_valid[["SO"]], country = "SO")))
  expect_true(all(check_postal_code(postal_code_valid[["SV"]], country = "SV")))
  expect_true(all(check_postal_code(postal_code_valid[["SZ"]], country = "SZ")))
  expect_true(all(check_postal_code(postal_code_valid[["TC"]], country = "TC")))
  expect_true(all(check_postal_code(postal_code_valid[["TH"]], country = "TH")))
  expect_true(all(check_postal_code(postal_code_valid[["TJ"]], country = "TJ")))
  expect_true(all(check_postal_code(postal_code_valid[["TM"]], country = "TM")))
  expect_true(all(check_postal_code(postal_code_valid[["TN"]], country = "TN")))
  expect_true(all(check_postal_code(postal_code_valid[["TR"]], country = "TR")))
  expect_true(all(check_postal_code(postal_code_valid[["TW"]], country = "TW")))
  expect_true(all(check_postal_code(postal_code_valid[["TZ"]], country = "TZ")))
  expect_true(all(check_postal_code(postal_code_valid[["UA"]], country = "UA")))
  expect_true(all(check_postal_code(postal_code_valid[["UM"]], country = "UM")))
  expect_true(all(check_postal_code(postal_code_valid[["US"]], country = "US")))
  expect_true(all(check_postal_code(postal_code_valid[["UY"]], country = "UY")))
  expect_true(all(check_postal_code(postal_code_valid[["UZ"]], country = "UZ")))
  expect_true(all(check_postal_code(postal_code_valid[["VA"]], country = "VA")))
  expect_true(all(check_postal_code(postal_code_valid[["VC"]], country = "VC")))
  expect_true(all(check_postal_code(postal_code_valid[["VE"]], country = "VE")))
  expect_true(all(check_postal_code(postal_code_valid[["VG"]], country = "VG")))
  expect_true(all(check_postal_code(postal_code_valid[["VI"]], country = "VI")))
  expect_true(all(check_postal_code(postal_code_valid[["VN"]], country = "VN")))
  expect_true(all(check_postal_code(postal_code_valid[["WF"]], country = "WF")))
  expect_true(all(check_postal_code(postal_code_valid[["YT"]], country = "YT")))
  expect_true(all(check_postal_code(postal_code_valid[["ZA"]], country = "ZA")))
  expect_true(all(check_postal_code(postal_code_valid[["ZM"]], country = "ZM")))
})

test_that("VIN numbers can be successfully validated", {
  
  expect_true(all(check_vin(vin_valid)))
  expect_true(all(!check_vin(vin_invalid)))
})

test_that("ISBN numbers can be successfully validated", {
  
  expect_true(all(check_isbn(c(isbn_10_valid, isbn_13_valid))))
  expect_true(all(!check_isbn(c(isbn_10_invalid, isbn_13_invalid))))
})

test_that("Phone numbers can be successfully validated", {
  
  expect_true(all(check_phone(phone_valid)))
  expect_true(all(! check_phone(phone_invalid)))
})

test_that("MAC addresses can be successfully validated", {
  
  expect_true(all(check_mac(mac_valid)))
  expect_true(all(!check_mac(mac_invalid)))
})

test_that("Swift/BIC numbers can be successfully validated", {
  
  expect_true(all(check_swift_bic(swift_bic_valid)))
  expect_true(all(!check_swift_bic(swift_bic_invalid)))
})

test_that("Email addresses can be successfully validated", {
  
  expect_true(all(check_email(email_valid)))
  expect_true(all(!check_email(email_invalid)))
})

test_that("URLs can be successfully validated", {
  
  expect_true(all(check_url(url_valid)))
  expect_true(all(!check_url(url_invalid)))
})

test_that("IPv4 addresses can be successfully validated", {
  
  expect_true(all(check_ipv4_address(ipv4_address_valid)))
  expect_true(all(!check_ipv4_address(ipv4_address_invalid)))
})

test_that("IPv6 addresses can be successfully validated", {
  
  expect_true(all(check_ipv6_address(ipv6_address_valid)))
  expect_true(all(!check_ipv6_address(ipv6_address_invalid)))
})

test_that("Credit card numbers can be successfully validated", {
  
  expect_true(all(check_credit_card(credit_card_valid)))
  expect_true(all(!check_credit_card(credit_card_invalid)))
})

test_that("the `is_isbn_10()` function works", {
  
  isbn_10 <- c("0307957802", "0679405828", "0679405437", "0307268217")
  
  expect_true(is_isbn_10(isbn_10[1]))
  expect_true(is_isbn_10(isbn_10[2]))
  expect_true(is_isbn_10(isbn_10[3]))
  expect_true(is_isbn_10(isbn_10[4]))
})

test_that("the `is_isbn_13()` function works", {
  
  isbn_13 <- c("978-0307957801", "978-0679405825", "978-0679405436", "978-0307268211")
  
  expect_true(is_isbn_13(isbn_13[1]))
  expect_true(is_isbn_13(isbn_13[2]))
  expect_true(is_isbn_13(isbn_13[3]))
  expect_true(is_isbn_13(isbn_13[4]))
})

test_that("functions that clean up strings all work", {
  
  strings <- c("978-0307957801", "978-0307957801 ", "isbn978-0307957801", " isbn:978 - 0307957801")
  
  expect_equal(
    remove_hyphens(strings),
    c("9780307957801", "9780307957801 ", "isbn9780307957801", " isbn:978  0307957801")
  )
  
  expect_equal(
    remove_spaces(strings),
    c("978-0307957801", "978-0307957801", "isbn978-0307957801", "isbn:978-0307957801")
  )
  
  expect_equal(
    remove_letters(strings),
    c("978-0307957801", "978-0307957801 ", "978-0307957801", " :978 - 0307957801")
  )
  
  expect_equal(
    remove_punctuation(strings),
    c("978 0307957801", "978 0307957801 ", "isbn978 0307957801", " isbn 978   0307957801")
  )
  
  expect_equal(
    strings %>%
      remove_hyphens() %>%
      remove_letters() %>%
      remove_punctuation() %>%
      remove_spaces(),
    c("9780307957801", "9780307957801", "9780307957801", "9780307957801")
  )
})

test_that("the `is_vin()` function works", {
  
  expect_true(is_vin(specifications$vin_numbers[1]))
  expect_true(is_vin(specifications$vin_numbers[2]))
  expect_true(is_vin(specifications$vin_numbers[3]))
  expect_true(is_vin(specifications$vin_numbers[4]))
  expect_true(is_vin(specifications$vin_numbers[5]))
  expect_false(is_vin(specifications$vin_numbers[6]))
})

# test_that("the `check_vin_db()` function works", {
#   
#   spec_table_duckdb <- 
#     db_tbl(table = specifications, dbname = ":memory:", dbtype = "duckdb") %>%
#     dplyr::select(vin_numbers)
#   
#   duck_vin_db_check_tbl <-
#     check_vin_db(
#       table = spec_table_duckdb,
#       column = vin_numbers
#     )
#   
#   expect_equal(
#     colnames(duck_vin_db_check_tbl),
#     c("vin_numbers", "pb_is_good_")
#   )
#   
#   expect_equal(
#     duck_vin_db_check_tbl %>% dplyr::pull(pb_is_good_),
#     c(rep(TRUE, 5), NA, FALSE, FALSE)
#   )
# })
