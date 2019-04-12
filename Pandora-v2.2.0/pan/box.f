      subroutine BOX
C
C     Rudolf Loeser, 1988 Jul 29
C     RL/SGK revised Jul 14 2014 
C
C     PANDORA's data structures are described, collected and
C     initialized here.
C
C     (Also, BOX does some actual computing!)
C
C     BOX should be the second module loaded, right after the
C     PANDORA main program.
C     This is because BOX is the   O N E   place containing
C     all the most up-to-date labelled common definitions.
C     !EJECT
C
C     G E N E R A L   D A T A   B L O C K S
C
C
C     The real*8 data block is the first part of "X", declared in the
C     main program.
C
C     (In the following tabulation:
C     NLM = NL-1
C     MUL = NL*NLM/2
C     MRS = sum[MRJ(I)], 1 .le. I .le. NSL+1
C     MRK = MRS+NSL+1
C     MLS = sum[LRJ(I)], 1 .le. I .le. NL
C     NTN = max(NT,NPT)
C     NP  = (N-2)/NTAN+1
C     M5  = 2*N+5
C     NLQ = max(MAXPOPL,NL)     .)
C
C
C      1  JJEQT    QTAIL     MQT       EP1-Q smoothing tail
C      2  JJEP2    EP2       N         Lyman Epsilon-2
C      3  JJOML    OML       NT        Line Continuum Opacity factor
C      4  JJMU     EMU       L         mu for Line Profile calculation
C      5  JJ304    HE304     N         He II 304 Line Jbar
C      6  JJRKX    RKMLT     NSL       RK enhancement factors
C      7  JJTE     TE        N         Electron temperature
C      8  JJTR     TR        N*NSL     Radiation temperatures
C      9  JJXNE    XNE       N         Electron number density
C     10  JJHNF    HNDF      NFH       Hydrogen density table of FNH
C     11  JJHND    HND       N         total Hydrogen number density
C     12  JJV      V         N         Microturbulent velocity
C     13  JJYBR    YBAR      N*NT      Line Mean Intensities (Jbar)
C     14  JJXIN    XINK      INK       incident radiation data
C     15  JJFIN    FINK      INK       incident radiation data
C     16  JJGM     GMI       N*NSL     P times Boltzmann factor
C     17  JJRK     RKI       N*NSL     Photoionization rates
C     18  JJRL     RLI       N*NSL     Photorecombination rates
C     19  JJQU     QUI       N*NSL     (ionization term)
C     20  JJQS     QSI       N*NSL     (ionization term)
C     21  JJFNH    FNH       NFH       standard table of flow velocity
C     22  JJTS     TS        M         standard Tau table
C     23  JJBIJ    BDIJ      N*NL      ratios of Departure Coefficients
C     24  JJWEI    WEIGHT    NT*MUL    (Fudge factors for NOVA)
C     25  JJALF    ALF       MUL       (alpha)
C     26  JJXNU    XNU       NSL       Frequencies
C     27  JJP      P         NSL       Statistical Weights
C     28  JJCP     CP        NSL+1     Photoionization cross-sections
C     29  JJCII    CII       NSL*NTE   Collisional ionization coeff.
C     30  JJCEI    CEIJ      MUL*NTE   Collisional excitation coeff.
C     31  JJCK     CKI       N*NSL     Collisional ionization rates
C     32  JJAIJ    AIJ       NL*NL     Einstein A values
C     33  JJMSI    GMASIN    N         Column Mass, input values
C     34  JJAL     AL        NL        added recombination fraction
C     35  JJZIN    ZIN       N         original input Z
C     36  JJCEK    CEK       N*(NL-2)  (convergence Checks)
C     37  JJZ      Z         N         depth
C     38  JJMSR    GMASRF    N         Column Mass, reference values
C     39  JJBAT    BATA      N*MUL     Stimulated Emission factors (TE)
C     40  JJXSH    XSHL      M5*NP     Ray distances (Shell)
C     41  JJVNH    VNH       NVH       master table of V(NH)
C     42  JJALK    ALBK      N         Kurucz Opacity albedo
C     43  JJBTR    BATAR     N*MUL     Stimulated Emission factors (TR)
C     44  JJBDI    BDI       N*NL      Departure Coefficients
C     45  JJRHO    RHOIJ     N*NT      net radiative brackets
C     46  JJXDR    XDR       NDR       auxiliary table for P.R.D.
C     47  JJDDR    DDR       NDR       auxiliary table for P.R.D.
C     48  JJZME    ZME       N         non-H Electrons
C     49  JJVT     VT        N         Turbulent Pressure velocity
C     50  JJNK     NK        N         Continuum number density
C     51  JJNKS    NKS       N         Continuum number density (LTE)
C     52  JJRZM    RZM       N         non-H Electron multiplier
C     53  JJRAB    RABD      N         Abundance multiplier
C     54  JJTS     T5000     N         Tau 5000
C     55  JJPEL    PEL       N         Electron pressure
C     56  JJPGS    PGS       N         Gas pressure
C     57  JJPTU    PTU       N         Turbulent pressure
C     58  JJPTO    PTO       N         total pressure
C     59  JJXND    XND       N*NL      Level number densities
C     60  JJMSH    EMSHL     N*NP      Ray mu's (Shell)
C     61  JJYLM    YLYM      KK        (Y for Lyman continuum source fn)
C     62  JJTDN    TDUST     N         Dust temperature
C     63  JJCRH    CRH       N         Cooling rate, H-
C     64  JJFKR    FKUR      KNW       Kurucz Opacity multipliers
C     65  JJWAV    WAVES     NWV       additional wavelengths
C     66  JJRKQ    RKQ       N*NSL     Cooling rates data
C     67  JJRLQ    RLQ       N*NSL     Cooling rates data
C     68  JJPNF    PNF       N         pop. normaliz. fact. (diffusion)
C     69  JJRRN    RRNUNJ    MRK       frequencies for rate integrations
C     70  JJRRC    RRCPNJ    MRK       CP ratios for rate integrations
C     71  JJTEX    TEX       N         Excitation Temperature
C     72  JJYK     YK        NSL       Hydrogen recombination parameter
C     73  JJPAB    PALBET    N         diffusion term
C     74  JJRKC    RKCJ      MLS       additional photoionization param.
C     75  JJTKI    TAUKIN    N         input TAUK (for Z-recalculation)
C     76  JJYCO    YCONT     NT        (Y for CSF at Line frequencies)
C     77  JJYRA    YRATE     MRK       (Y for CSF for rate integrations)
C     78  JJYWA    YWAVE     NWV       (Y for CSF for additional waves)
C     79  JJTER    TER       NTE       Temperature values for CII + CEIJ
C     80  JJAEL    AEL       N         added electrons
C     81  JJYBC    YBCON     N*NT      Continuum Jnu in Line Core
C     82  JJQIN    QIN       N         K-Shell ionization data
C     83  JJQUO    QOUT      N         K-Shell ionization data
C     84  JJLHM    LHM       MHM       wavelengths for AHM
C     85  JJAHM    AHM       MHM       H- bound-free absorption coeff
C     86  JJBHM    BDHM      N         H- departure coefficient
C     87  JJYHM    YHM       MHM       (Y for CSF for H-)
C     88  JJPKS    PIKS      N         K-Shell ionization correction
C     89  JJYDT    YLDT      NDT       (Y for Dust wavelengths)
C     90  JJLDT    LDT       NDT       Type-2 Dust data wavelengths
C     91  JJADT    ADT       NDT       Type-2 Dust absorption table
C     92  JJABD    ALBDT     NDT       Type-2 Dust albedo table
C     93  JJLMM    XLMM      JM        wavelengths for XMLC
C     94  JJMLC    XMLC      JM        Opacity Multiplier
C     95  JJXM     XM        M*M       standard Weight Matrix
C     96  JJLMD    XLMDUST   LDU       Dust opacity input wavelengths
C     97  JJDFD    DFDUST    LDU       Dust opacity parameter
C     98  JJALD    ALBDUST   LDU       Dust opacity parameter
C     99  JJEPD    EPDUST    LDU       Dust opacity parameter
C    100  JJLXX    XLMXX     LLY       Lyman alpha opacity x table
C    101  JJLDR    XLMDR     LLY       Lyman alpha opacity DR(x) table
C    102  JJTKR    TKRJ      MLS       additional photoionization param.
C    103  JJYKR    YKRJ      MLS       additional photoionization param.
C    104  JJPBA    PBETAL    N         diffusion term
C    105  JJEP1    EP1       N         Lyman Epsilon-1
C    106  JJMCE    CMCE      MUL       CE multiplier
C    107  JJACE    CACE      MUL       CE addend
C    108  JJR1W    RK1W      N         RK1 weights
C    109  JJCSH    CSHL      N*NP      Shell ray integr. weights (WN)
C    110  JJOR1    ORK1      N         previous RK1 values
C    111  JJNE0    NE0       N         input NE, conserved
C    112  JJGK     GK        KK        GK for Lyman
C    113  JJPBG    PBETGM    N         diffusion term
C    114  JJPGB    PGMBET    N         diffusion term
C    115  JJMUF    EMUF      LF        mu for Flux Profile integration
C    118  JJLCR    XLCR      NCR       wavelengths for Coronal Radiation
C    117  JJICR    XICR      NCR       incident Coronal Radiation
C    118  JJYCR    XYCR      NCR       (Y for Coronal Radiation)
C    119  JJMSS    GMASS     N         mass of Column of gas
C    120  JJWTP    WTP       NVX       profile weights for flow-broad.
C    121  JJXIF    XIFUL     KF        Line Profile frequencies
C    122  JJAF     AFUL      KF        Line Integration weights
C    123  JJXK     XK        KK        "Lyman" Integration frequencies
C    124  JJH1     H1        N         H(level 1) population
C    125  JJBDL    BDL       N         "Lyman" calculated BD-KOLEV
C    126  JJPRF    PREF      N         opacity(REFLM), for TAUKIN
C    127  JJWVK    WAVEK     KNW       Kurucz Opacity wavelengths
C    128  JJARK    ARRK      N*KNW     Kurucz Opacity array
C    129  JJVXS    VXS       N         Expansion Velocity (Line S.F.)
C    130  JJXMU    XMU       LG        mu table for GR method
C    131  JJCMU    CMU       LG        mu integr. weights (WN)
C    132  JJXDK    XDSK      N*MRR     Ray distances (Disk)
C    133  JJMDK    EMDSK     N*MRR     Ray mu's (Disk)
C    134  JJCDK    CDSK      N*MRR     disk ray integr. weights (WN)
C    135  JJFRR    FRR       MRR       radius fractions
C    136  JJHJ     HJ        N         Ionization rates multiplier
C    137  JJBTL    BATAL     N*NLQ     Stimulated Emission factors (TE)
C    138  JJFRS    FRS       N         distance (radii)
C    139  JJCEQ    CEQHH     N         H2 number density coefficient
C    140  JJH2N    H2N       N         H2 number density
C    141  JJPF     PF        N         Partition function of ion of run
C    142  JJPFT    PFT       N*NMT     Partition function ratios table
C    143  JJCHI    CHI       NMT       general Ionization Potent. table
C    144  JJPIJ    PIJ       N*NL*NL   bound-free-bound Transition rates
C    145  JJCIJ    CIJ       N*NL*NL   collisional Transition rates
C    146  JJXIS    XISYM     KS        Line Profile frequencies
C    147  JJAS     ASYM      KS        Line Integration weights
C    148  JJXIR    XIRED     KR        Line Profile frequencies
C    149  JJXNC    XNC       N         charged particle density
C    150  JJXIB    XIBLU     KB        Line Profile frequencies
C    151  JJDGM    DGM       N         depth-dep. G multiplier (HSE)
C    152  JJDWV    DWAVE     NDV       CSF dump wavelengths
C    153  JJKSR    KODSRW    NP        Shell-ray WN-Matrix kodes
C    154  JJFON    FION      N         fraction of ions
C    155  JJFVS    FLVS      N         total bound-levels population
C    156  JJRML    RML       N         mass loss rate
C    157  JJWVC    WAVCO     NCP       Compos. Line Opac. wavelengths
C    158  JJARC    ARRCO     NCP*N     Compos. Line Opac. data array
C    159  JJYWC    YWVCO     NCP       (Y for Compos. Opac wavelengths)
C    160  JJBNL    BANDL     NAB       Compos. Opac. band, lower limit
C    161  JJBNU    BANDU     NAB       Compos. Opac. band, upper limit
C    162  JJBNY    BANDY     NAB       (Y for Compos. Opac. bands)
C    163  JJTIJ    TIJ       N*NL*NL   PIJ "without diffusion"
C    164  JJVXN    VXN       N*NVX     additional expansion velocities
C               or VAX       N*NVX     additional shock velocities
C    165  JJVR     VR        N         radial comp. of Broad. Velocity
C    166  JJMCI    CMCI      NSL       CI multiplier
C    167  JJWMU    WMU       LG        mu integr. weights (WH)
C    168  JJWDK    WDSK      N*MRR     disk ray integr. weights (WH)
C    169  JJWSH    WSHL      N*NP      shell ray integr. weights (WH)
C    170  JJZBK    ZALBK     NKA       Z-table for ALBK
C    171  JJABK    ALBK      NKA       input albedo table for Kurucz opac.
C    172  JJVSB    VSB       N         "Sobolev" expansion velocity
C    173  JJTRA    TRA       N         "Artificial TAU" for RHO/W
C    174  JJJBN    XJBN      N*KK      Lyman-transfer JNU
C    175  JJGVL    GNVL      N*NL      S.E. diffusion term
C    176  JJGVI    GNVION    N         S.E. diffusion term
C    177  JJVAD    VADD      N         diffusion-related velocity increment
C    178  JJZT     ZT        N         logarithmic temperature gradient
C    179  JJZI     ZI        N         log. grad. of (proton/H) ratio
C    180  JJVAM    VAMB      N         (neutrals/ions) diffusion velocity
C    181  JJVM     VM        N         mass motion velocity
C    182  JJOLL    OLL       NT        Line opacity factor (in GTN)
C    183  JJCOL    COL       NCL       CO-lines wavelengths offsets
C    184  JJNCO    CON       N         CO number density
C    185  JJSWV    SWAVE     NWS       subtractional wavelengths
C    186  JJCVX    CVX       NVX       fluid velocity parameters
C    187  JJVPR    VP        N         (protons) diffusion velocity
C    188  JJVHA    VH        N         (Hydrogen atoms) diffusion velocity
C    189  JJRBL    RABDL     N         log(Abundance multiplier)
C    190  JJGDZ    GDZ       N         set of Z-values for current GDT
C    191  JJGDT    GDT       N*N       geometrical dilution terms matrix
C    192  JJVEL    VE        N         (electrons) diffusion velocity
C    193  JJV1     V1        N         (He I) diffusion velocity
C    194  JJV2     V2        N         (He II) diffusion velocity
C    195  JJV3     V3        N         (He III) diffusion velocity
C    196  JJVBM    VBMB      N         (H/He I) diffusion velocity
C    197  JJVCM    VCMB      N         (He I/He II) diffusion velocity
C    198  JJVDM    VDMB      N         (He II/He III) diffusion velocity
C    199  JJZ1     Z1        N         log. grad. (He I/H) ratio
C    200  JJZ2     Z2        N         log. grad. (He II/He I) ratio
C    201  JJZ3     Z3        N         log. grad. (He III/He II) ratio
C    202  JJVXI    VXSI      N         input component of VXS
C    203  JJZIO    ZION      N         H ionization multiplier
C    204  JJACI    CACI      NSL       CI addend
C    205  JJPEX    PEX       N         gas expansion pressure
C    206  JJHNV    HNDV      NVH       NH for VNH
C    207  JJCIA    CIJADD    N*NL*NL   input term to be added to CIJ
C    208  JJCKA    CKIADD    N*NSL     input term to be added to CKI
C    209  JJDIO    DIONL     N         fraction of ions (diffusion)
C    210  JJDLV    DLVSL     N         fraction of bound levels (diffusion)
C    211  JJRNI    FRNIN     N         input ND,NK normalization term
C    212  JJRKH    XRKH    N*NPQLM*NXI charge exchange terms for Hydrogen
C    213  JJRLH    XRLH    N*NPQLM*NXI charge exchange terms for Hydrogen
C    214  JJXRK    XRK       N*NPQLM   charge exchange terms for Hydrogen
C    215  JJXRL    XRL       N*NPQLM   charge exchange terms for Hydrogen
C    216  JJCXP    CXXP      N*NL      charge exchange term
C    217  JJCXX    CXX       N*NL      charge exchange term
C    218  JJRCX    RCHX    NPQLM*NPQLM charge exchange parameter
C    219  JJHEA    RHEAB     N         Helium abundance variation
C    220  JJWNU    WNU       NSL       level edges, in wavenumbers
C    221  JJXCA    XLCOA     NCB       CO spectrum bands, lower limits
C    222  JJXCB    XLCOB     NCB       CO spectrum bands, upper limits
C    223  JJSQS    SQS       N         sum of QUI (for each depth)
C    224  JJOSF    OSF       MUL       oscillator strength
C    225  JJFMV    FMV       N         fluid velocity multiplier
C    226  JJZEC    ZECL      NZE       selected Z-values for Eclipse calculation
C    227  JJWVA    WAVCA     KWA       Averaged line opacity wavelengths
C    228  JJARA    ARRCA     KWA*N     Averaged line opacity data array
C    229  JJFIW    WMUF      LF        flux-from-intensity integration weights
C    230  JJZGM    ZGM       NGM       Z-table of DGMZ
C    231  JJICO    CONI      N         input values of CO number density
C    232  JJCQT    CQT       NCQ       temperature table for CQA
C    233  JJCQA    CQA       NCQ       scattering albedo input table
C    234  JJDTE    DTE       N         temperature gradient
C    235  JJSA     SA        N         Saha-Boltzmann term for ion of run
C    236  JJZRN    ZRN       N         ions other than protons
C    237  JJTCO    TCO       N         T for computing CO number density
C    238  JJFCT    FCT       N         factor for TCO
C    239  JJSCW    SCOW      NSW       selected continuum output wavelengths
C    240  JJDAH    DAH       N*MUL     depth-dependent A-values, Hydrogen
C    241  JJS      S         N*NT      line source function
C    242  JJAW     AW        N*NT      integrated diagonal of WN-matrix
C    243  JJAEW    AEW       NSL       level edges, in nanometers
C    244  JJWRA    RRWRAT    MRK       wavelengths for rates integrations
C    245  JJQHI    QHI       N*NT      aka Chi, a Rho-type quantity
C    246  JJQSA    SA        N*NT      related to Chi (QHI)
C    247  JJQST    ASTAR     N*NT      related to Chi (QHI)
C    248  JJFCE    FCE       N*NT      CE-enhancement factors
C    249  JJPCE    PCE       NT        FCE multiplicative increment
C    250  JJHIJ    HIJ       N*NL*NL   collisions with Hydrogen
C    251  JJFCJ    JCJ       N*NL      fast-electrons term
C    252  JJSIJ    SIJ       N*NL*NL   part of CIJ
C    253  JJSET    SET       N*MUL     stimulated emission terms
C    254  JJBXI    BXI       KBX       XI-table for background lines
C    255  JJWLA    WNRMLA    NFL       wavelength table for FNRMLA
C    256  JJFNA    FNRMLA    NFL       H Ly-alpha normalizing factor
C    257  JJWLB    WNRMLB    NFL       wavelength table for FNRMLB
C    258  JJFNB    FNRMLB    NFL       H Ly-beta normalizing factor
C    259  JJWSL    WSLN      NLN       base wavelengths for WNRMLA,B
C    260  JJXCU    XNUC      NSL       auxiliary continuum levels
C    261  JJWCU    WNUC      NSL       aux. cont. levels, wavenumbers
C    262  JJAAT    AATIJ     NL*NL     Einstein As for all (u,l) trans.
C    263  JJGMZ    DGMZ      NGM       standard DGM table
C    264  JJYAF    YALSF     NT        final Y for LSF frequ. integr.
C    265  JJPMG    PMG       N         magnetic pressure
C    266  JJBHZ    BHORIZ    N         horiz. compt. of magnetic field
C    267  JJHMN    HMN       N         H- number density
C    268  JJCHN    CHN       N         CH (molecule) number density
C    269  JJOHN    OHN       N         OH (molecule) number density
C
C     These indices are in Common Block MANAGER.
C
C
C
C     The integer*4 general data block is the first part of "IX",
C     declared in the main program.
C
C     (In the following tabulation:
C     MUL = (NL*(NL-1))/2                    .)
C
C
C      1  JJMRJ    MRJ       NSL+1     no.'s of RRNUNJ for each level
C      2  JJKIJ    KIJ       NL*NL     Transition Type switch
C      3  JJLRJ    LRJ       NL        no.'s of RKCJ for each level
C      4  JJRKS    IRKCOMP   NSL       RK component compute switch
C      5  JJRLS    IRLCOMP   NSL       RL component compute switch
C      6  JJIKW    INWVC     NCP       input file indices of WAVCO
C      7  JJIBE    IBNDE     NAB       Compos. Opac. band, eclipse switch
C      8  JJLIJ    LIJ       MUL       single rates switch
C      9  JJNPQ    NPQ       NSL       principal quantum number n
C     10  JJLRQ    LRQ       NSL       rotational quantum number l
C     11  JJLCX    LCX       NSL       "charge exchange" levels
C     12  JJNLE    NLE       NSL       number of "nl" electrons
C     13  JJISV    ISSV      NVX       VAX-tables generating indices
C     14  JJKZA    KZAUG     N         Z-augmentation counters (diffusion)
C     15  JJKZU    KZUNL     N*IOMX    unlimited form of KZAUG
C     16  JJMIJ    MIJ       NL*NL     GTN-editing record
C     17  JJLCH    LCH       NSL       collisions-with-hydrogen selector
C
C
C     These indices are in Common Block MINIGER.
C     !EJECT
C
C     P O P U L A T I O N   D A T A   B L O C K S
C
C     Information generally collected in Common Blocks POPDATA and
C     IONDATA. Population Data Blocks are read and rewritten as
C     necessary.
C
C     NPOPS        total number of Population Data Blocks
C     LENPOP       number of level populations in a set
C     LIMPOP       limit of level populations for a set
C     LENT         sum of LIMPOP
C     MAXPOPL      maximum value of LIMPOP
C     LIMDAT       limit of level entries in Ionization Data Table
C     LEND         sum of LIMDAT
C     MAXDATL      maximum value of LIMDAT
C     NAMES        NAME of Population Data Block
C     TNAMES       NAME of Population Data Block (truncated)
C     NAMKNT       number of characters in NAME
C     POPSYM       element symbol, as in Element Data Tables
C     IPSWICH      copy of population PRINT option
C     IUPOP        population update switch
C     KAPNO        absorber (KAPPA) number
C     ICKSM        index of "SENNA" checksum
C     IBLAD        random access address of Population Data Block
C     LENPBL       length of Population Data Block
C
C     Constituents of a Population Data Block:
C
C     (Names in [brackets] indicate integer values that are converted to
C     floating point form, so as not to mix data types in this block.)
C
C      1 LLNPOP    XNPOP     1         Number of Block
C      2 LLIUP    [IUP]      1         Update switch
C      3 LLPOPK    POPK      N         ionized number density
C      4 LLPOPN    POPN      N*MAXPOPL Level populations
C      5 LLBD      BD        N*MAXPOPL Level departure coefficients
C
C     These constituents are allocated by subroutine LOLIPOP.
C     !EJECT
C
C     L I N E   I N T E N S I T Y   D A T A   B L O C K S
C
C     Information collected in Common Blocks ELIZA and THULE.
C     Line Intensity Data Blocks are read and rewritten as necessary.
C
C     A Line Intensity Data Block differs from other PANDORA data
C     blocks in that it consists of three logical records, each
C     allocated, indexed, and written separately.
C
C     Constituents of a Line Intensity Data Block:
C
C     (Names in [brackets] indicate integer values that are converted
C     to floating point form, so as not to mix data types in this block.
C     "FF" and "BF" denote front-face and back-face, respectively.)
C
C     Section 1:
C
C      1 XNAME     MMNAM     1         Block name, = 100*IU + IL
C      2 LAMBDA    MMLAM     1         Wavelength of Transition
C      3 XI        MMXI      KM        profile integration frequencies
C      4 CDWC      MMCDW     1         selected DW value
C      5 DAMP      MMY       1         S.F. method parameter
C      6 DPMULT    MMDPM     1         Damping Parameter multiplier
C      7 CRD       MMCRD     LDLMX     radiative half width
C      8 CVW       MMCVW     LDLMX     van der Waals half width
C      9 CSK       MMCSK     LDLMX     Stark half width
C     10 CRS       MMCRS     1         resonance half width
C     11 FNDT      MMFND     N         Incident Radiation term
C     12 DP        MMDP      N*LDLMX   Damping parameter
C     13 DW        MMDW      N         Doppler width
C     14 A         MMA       KM        profile integration weights
C     15 GTN       MMGTN     N
C     16 TAU       MMTAU     N         optical depth
C     17 COP       MMCOP     N         Continuous Opacity
C     18 CSF       MMCSF     N         Continuum Source function
C     19 PE        MMPE      N
C     20 FE        MMFE      N
C     21 EP        MMEP      N
C     22 B         MMB       N         Planck function (TE)
C     23 BS        MMBS      N
C     24 BC        MMBC      N
C     25 CNDT      MMCND     N         Incident Radiation term
C     26 S         MMS       N         Line Source function
C     27 YBAR      MMJBR     N         Mean Intensity (Line)
C     28 RHO       MMRHO     N         net radiative bracket
C     29 RHOWT     MMRHW     N         Rho Weights
C     30 ORHO      MMORH     N         Rho values of preceding iteration
C     31 CDL       MMCDL     LDLMX     blended line components weights
C     32 TAUS      MMTS      N         LTE Tau
C     33 SS        MMSS      N         LTE S
C     34 DDL       MMDDL     LDLMX     blended lines offsets (Angstroms)
C     35 GTNS      MMGTS     N         LTE GTN
C     36 YBARC     MMJBC     N         Mean Intensity (Continuum)
C     37 WMS       MMWSM     1         smoothing weight
C     38[IFS]      MMIFS     1         smoothing index
C     39[ILS]      MMILS     1         smoothing index
C     40 DRHO      MMDRO     1         Rho editing parameter
C     41[NED]      MMNED     1         Rho editing parameter
C     42 XC        MMXC      1         RNC adjustment parameter
C     43 XP        MMXP      1         RNC adjustment parameter
C     44 GMMA      MMGMA     1         RNC parameter
C     45 GTO       MMGTO     N         unedited (raw, original) GTN
C     46 ST        MMST      N         total source function
C     47 BTR       MMBTR     N         Planck function (TR)
C     48 FXI       MMFXI     N         term for Sobolev solution
C     49 ISB1      MMSB1     1         Sobolev solution limit index
C     50 ISB2      MMSB2     1         Sobolev solution limit index
C     51 TAUM      MMTAM     N         mean optical depth
C     52 GMAI      MMGMI     N         depth-dependent GMMA
C     53 PROGLI    MMPGL     1         profile graphs limit
C     54 PGD       MMPGD     3*LDLMX   prof. graph data: core, blue, red
C     55 DWN       MMDWN     LDLMX     blended line offset (wavenumber)
C     56 XIBLUT    MMIXB     KBTMX     specific frequency table - blue
C     57 XIREDT    MMIXR     KRTMX     specific frequency table - red
C     58 XISYMT    MMIXS     KSTMX     specific frequency table - half
C     59 DL        MMDL      KM        profile wavelengths
C     60[ISTRK]    MMSTI     1         NE-index for H Stark splitting
C     61 XNESTRK   MMSTE     1         NE-value for H Stark splitting
C     62[KSTRK]    MMSTK     1         Convolved-Stark-profile switch
C     63 XR        MMXR      1         RNC adjustment parameter
C     64 DRLIMI    MMLRI     N         RNC adjustment parameter
C     65 AW        MMAW      N         freq.-integrated WN diagonal
C     66 QHI       MMQHI     N         aka Chi, a Rho-type quantity
C     67 SN        MMSN      N         "S-from-Number-Density"
C
C     Section 2:
C
C      1 YNAME     MMYAM     1         Block name, = 100*IU + IL
C      2 SNU       MMSNU     N*KM      P.R.D. modified Source function
C      3 SCNU      MMSCN     N*KM      background source function array
C      4 XKCNU     MMKCN     N*KM      background opacity array
C      5 XJNU      MMJNU     N*KM      background intensity
C      6 SLF       MMSLF     N*KM      frequency-dependent L.S.F.
C
C     Section 3:
C
C      1 ZNAME     MMZAM     1         Block name, = 100*IU + IL
C      2 CINTZ     MMCIZ     KM*L      core continuum intensity /Hz -FF
C      3 CINTA     MMCIA     KM*L      core continuum intensity /A - FF
C      4 CFLXZ     MMCFZ     KM        core continuum flux /Hz - FF
C      5 CFLXA     MMCFA     KM        core continuum flux /A - FF
C      6 BTINT     MMBTI     KM*L      brightness temp. from int. - FF
C      7 BTFLX     MMBTF     KM        brightness temp. from flux - FF
C      8 CINZA     MMCZA     KM        cont. intensity /Hz + INC - FF
C      9 CINAA     MMCAA     KM        cont. intensity /A + INC - FF
C     10 BTINA     MMBTA     KM        brightness temp. (int.+INC) - FF
C     11 CNTZB     MMCZB     KM*L      core continuum intensity /Hz - BF
C     12 CNTAB     MMCAB     KM*L      core continuum intensity /A - BF
C     13 CFXZB     MMFZB     KM        core continuum flux /Hz - BF
C     14 CFXAB     MMFAB     KM        core continuum flux /A - BF
C     15 BTNTB     MMTIB     KM*L      brightness temp. from int. - BF
C     16 BTFXB     MMTFB     KM        brightness temp. from flux - BF
C     17 CNZAB     MMZAB     KM        cont. intensity /Hz + INC - BF
C     18 CNAAB     MMAAB     KM        cont. intensity /A + INC - BF
C     19 BTNAB     MMTAB     KM        brightness temp. (int.+INC) - BF
C
C     These constituents are allocated via subroutine MOODY.
C     !EJECT
C
C     C O N T I N U U M   D A T A   B L O C K S
C
C     Information collected in Common Block COBLOCK.
C     Continuum Data Blocks are read and rewritten as necessary.
C
C     Constituents of a Continuum Data Block:
C
C     (Names in [brackets] indicate integer values that are converted to
C     floating point form, so as not to mix data types in this block.)
C
C      1 NAME      KKLTIT    1         block name, with LTYPE (below)
C      2 FUDGE     KKMULT    1         opacity multiplier
C      3[KONT]     KKKONT    NOPAC     contributors switches
C      4 Y         KKDAMP    1         damping parameter for CSF
C      5 CKSM      KKCKSM    NCSBA     checksums of "input" parameters
C      6 OPAC      KKOPAC    N         total opacity
C      7 SCAT      KKSCAT    N         Scattering ratio
C      8 BHSNUM    KKBHSN    N         numerator of BHS
C      9 BHSDEN    KKBHSD    N         denominator of BHS
C     10 BHS       KKBHS     N         Absorption Source function
C     11 CNXP      KKCNXP    N         Incident Radiation term
C     12 TAUK      KKTAUK    N         optical depth
C     13 JNU       KKJNU     N         Intensity
C     14 SCON      KKSCON    N         CSF - Continuum Source function
C     15 B         KKB       N         Planck function
C     16 BHSNMS    KKBNMS    N         BHSNUM-star
C     17 FD        KKFD      N         Flux Derivative
C     18[ACTO]     KKACTO    1
C     19 CO        KKCO      NOPAC*N   opacity components
C     20[ACTB]     KKACTB    1
C     21 CB        KKCB      NOPAC*N   emission components
C     22 T1        KKT1      N
C     23 TR        KKTR      N
C     24 S1        KKS1      N
C     25 SR        KKSR      N
C     26 BHS1      KKBHS1    N
C     27 BHSR      KKBHSR    N
C     28 CAPPAS    KKCAPR    N         CAPPA-star
C     29 CAPPA     KKCAPP    N
C     30 SIGMA     KKSIGM    N
C     31 ALBEDO    KKSLBD    N         "Line" contributions albedo
C     32[NRES]     KKRESN    1         Lyman calc. absorber number
C     33[ISLV]     KKILSV    1         reserved level number
C     34[LPRD]     KKCSCW    1         Partial Redistribution kode
C     35 ZABS      KKZABS    N         PRD term
C     36 ZSCA      KKZSCA    N         PRD term
C     37 ZSCR      KKZSCR    N         PRD term
C     38 CNDT      KKCNDT    N         Incident Radiation term
C     39 BULT      KKBULT    1         "Line Background Opac." mult.
C     40 LNAME     KKKTIT    100       supplementary block names
C     41 ZBNM      KKZBNM    N         PRD term
C     42[ITS]      KKITS     1         S iterations
C     43[ISWA]     KKISWA    NOPAC     absorbers recalculation switches
C     44[ISWE]     KKISWE    NOPAC     emitters recalculation switches
C     45 SIGMAS    KKSIGS    N         SIGMA-star
C     46 DL        KKDL      1         PRD only: line delta-lambda
C     47 CLO       KKCLO     N         Composite Line opacity values
C     48[KODE]     KKKODE    1         calculation-type code
C     49 TTAUK     KKTTAU    N         "true continuum" optical depth
C     50 TSCON     KKTSCN    N         "true continuum" source function
C     51 TOPAC     KKTOPA    N         "true continuum" opacity
C     52 TCNXP     KKTNXP    N         "true continuum" incident term
C     53 TFD       KKTFD     N         "true continuum" flux derivative
C     54 LAMBDA    KKLAMD    1         current wavelength value (A)
C     55 LAMBDP    KKLAMP    1         previous wavelength value
C     56[KNTT]     KKKNTT    NOPAC     "true continuum" contributor sw.
C     57 ZBDN      KKZBDN    N         PRD term
C     58 ZAXA      KKZAXA    N         PRD term
C     59 ZAYA      KKZAYA    N         PRD term
C
C     These constituents are allocated by subroutine MIKI.
C
C
C     Meaning of  L T Y P E :
C
C     LTYP = 1 MEANS: regular Line Center, LSF printed (:19)
C            2        additional, no Eclipse (:2)
C            3        additional, with Eclipse (:3)
C            4        Partial Redistribution
C            5        Rates Integration tables - regular (:12,27)
C            6        additional Photoionization
C            7        H- calculation
C            8        Dust Temperature adjustment procedure
C            9        HSE calculation
C           10        Level-K-to-Continuum Integration
C           11        Incident Coronal Radiation
C           12        Rates Integration table - K-Shell (:5,27)
C           13        Composite Line opacity, no Eclipse (:15)
C           14        miscellaneous
C           15        Composite Line opacity, with Eclipse (:13)
C           16        frequency-dependent line background, FDB
C           17        actual CO-lines, fundamental (:20,21,22,23)
C           18        FDB Line Center, LSF printed (:26)
C           19        regular Line Center, LSF not printed (:1)
C           20        actual CO-lines, 1. overtone (:17,21,22,23)
C           21        actual CO-lines, band limit (:17,20,22,23)
C           22        actual CO-lines, rotational (:17,20,21,23)
C           23        actual CO-lines, 2. overtone (:17,20,21,22)
C           24        PRD Line Center, LSF printed (:25)
C           25        PRD Line Center, LSF not printed (:24)
C           26        FDB Line Center, LSF not printed (:18)
C           27        Standard background (:5,12)
C
C     (See common block KWACK, and subroutines BEECH and MARTHA.)
C     !EJECT
C
C     C O U N T E R S
C
C
C     These counters are kept in the array JZQ (intended use: read-only)
C     in labelled common COUNTS.
C
C      1 N         number of depth points
C      2 NL        number of atomic levels
C      3 M         number of standard Tau values for Line Source
C                    Function calculations
C      4 KF        number of points in whole line profile
C      5 NT        number of radiative transitions
C      6 NSW       number of selected continuum output wavelengths
C      7 L         number of mu values for Line Intensity Profile
C                    calculations
C      8 NLB       number of levels for which Stimulated Emission
C                    Factors are computed
C      9 NFB       flow-broadening velocities counter
C     10 KK        number of standard frequency values for Level-1-to-
C                    Continuum Source Function calculation
C     11 KBX       number of BXI (background lines frequencies) values
C     12 KKX       augmented value of KK
C     13 LZM       maximum auxiliary Z-table length
C     14 NZM       maximum auxiliary Z-table index
C     15 MRR       number of radius fraction values
C     16 NFL       number of WNRMLA,B values
C     17 NWV       number of additional wavelengths
C     18 MMR       maximum of MRJ
C     19 LF        number of mu values for Line Flux Profile calculation
C     20 NTE       number of temperature values for Collisional
C                    Excitation and Ionization coefficients
C     21 NDT       number of Type-2 Dust wavelengths
C     22 MHM       number of wavelengths for H- calculation
C     23 NWS       number of subtractional wavelengths
C     24 JM        number of wavelengths for opacity multiplier
C     25 KNW       number of Statistical Line Opacity wavelengths
C     26 LDU       number of Dust Opacity input wavelength values
C     27 LLY       number of Lyman Alpha opacity x-table values
C     28 MLR       maximum of LRJ
C     29 MRS       sum of MRJ
C     30 MRX       maximum of augmented MRJ
C     31 NFH       number of HNDF values
C     32 NCR       number of values of Incident Coronal radiation
C     33 MLS       sum of LRJ
C     34 LG        number of mu values for GR method
C     35 INK       number of Incident Radiation data points
C     36 KS        number of points in half of symmetric line profile
C     37 KR        number of points in red half, asymmetric line profile
C     38 KB        number of points in blue half, asymmetric line profile
C     39 MQT       number of QTAIL values
C     40 NSL       number of atomic levels, including supplementaries
C     41 NDR       number of XDR values for P.R.D.
C     42 NVX       number of VX tables
C     43 NDV       number of CSF dump wavelengths
C     44 NCP       number of wavelengths for Composite Line opacity
C     45 NAB       number of wavelength bands for Composite Line opacity
C     46 KWC       number of Composite Line Opacity input data wavelengths
C     47 NVF       number of velocity values, for fast electrons
C     48 NXF       maximum number of injection function integrand
C                    values, for fast electrons calculation
C     49 KM        maximum possible number of line profile frequency points
C                    (to be used when it is not known a priori which one
C                     of the several alternate frequency tables will be
C                     used)  Note: KM is computed by THESEUS.
C     50 NKA       number of ZALBK values
C     51 NCL       number of CO-lines wavelengths offsets
C     52 NLN       number of WSLN values
C     53 NCQ       number of CQA values
C     54 NVH       number of VNH values
C     55 NCB       number of CO-lines spectral bands
C     56 NZE       number of ZECL values
C     57 KWA       number of Averaged Line opacity wavelengths
C     58 NGM       number of DGMZ values
C     !EJECT
C
C     S I N G L E   P A R A M E T E R S
C
C
C     These parameters are kept in the arrays RZQ, KZQ and QZQ
C     (intended use: read-only), in labelled commons ARGUS1, ARGUS2
C     and ARGUS3, respectively.
C
C      1 ADS       star/Sun angular diameter ratio (see also ADMAS)
C      2 WZ        Z-from-TAUK recalculation weight
C      3 RWKSI     reference wavelength for K-Shell integration
C      4 AMASS     atomic mass
C      5 PART      partition function
C      6 ABD       ion abundance
C      7 PW        exponent in Stark broadening term
C      8 Y         damping parameter for A and AK
C      9 XNUK      continuum frequency interval
C     10 CWR       a RHOW parameter
C     11 CHOP      a RHOW parameter
C     12 EXLYM     Lyman TAU change-over parameter
C     13 TGLYM     Lyman TAU change-over parameter
C     14 HSEC      iteration weight for Hydrostatic Equilibrium calc.
C     15 YH        Helium to Hydrogen ratio
C     16 CGR       gravity ratio (see also CLOGG)
C     17 DLU       dilution factor
C     18 TX        brightness temperature for illuminating source
C     19 YL        damping parameter for Lyman Source Function calc.
C     20 YPRE      damping parameter for XM
C     21 TSM       change-over TAU for intensity and mean intensity
C     22 DRLIM     RNC limit parameter
C     23 R1N       distance from illuminating source
C     24 TBAR      change-over TAU for weight-matrix
C     25 WPOP      iteration weight for level populations
C     26 XLMT      negative Lyman EP1 editing parameter
C     27 XLME      negative Lyman EP1 editing parameter
C     28 XKDST     Dust Opacity parameter
C     29 TDUST     Dust Opacity parameter
C     30 YFLUX     damping parameter for continuum flux
C     31 HEL       iteration weight for Hydrostatic Equlibrium calc.
C     32 XLMZ      H Ly lines (1-15) opacity wavelength cutoff
C     33 XLMF      negative Lyman EP1 editing parameter
C     34 WSM       Lyman RK-1 smoothing parameter
C     35 XLMA      negative Lyman EP1 editing parameter
C     36 XLMB      negative Lyman EP1 editing parameter
C     37 XLMR      negative Lyman EP1 editing parameter
C     38 TLIMG     absorption contribution graph limit
C     39 BLIMG     absorption contribution graph limit
C     40 WEP       Lyman EP1 weighting parameter
C     41 OPF       Incident Radiation extinction parameter
C     42 RFAC      reduction factor for all collision rates
C     43 WMN       a Rho-fudging parameter
C     44 TMS       small TAU change-over for RT and GR methods
C     45 WBD       iteration weight for departure coefficients
C     46 RCCFE     accuracy criterion for injection function
C                    integration
C     47 PRTLM     fudge factor for Partition Function
C     48 CURMI     limit wavelength for Statistical Line Opacity
C     49 CURMA     limit wavelength for Statistical Line Opacity
C     50 WTD       restraining parameter for TDUSTF
C     51 TLTR      limiting multiplier for TDUSTF
C     52 XCOMX     CO-lines width limit
C     53 DDT       Dust temperature calculation parameter
C     54 XINCH     a Rho-fudging parameter
C     55 WMX       a Rho-fudging parameter
C     56 SMP       a Rho-fudging parameter
C     57 EIDIF     NE iteration convergence criterion
C     58 CUTFE     cutoff criterion for injection function
C                    integration
C     59 WZM       Z weight
C     60 RFMAS     reference mass
C     61 FABD      multiplier for element abundances
C     62 EPCBR     branching ratio for Lyman-EP1 (suppl. levels)
C     63 TSMLL     weight matrix editing parameter
C     64 TLRGE     weight matrix editing parameter
C     65 CVXM      flow broadening velocities parameter
C     66 PRDCV     PRD-iterations convergence criterion
C     67 CVXF      flow broadening velocities parameter
C     68 WFB       weight for flow broadening
C     69 CSFCT     convergence criterion for CSF iteration
C     70 CLNH      HSE HND-iteration limiting factor
C     71 HTAU      HSE TAU5000(ref) weight
C     72 PZERO     Z-from-TAUK calculation parameter
C     73 TML       large TAU cutoff for Line Intensity integrals
C     74 DELTB     departure coefficients editing parameter
C     75 CEQMX     H2 abundance calculation limiting parameter
C     76
C     77 SMATC     Sample Output Matrix selection criterion
C     78 ELLED     L for particle energy dissipation calculation
C     79 EMXED     EMX for particle energy dissipation calc.
C     80 VMNFE     minimum velocity for fast electrons calc.
C     81 XQMAX     parameter for injection function integration
C     82 DQMIN     parameter for injection function integration
C     83 DQMAX     parameter for injection function integration
C     84 XJFE      flux of fast electrons
C     85 VSMLL     value for B in A/B when B=0
C     86 CPRSS     specified constant pressure
C     87 WPRSS     weight for adjusting NH to CPRSS
C     88 FZLIM     limit for Z-from-mass calculation
C     89 CLOGG     log of surface gravity (see also CGR)
C     90 DZMSS     parameter for Z-from-mass calculation
C     91 REFLM     wavelength of input TAUK
C     92 SBFEQ     "Sobolev" integration control parameter
C     93 SBDMX     "Sobolev" integration control parameter
C     94 SBDMN     "Sobolev" integration control parameter
C     95 ADMAS     diameter in milliarcseconds (see also ADS)
C     96 CHLIM     a RHOW parameter
C     97 YCOL      damping parameter for CO-lines wavelengths
C     98 XLCOD     CO-lines opacity dump wavelength
C     99 CCHX      charge exchange parameter
C    100 CHEFL     Helium flow constant
C    101 CQM       scattering albedo multiplier
C    102 XCL       PRD parameter for XXC
C    103 TAUCL     PRD parameter for XXC
C    104 CVXS      fluid velocity parameter for VXS
C    105 XMCOA     multiplier for CO profiles damping
C    106 VOITC     Voigt function cutoff
C    107 ZNDW      Z-value for optional default NDW determination
C    108 WNJNK     WN-matrix "cleanup" parameter
C    109 WBDIR     weight for "BD-direct" results
C    110 FBVMX     max. velocity for flow broadening
C    111 ASMCR     sequential smoothing parameter
C    112 ZXMIN     diffusion calculation parameter, for ZION
C    113 CFH       mass flow parameter (Hydrogen)
C    114 CVSB      fluid velocity parameter for VSB
C    115 HEABL     Helium abundance limit factor
C    116 RFHEA     Helium abundance coefficient reduction factor
C    117 TRFLI     limit interval for TR-effective calculation
C    118 EPTAU     TAU limit to force use of escape probability
C    119 WNUK      continuum interval (wavenumbers)
C    120 HSBM      Hydrogen Stark convolution control parameter
C    121 HSBMN     Hydrogen Stark convolution control parameter
C    122 HSBMX     Hydrogen Stark convolution control parameter
C    123 HSBFQ     Hydrogen Stark convolution control parameter
C    124 PMSK      multiplier of default Stark halfwidth
C    125 CEDMN     Impact-parameter CE calculation control
C    126 CEDMX     Impact-parameter CE calculation control
C    127 CEFEQ     Impact-parameter CE calculation control
C    128 FZION     ZION-multiplier for diffusion calculation
C    129 FROSC     parameter for default CE-values calculation
C    130 FSTKM     Hydrogen Stark splitting reduction factor
C    131 FRCDL     Hydrogen Stark splitting line elimination criterion
C    132 FMCDL     Hydrogen Stark splitting line elimination criterion
C    133 CSDW      Hydrogen Stark splitting components attenuation
C    134 CLVLS     diffusion calculation parameter, for GHL
C    135 WAVMN     limit for automatic additional wavelengths
C    136 WAVMX     limit for automatic additional wavelengths
C    137 CVZ       fluid velocity parameter
C    138 CDZ       fluid velocity parameter
C    139 COMU      mu-value for CO-lines-opacity
C    140 BMWAC     beam width parameter for eclipse calculation
C    141 SRCO      scattering ratio for CO lines
C    142 ALOMI     limit wavelength for averaged line opacity
C    143 ALOMA     limit wavelength for averaged line opacity
C    144 HNAJL     HND-adjustment limit for HSE calculation
C    145 CORMN     wavelength limit for ORIGINS, CONTRIBUTORS
C    146 CORMX     wavelength limit for ORIGINS, CONTRIBUTORS
C    147 FMVLM     FMV (fluid velocity multiplier) limit
C    148 RCOMN     C or O abundance ratio lower limit
C    149 CWJ       RHOJ calculation parameter
C    150 PNH       scattering albedo parameter
C    151 CLM       scattering albedo parameter
C    152 CFHE      mass flow parameter (Helium)
C    153 SN1CC     convergence criterion, Special-N1
C    154 XLMD3     Lyman Alpha opacity DR-formula parameter
C    155 WSN1D     weight for Special-N1 & -NK (diffusion)
C    156 AOWXP     alpha normalization weight exponent (diffusion)
C    157 CTCO      NCO temperature enhancement factor
C    158 CTMX      maximum allowed value of FCT
C    159 SHCOP     CO photospehric scale height
C    160 ZRCO      CO reference height
C    161 SHCOC     CO chromospheric scale height
C    162 XLMXC     Lyman Alpha opacity DR-formula parameter
C    163 XLMXP     Lyman Alpha opacity DR-formula parameter
C    164 XLMD2     Lyman Alpha opacity DR-formula parameter
C    165 XLMCR     Lyman Alpha opacity parameter
C    166 WRTMN     rates integrations wavelength limit
C    167 WRTMX     rates integrations wavelength limit
C    168 YRATS     damping parameter for standard rates integr.
C    169 XLMH      H Ly lines (>15) opacity wavelength cutoff
C    170 SCVA      VAX-table generating parameter
C    171 SCVS      VAX-table generating parameter
C    172 SCTA      shock TE increment amplitude
C    173 SCTS      shock TE increment scale height
C    174 SCVB      VAX-table generating parameter
C    175 SCPS      VAX-table generating parameter
C    176 CN1S      rcheck-criterion for Special-N1 (diffusion)
C    177 DELLM     DEL-criterion to replace FULL solution
C    178
C    179
C    180
C    181
C    182
C    183
C
C
C      1 NDW       index of Reference Doppler Width
C      2 MS        upper level of reference transition
C      3 NS        lower level of reference transition
C      4 ISUB      number of sub-iterations
C      5 IDFSW     DIDH details print switch
C      6 IRLS1     RL integration scheme choice switch
C      7 JSTIN     Input Check Only switch
C      8 IOMX      number of overall iterations
C      9 IRLSN     RL integration scheme choice switch
C     10 JBD       numerical BD-option specification
C     11 IXSTA     printout of Execution Analysis data
C     12 JRHO      numerical Rho-option specification
C     13 ILI       a RHOW parameter
C     14 NIL       a RHOW parameter
C     15 MFONT     Fontenla atmosphere data output switch
C     16 LNLIM     limit for Saturation Approximation in Lyman calc.
C     17 TOPE      Continuum Blocks summary save switch
C     18 IPEX      extra printout switch
C     19 LYMIT     number of Lyman iterations
C     20 IHSLT     number of HSL iterations
C     21 NGRL      graph Z-scale limit
C     22 NGRR      graph Z-scale limit
C     23 INFSM     first Lyman RK-1 smoothing index
C     24 INLSM     last Lyman RK-1 smoothing index
C     25 METEP     Lyman Epsilons method selection switch
C     26 KNFRM     OPAC and BHS detail print format switch
C     27 KURIN     index for selecting Kurucz Opacity
C     28 KINMX     Kurucz Opacity plot depth selection parameter
C     29 KININ     Kurucz Opacity plot depth selection parameter
C     30 MDTR1     Dust Temperature calculation parameter
C     31 MDTR2     Dust Temperature calculation parameter
C     32 KKPR      Level-N-to-Continuum detail print index
C     33 KOLEV     Level-N-to-Continuum level index
C     34 JNUNC     Jnu input switch for Partial Redistribution
C     35 JSTCN     Continuum Calculations only switch
C     36 IZOPT     graph Z-scale mode switch
C     37 JZOPT     graph Z-scale mode switch
C     38 IVOIT     Voigt Function routine selector
C     39 NVOIT     Voigt Function statistics keeping switch
C     40 JBDNC     Rho and b-ratio calculation bypass switch
C     41 IMUCD     index oF XMU for C.S.F. debug printout
C     42 M304      HE304 reference value index
C     43 LSTMP     STIM for GTN details print switch
C     44 NCOSW     Carbon Monoxide abundance correction switch
C     45 NTAN      Shell ray selection parameter
C     46 JBFSW     supplementary levels B method switch
C     47 IHEDF     use He=0 in ambipolar diffusion calculations
C     48 LDINT     transition terms detail print depth increment
C     49 LDTYP     transition terms detail print type switch
C     50 KUDNT     Stat. Opac. data dump wavelengths interval
C     51 JH1       Photoionization rate multiplier index
C     52 JH2       Photoionization rate multiplier index
C     53 ISRCD     index of Shell ray for C.S.F. debug printout
C     54 IDRCD     index of Disk ray for C.S.F. debug printout
C     55 NHTSW     H2 abundance correction switch
C     56 IONST     stage of ionization of ion of run
C     57 IPR01     "PERDMP-1,2" limiting index
C     58 IPR02     "PERDMP-1,2" limiting index
C     59 IPR03     "PERDMP-1,2" limiting index
C     60 IPR04     "PERDMP-1,2" limiting index
C     61 MSKIP     Shell-ray WN-matrix control
C     62 IPRFA     performance data archive record switch
C     63 IRPUN     RABD-calculation data output switch
C     64 IDEX      extra Dayfile information switch
C     65 KALOR     Hi/Bye/Abort-system control parameter
C     66 MCON      CO number density output switch
C     67 NIASM     sequential smoothing parameter
C     68 KOMNV     Composite Line Opacity input data table length
C     69 KOMNP     Composite Line Opacity input data table length
C     70 KOMNT     Composite Line Opacity input data table length
C     71 KODNT     Compos. Opac. data dump wavelengths interval
C     72 JHEAS     secret HEABD switch, for diffusion
C     73 MAMAS     matrix elements magnitudes scanning switch
C     74 ISNUD     P.R.D. Snu-shift debug dump switch
C     75 IDWIN     DP-dump index increment
C     76 ITRFI     TR-iteration output control
C     77 NSPED     NS for particle energy dissipation calculation
C     78 IFXDS     Continuum Flux detail output control
C     79 NVDFE     dump output control for injection function FINJ
C     80 NNDFE     dump output control for injection function FJIN
C     81 NZDFE     dump output control for fast electrons calc.
C     82 IPZER     dump output control for A/B when B=0
C     83 IHEAB     reference depth index for RHEAB calculation
C     84 IHDMP     dump output switch for H calculation
C     85 IRTIS     incident radiation table interpolation switch
C     86 KOOLS     Hydrogen-runs total cooling rate parts control
C     87 MH2N      H2 number density output switch
C     88 ISCRS     "in-memory" scratch I/O control
C     89 LYODS     H Lyman background lines dump switch
C     90 KDIAG     N1(diffusion) diagonal matrix method selector
C     91 N1MET     N1(diffusion) method selector
C     92 ISMBD     Intensity integration (SIMBA) dump interval
C     93 IRUNT     Run "type" (production or development)
C     94 NOION     "no ion" switch (a form of "continuum only")
C     95 NERM      "Edith" error messages limit
C     96 KARB      Character choice for banner page (-1,+1)
C     97 ISOD      depth index for "Sobolev" integration dump
C     98 IPRDD     depth-interval for PRD printout
C     99 IPRDF     frequency-interval for PRD printout
C    100 IPPOD     "population ion" absorption/emission dump switch
C    101 MN1       depth index of N1 for ambipolar diffusion
C    102 IDRDP     depth index for DRDMP option
C    103 KDRDP     frequency index for DRDMP option
C    104 KAPDB     continuum contributions control debug switch
C    105 MTHEI     method switch for Exponential Integral
C    106 MDFV      diffusion velocities output switch
C    107 NCOPT     CO opacity statistics keeping switch
C    108 LSFGC     Line Source Function graphs control code
C    109 NODCG     depth index for diffusion calculation graphs
C    110 MNG1      limit index for GNV-1 replacement
C    111 IDFDM     diffusion d-coefficients method selection switch
C    112 IDFDI     diffusion d-coefficients dump depth index
C    113 ISNDD     S(ND) calculation dump switch
C    114 IDEDP     diffusion d-coefficients debug dump switch
C    115 IBRDP     ion broadening (hydrogen) dump switch
C    116 ICXDP     charge exchange dump depth index
C    117 NSPRD     (secret PRD switch, used in MANA)
C    118 ICHDP     H-atoms collisions debug dump index
C    119 ISMSW     iteration summaries form switch
C    120 ICDIT     dI/dh continuum wavelengths selector
C    121 IRATE     index for minimal RATES printout
C    122 ICHSW     collisions with Hydrogen switch
C    123 IHSSW     Hydrogen Stark broadening switch
C    124 IHSDP     Hydrogen Stark broadening dump switch
C    125 IHSDD     Hydrogen Stark broadening dump switch
C    126 IHSKM     Hydrogen Stark broadening table limit
C    127 IHSSM     Hydrogen Stark broadening table limit
C    128 NDWM      index of Reference Doppler Width, Atmos. Model
C    129 LHHSE     reference depth index for H & M, in HSE
C    130 LX2DS     O-II background lines dump switch
C    131 LX3DS     O-III background lines dump switch
C    132 KMMAX     limit for full-XI tables tength
C    133 ISTRK     Hydrogen Stark-splitting default NE-index
C    134 NBS       b-smoothing control index
C    135 NANA1     Profile ANALYSIS depths selection parameter
C    136 IHSSP     Hydrogen Stark splitting control switch
C    137 NANA2     Profile ANALYSIS depths selection parameter
C    138 LOGAS     location analysis graph abscissa switch
C    139 NECLP     Continuum Eclipse printing selector
C    140 NARB      number of "banner" pages
C    141 KAVNT     averaged line opacity input table length
C    142 KAVNP     averaged line opacity input table length
C    143 KAVNZ     averaged line opacity input table length
C    144 IXNCS     lowering of IP for Hydrogen CE's & CI's
C    145 IRFNC     index for reference value of NC
C    146 JDMCI     debug dump switch for CII calculation
C    147 JDMCE     debug dump switch for CEIJ calculation
C    148 IDNRT     debug switch for DNRT,DNRTC in Lyman
C    149 JHBFD     H-bf dump switch
C    150 MOPRN     print built-in population-ion models
C    151 IWSMD     WAVELENGTH summary Part-2 switch
C    152 LWNT      "Line" opacity printouts wavelengths interval
C    153 KDFD1     switch for derivatives method in diffusion
C    154 KDFGS     GNV-fudging switch
C    155 KDFGA     GNV-fudging index
C    156 KDFGB     GNV-fudging index
C    157 ITN1R     N1-recalculation iterations limit
C    158 LODCG     depth index for diffusion calculation graphs
C    159 I4DFM     N1(diffusion) 4-diag formulation selector
C    160 I4DEQ     N1(diffusion) 4-diag equations selector
C    161 I4DIO     N1(diffusion) 4-diag inward/outward selector
C    162 MSSPR     simultaneous Special-N1 matrix print switch
C    163 NEFDF     switch for NE for dees in diffusion
C    164 IPDIJ     DIJ abbreviated printout switch (diffusion)
C    165 IPDEE     d-coefficients print switch (diffusion)
C    166 IBTSW     beta-set-up switch (diffusion)
C    167 KB1WA     B1-weights index
C    168 KB1WB     B1-weights index
C    169 KDAMP     N1(diffusion) matrix solution damping switch
C    170 KBNDS     N1(diffusion) boundary conditions switch
C    171 N1NUP     N1(diffusion) populations-of-the-run update
C    172 MDFG      diffusion GVL-term output switch
C    173 INDRN     input ND,NK normalization switch
C    174 MKURU     Kurucz spectrum data output switch
C    175 KHFFS     Hff contribution to Total Hydrogen cooling
C    176 MTREF     TR-effective output switch
C    177 KATNU     TNU-analysis switch
C    178 ISCMP     LSF-comparison printout details switch
C    179 KB1WS     B1-weights type selection switch
C    180 KOELS     ORIGIN every-line switch
C    181 JATOM     zero-print mode switch for ATOM
C    182 JATMO     zero-print mode switch for ATMOSPHERE
C    183 IGMSW     H Ly alpha and beta GMMA switch
C    184 NLY       H Ly lines background opacity limit
C    185 KCOAA     Comp. Line Op. analysis printout switch
C    186 IWEIT     print-switch for weighting details
C    187 IFALL     H Ly-alpha wing background opacity switch
C    188 NQLYM     Highest H Lyman lines weight limit
C    189 JXNCS     lowering of IP for Hydrogen Einstein A's
C    190 JHLSK     H Lyman lines Stark broadening switch
C    191 NMLR      mass-loss-rate reference index
C    192 JSSV      shock TE increment position index
C    193 JNEDP     N-editing dump switch
C    194 JEDIT     N-editing depth index
C    195 MBREC     B-editing switch (to keep in sync with N's)
C    196 JATAW     force WRAT/RRCP output
C    197 IBNVW     depth index for N and B calculation details
C    198 NDSN1     Diffusion Special N1 suppression switch
C    199 IXASM     smoothing detail dump index (IPEX=15)
C    200 MXPPI     limit for KZAUG values (diffusion)
C    201 MXTAP     limit for KZAUG sum (diffusion)
C    202 ITKZA     Z-augmentation iteration limit (diffusion)
C    203 IDFDS     d-coefficients smoothing (diffusion)
C    204 LDFD1     smoothing of computed derivative
C    205 NGNV      GNV suppression limit
C    206 IPIJG     GNV-in-PIJ fudging switch
C    207 ISMVE     small-values editing switch
C    208 JSFEX     LSF solution explanation print switch
C    209 LOXDS     O-I background lines dump switch
C    210 IORIC     line-center depths-of-formation switch
C    211 LHEDS     He-II background lines dump switch
C    212
C    213 IGII      GII function routine selector
C    214 ITPRD     PRD-iterations limit
C    215 LEEDS     He-I background lines dump switch
C    216 KXLYM     XK-table augmentation switch
C    217 LPVEL     Profile-velocities print switch
C    218 LPMLR     mass-loss-rates print switch for LPVEL
C    219 KLDIN     "Lyman" dump depth interval
C    220 KLFIN     "Lyman" dump frequency interval
C    221 MSEDG     METSE default, general
C    222 MSEDW     METSE deafult, weak lines
C    223 MCEOF     CE on-the-fly switch
C    224 MCIOF     CI on-the-fly switch
C    225 INCEI     CE/CI comparisons depth index
C    226 ICIH1     Hydrogen default CI-values selector
C
C
C      1 QNAME     name of ion of run
C      2 QELSM     element symbol of ion of run
C      3 QMODL     "Atmosphere Name" for MODEL DATA page
C      4 QATOM     "Ion Name" for ATOM data page
C      5 QALHD     Hi/Bye/Abort-system control parameter
C     !EJECT
C
C     E V O L V I N G   P A R A M E T E R S
C
C
C     These parameters are kept in the arrays REST, LEST and QEST,
C     in labelled commons MISC1, MISC2 and MISC3, respectively.
C
C      1 WTPZ      static-profile weight for flow-broadening
C      2 R1GD      "R1N" for Geometrical Dilution
C      3 YALYM     final Y for Lyman frequency integration
C      4 XLCOW     CO-lines opacity dump wavelength
C      5 RFXNC     reference value of XNC (for CE and CI)
C      6 YASYM     final Y for symmetric profile integration
C      7 YAFUL     final Y for full profile integration
C
C
C      1 KSHEL     K-Shell ionization correction switch
C      2 IOVER     overall iteration count
C      3 ITER      sub-iteration count
C      4 NSHL      number of Shell rays
C      5 MHL       limit for number of H- wavelengths
C      6 KPRSW     constant-pressure NH adjustment switch
C      7 NPT       number of passive transitions
C      8 NRPMX     maximum number of points along a Shell ray
C      9 NH2CS     H2 abundance correction switch
C     10 KZERO     NH adjustment index (for HSE)
C     11 KTKIN     input TAUK switch
C     12 LGGIN     input CLOGG switch
C     13 J304I     He II 304 Line Jbar input switch
C     14 NTK       number of P.R.D. Continuum wavelengths
C     15 LFLX      number of transitions with Line Flux Distrib.
C     16 MSFQR     S.F. method summary - QR direct
C     17 MOMET     model change switch
C     18 MSFQM     S.F. method summary - QR mapped
C     19 ITHSL     HSL iteration count
C     20 MSFRT     S.F. method summary - RT
C     21 MSFGR     S.F. method summary - GR
C     22 JPOP      =1 if this is a Population Update run
C     23 J304S     He II 304 Line Jbar save switch
C     24 LITER     Lyman iteration count
C     25 NOTX      radiative Eclipse Profile switch (=0 means do none)
C     26 KNZGM     non-zero Gammas switch
C     27 NOTA      radiative Line Profiles switch (=0 means do none)
C     28 NXRW      number of wavelengths with X-ray absorption
C     29 NONC      number of radiative transitions with P.R.D.
C     30 KMASN     input mass switch
C     31 KMUS      mu-tables subset switch
C     32 KAMB      ambipolar diffusion calculation switch
C     33 LDLMX     maximum value of LDL(u,l) (an input parameter)
C     34 NEWZ      Z-recalculation signal
C     35 NLFDB     number of transitions with frequency-dependent
C                    background data
C     36 NVSB      Sobolev solutions indicator
C     37 KVSB      "Sobolev" velocity descriptor
C     38 KTRAS     "Artificial TAU" descriptor
C     39 KDGV      statistical equilibrium diffusion terms switch
C     40 NCOW      number of wavelengths with CO-lines absorption
C     41 KIMS      "in-memory" scratch I/O switch
C     42 IVNH      V(NH) use switch
C     43 MLSFP     line source function printouts summary switch
C     44 LCOW      number of CO lines used
C     45 NGDZ      number of GDZ values
C     46 KGDT      GDT-terms switch
C     47 KVLG      mass motions calculations switch
C     48 KCRH      H- cooling rate signal
C     49 KDFA      diffusion analysis switch
C     50 NVSBP     Sobolev profiles indicator
C     51 JIBR      ion broadening switch
C     52 JDDL      DDL-adjustment signal (Hydrogen lines)
C     53 LDLMU     largest actual value of LDL
C     54 MFMV      FMV-signal for fluid velocity calculation
C     55 MCXK      upper-level charge exchange run code
C     56 KNEGA     negative-A signal
C     57 KX2OK     O-II background line status switch
C     58 KX3OK     O-III background line status switch
C     59 JHEAB     RHEAB recalculation signal
C     60 KBTMX     maximum value of KBT(u,l) (an input parameter)
C     61 KRTMX     maximum value of KRT(u,l) (an input parameter)
C     62 KSTMX     maximum value of KST(u,l) (an input parameter)
C     63 MPROM     Line Profile function calculation mode switch
C     64 IDGMZ     DGM(Z) use switch
C     65 KTECH     TE-was-edited signal
C     66 KVXVA     VXS has been set equal to VAX(1)
C     67 MWNSV     count of WN-matrices save from PRD continuum
C     68 KZXST     Z-table existence switch
C     69 KFCEU     FCE update switch
C     70 KFELE     fast-eletrons term use switch
C     71 KCHIJ     collisions-with-Hydrogen term use switch
C     72 KOXOK     O-I background line data status
C     73 KHEOK     He-II background line data status
C     74 KALTG     alternate PRD GMMA is used
C     75
C     76 KDZIN     input-NDs = zero signal for diffusion
C     77 KEEOK     He-I background line data status
C     78 IGKSW     input-GK switch (=1: there was input)
C     79 NCINM     length of QIONM
C     80 KCHKI     collisions-with-Hydrogen term use switch
C     81 KLYNF     H Ly lines normalization factor switch
C     82 KXNUC     XNUC-was-specified switch
C
C
C      1 QIONM     full name of ion-of-the-run
C     !EJECT
C
C
C     O P T I O N S
C
C
C     The option switches are kept in the array IQQ (intended use:
C     read-only), in labelled common OPTIONS.
C
C     The specifications of what these options do are printed by
C     subroutine FOP, at the start of the printout for a run.
C
C     The following table lists equivalences between 3-letter codes
C     and the complete option names.
C
C         1 IQANA ANALYSIS     2 IQSCL SCALE        3 IQSEB SEBUG
C         4 IQLTD LTEDATA      5 IQPH2 PHASE2       6 IQECL ECLIPSE
C         7 IQEPC EPCOMP       8 IQSFO SPHOUT       9 IQLGT LIGHT
C        10 IQSTD TRANSAV     11 IQCPA ADDCOPR     12 IQOPO OPAPRNT
C        13 IQLYM LYMAN       14 IQCSW CSWITCH     15 IQARD ARHODMP
C        16 IQHSE HSE         17 IQSET SETIME      18 IQSUM SUMMARY
C        19 IQEVR EVERY       20 IQKUP KURPRNT     21 IQSTO STANPRNT
C        22 IQFLD FELEDMP     23 IQSTI STIMPRNT    24 IQOST RATEPRNT
C        25 IQDPW DPDWPRNT    26 IQTAF TAUPRNT     27 IQCAP CARPRNT
C        28 IQFBO BDPRNT      29 IQNUM POPPRNT     30 IQDDP DRDMP
C        31 IQSFS SPHERE      32 IQVTV VTV         33 IQLTE LTE
C        34 IQNPL POPGRAF     35 IQNHA NHADJ       36 IQHNP HNPRNT
C        37 IQAPC APHICOPR    38 IQENH ENHANCE     39 IQCSD CSFDMP
C        40 IQESD ECLIDMP     41 IQIRC INSCARD     42 IQPPR PASSPRNT
C        43 IQTNO PRODMP      44 IQIGR INTGRAF     45 IQCSP CSFPRNT
C        46 IQCSF CSF         47 IQCGR CHKGRAF     48 IQHEP HELPRNT
C        49 IQFIN FINITE      50 IQREF REFLECT     51 IQINC INCIDNT
C        52 IQCDP RATECOPR    53 IQUTR USETRIN     54 IQMNT MONOTAU
C        55 IQEMI EMERINT     56 IQNES NESWICH     57 IQLID LINTDMP
C        58 IQEID EMINDMP     59 IQABS OPASUM      60 IQGDS GDS
C        61 IQIRL ILRT        62 IQRSQ RSQUARE     63 IQOPG OPAGRAF
C        64 IQDNT TAUDMP      65 IQCPL LINECOPR    66 IQEMO EMIPRNT
C        67 IQELO ELEPRNT     68 IQHMS HMS         69 IQHMP HMSCOPR
C        70 IQNCP PRDPRNT     71 IQENL ENL         72 IQEN2 ENL2
C        73 IQPRP PRDCOPR     74 IQSIP SILPRNT     75 IQSEP SEPRNT
C        76 IQSTA STANDARD    77 IQUWT USEWTAB     78 IQIBE INBED
C        79 IQPTN PTN         80 IQCFX CONFLUX     81 IQFLP FELEPRNT
C        82 IQPWT WTABPRNT    83 IQSLY SLYR        84 IQCOT COLTEMP
C        85 IQPAI APHIPRNT    86 IQGDP GDSDMP      87 IQNDP INDPRNT
C        88 IQIJR INTRPRNT    89 IQLSC LSCALE      90 IQH2P HEL2PRNT
C        91 IQALP ALUPRNT     92 IQEMG EMIGRAF     93 IQCSG CSFGRAF
C        94 IQALL ALL         95 IQRGR RATEGRAF    96 IQSPC SPECSUM
C        97 IQCPU CONSAV      98 IQMGP MAGPRNT     99 IQDT2 DUSTYPE
C       100 IQDTP DUSTCOPR   101 IQORI ORIGIN     102 IQDEF FLUXDMP
C       103 IQSEC SECOMP     104 IQEMS EMISUM     105 IQTAS TAUSUM
C       106 IQBRF RHOFUDGE   107 IQALY ALLY       108 IQRID RATEFULL
C       109 IQISS ITERS      110 IQIRH ITERRHO    111 IQITA ITERTAU
C       112 IQIRK ITERRK     113 IQINN ITERN      114 IQIRW ITERRWT
C       115 IQLYC LYMCOPR    116 IQLYD LYMDMP     117 IQSPU STATPRNT
C       118 IQSNB NBPRNT     119 IQGDM GDMP       120 IQROD RHBPRDT
C       121 IQEDP EPDMP      122 IQEPS EPSNEDC    123 IQWSR WATESTR
C       124 IQWSE WATESTE    125 IQSUP SULPRNT    126 IQCRK COMPRK
C       127 IQINE ITERNE     128 IQIBD ITERB      129 IQLND LNUMDMP
C       130 IQPBD BDMP       131 IQFEP FEPRNT     132 IQHSD HSEDMP
C       133 IQCCR CALCOOL    134 IQSCD COOLSAV    135 IQORT ORT
C       136 IQIZZ ITERZ      137 IQPD1 PERDMP1    138 IQPD2 PERDMP2
C       139 IQFCD HFFCOOLD   140 IQCCI COOLINT    141 IQIFF INCIFRNT
C       142 IQIDP ITDMP      143 IQCFD CNFLXDMP   144 IQIVK IVALICK
C       145 IQESL SEDITIF    146 IQWDD WNDMP      147 IQRSM RSMOOTH
C       148 IQSSP SQSMPRNT   149 IQSED SEDIT      150 IQRED RHEDIT
C       151 IQPPU PROSAV     152 IQJNT JNTRPOL    153 IQBLN BLENDMP
C       154 IQNCJ USENCJ     155 IQPRE RCOMPRNT   156 IQTOP SPHETAU
C       157 IQPSG SPHEGEOM   158 IQPBS POPBSW     159 IQPD0 PERDMP0
C       160 IQEPU ECLISAV    161 IQBED BEDIT      162 IQEGR ECLIGRAF
C       163 IQTNG TANG       164 IQPPF PARTPRNT   165 IQUVP PARTVAR
C       166 IQCSB CSFB       167 IQPPJ JNUPRNT    168 IQQSE QSFEDIT
C       169 IQEXA EXPAND     170 IQKOP KOMPRNT    171 IQCPC COMPCOPR
C       172 IQSHF SNUSHFT    173 IQVSW VSWITCH    174 IQIXD INDXDMP
C       175 IQPED PED        176 IQEDD PEDDMP     177 IQFEL FELEC
C       178 IQCFE FELE       179 IQPDC PDCHECK    180 IQRTA RATEALL
C       181 IQKSP KSHLCOPR   182 IQHMJ HMSJPRNT   183 IQHMO HMSONLY
C       184 IQCPS CPSW       185 IQPSW EPSW       186 IQESW METSW
C       187 IQMSP METPRNT    188 IQLFD LFDPRNT    189 IQRIJ RIJPRNT
C       190 IQOSH ORSHORT    191 IQFXV FLUXSAV    192 IQAPF SPECSAV
C       193 IQLNB LONGNBM    194 IQKLC COOLCOM    195 IQKLD COMCRID
C       196 IQXRC COOLXRAY   197 IQXRI XRAYCRID   198 IQKRD KROSSID
C       199 IQROP RHBPRNT    200 IQPFC FDBCOPR    201 IQINH ITERNH
C       202 IQFDD FDBDMP     203 IQSOD SOBDMP     204 IQRWO RHOWOPT
C       205 IQINI MITPRNT    206 IQPD3 PERDMP3    207 IQISF WISFILE
C       208 IQEBI EMERBACK   209 IQSOP SODPRNT    210 IQCLP CALPRNT
C       211 IQD2D DUSTDMP    212 IQND2 DUSTEMP    213 IQITD ITERTD
C       214 IQVES VESCAPE    215 IQJLY JLYSAV     216 IQWSP WAVEPRNT
C       217 IQMIX MAKIX      218 IQSOI SOBINT     219 IQAMD AMDIFF
C       220 IQADP AMDDMP     221 IQVLG VELGRAD    222 IQVDP VELGDMP
C       223 IQCOD CODMP      224 IQCCP COCOPR     225 IQCOC COOLCO
C       226 IQCRD COCRID     227 IQSTM STAUREDM   228 IQOPP OPTPRNT
C       229 IQPGA PEGTNALL   230 IQHNM HENORM     231 IQCHR CALHEAT
C       232 IQISO ISCRS      233 IQDIR DOION      234 IQDFA DIFFANA
C       235 IQNVT NVOIT      236 IQPPS IXSTA      237 IQBNC JBDNC
C       238 IQO2P OXY2PRNT   239 IQVLP VLGPRNT    240 IQJIN JSTIN
C       241 IQRCO RABDAT     242 IQPDV IRUNT      243 IQSCP TOPE
C       244 IQGNV GNVCALC    245 IQBDC BDCALC     246 IQSRJ PESRJALL
C       247 IQAMP ATMOPRNT   248 IQATP ATOMPRNT   249 IQINP INDAPRNT
C       250 IQLSG LSFGRAF    251 IQCGP CHKPRNT    252 IQBDG BDGRAF
C       253 IQTEG TEGRAF     254 IQACP ACSFPRNT   255 IQLYA ALYMPRNT
C       256 IQANU APOPPRNT   257 IQAHS AHSEPRNT   258 IQAOP AOPTPRNT
C       259 IQASP ALSFPRNT   260 IQADA AINDPRNT   261 IQAPP APRFPRNT
C       262 IQOPE OPANEG     263 IQSDI SDIRECT    264 IQAMB AMBPRNT
C       265 IQVLS VELS       266 IQDSM DSMOOTH    267 IQA1P ADN1PRNT
C       268 IQA1D ADN1DMP    269 IQSSD SQSMDMP    270 IQHSV HSEV
C       271 IQBSM BSMOOTH    272 IQAN1 AMDN1      273 IQHBR HBROAD
C       274 IQHST HSTSUMM    275 IQGTS GTNSTIM    276 IQCXP CHXPRNT
C       277 IQCXD CHXDMP     278 IQCIJ CIJPRNT    279 IQPIJ PIJPRNT
C       280 IQTPL TAUPLOT    281 IQHEA HEABD      282 IQCHP COLHPRNT
C       283 IQSMG SUMGRAF    284 IQSMT SUMTREND   285 IQIWA INTAPRNT
C       286 IQAIW AINTPRNT   287 IQLSP LSFPRNT    288 IQDIC DIDHC
C       289 IQDIL DIDHL      290 IQWNM WAVENUMB   291 IQTRP TRPRNT
C       292 IQPZT ZPRNT      293 IQDAS ATOMSAV    294 IQSLV SLFSAV
C       295 IQSLP SLFPRNT    296 IQSLG SLFGRAF    297 IQIXT INPEX
C       298 IQIXW INPEXW     299 IQTRC TRUECONT   300 IQTCP TRUECOPR
C       301 IQBQD BDQPRDT    302 IQICK ITERCHK    303 IQAVK AVCON
C       304 IQSTW STKWATT    305 IQCOE COCLIPSE   306 IQEND NEDIT
C       307 IQALO AVELOP     308 IQALD AVOPRNT    309 IQRGS RATESUMM
C       310 IQSSM SSMOOTH    311 IQBRD BRATDMP    312 IQIRE IRHWED
C       313 IQLCP LINECOMP   314 IQLCD LINECDMP   315 IQROS RHBPRSM
C       316 IQDLA DELABRT    317 IQMXC MCINPUT    318 IQUET INTEDIT
C       319 IQNRS NRSMOOTH   320 IQCOA COMOPAN    321 IQUTM USETSM
C       322 IQSBP STANCOPR   323 IQQHR ITERCHI    324 IQINB INNBPRNT
C       325 IQBNV PDETPRNT   326 IQZCP ZCOMP      327 IQOXP OXYPRNT
C       328 IQCEF CEFACTS    329 IQPCP PROCPRNT   330 IQCXL CHEXLO
C       331 IQCXU CHEXUP     332 IQGSM GTNSMTH    333 IQLXL CHEXLOL
C       334 IQRKE RKINCR     335 IQFLW FLWBROAD   336 IQFBP FLWBPRNT
C       337 IQPMH PRDMETH    338 IQPIP PRDITER    339 IQLNC CLNORM
C       340 IQLNU ULNORM     341 IQLBD LBDPRNT    342 IQPFU LSFFULL
C       343 IQAED AEDIT      344 IQOTL OPTHINL    345 IQO3P OXY3PRNT
C       346 IQACC ALLCICE
C     !EJECT
C
C     !DASH
      save
C     !DASH
C
      external KOSMOS, RIGEL, HI, BYE
C
C
C     !EJECT
C
C     L A B E L L E D   C O M M O N   B L O C K S
C
C
C     (They SHOULD all be collected here, in one place!)
C
C
C---- P A R T  1: Storage, pointers and blocks indices
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
C     .
      equivalence
     $(IZOQ(  1),JJEQT),(IZOQ(  2),JJEP2),(IZOQ(  3),JJOML),
     $(IZOQ(  4),JJMU ),(IZOQ(  5),JJ304),(IZOQ(  6),JJRKX),
     $(IZOQ(  7),JJTE ),(IZOQ(  8),JJTR ),(IZOQ(  9),JJXNE),
     $(IZOQ( 10),JJHNF),(IZOQ( 11),JJHND),(IZOQ( 12),JJV  ),
     $(IZOQ( 13),JJYBR),(IZOQ( 14),JJXIN),(IZOQ( 15),JJFIN),
     $(IZOQ( 16),JJGM ),(IZOQ( 17),JJRK ),(IZOQ( 18),JJRL ),
     $(IZOQ( 19),JJQU ),(IZOQ( 20),JJQS ),(IZOQ( 21),JJFNH),
     $(IZOQ( 22),JJTS ),(IZOQ( 23),JJBIJ),(IZOQ( 24),JJWEI),
     $(IZOQ( 25),JJALF),(IZOQ( 26),JJXNU),(IZOQ( 27),JJP  ),
     $(IZOQ( 28),JJCP ),(IZOQ( 29),JJCII),(IZOQ( 30),JJCEI),
     $(IZOQ( 31),JJCK ),(IZOQ( 32),JJAIJ),(IZOQ( 33),JJMSI),
     $(IZOQ( 34),JJAL ),(IZOQ( 35),JJZIN),(IZOQ( 36),JJCEK),
     $(IZOQ( 37),JJZ  ),(IZOQ( 38),JJMSR),(IZOQ( 39),JJBAT),
     $(IZOQ( 40),JJXSH),(IZOQ( 41),JJVNH),(IZOQ( 42),JJALK),
     $(IZOQ( 43),JJBTR),(IZOQ( 44),JJBDI),(IZOQ( 45),JJRHO),
     $(IZOQ( 46),JJXDR),(IZOQ( 47),JJDDR),(IZOQ( 48),JJZME),
     $(IZOQ( 49),JJVT ),(IZOQ( 50),JJNK ),(IZOQ( 51),JJNKS),
     $(IZOQ( 52),JJRZM),(IZOQ( 53),JJRAB),(IZOQ( 54),JJT5 )
      equivalence
     $(IZOQ( 55),JJPEL),(IZOQ( 56),JJPGS),(IZOQ( 57),JJPTU),
     $(IZOQ( 58),JJPTO),(IZOQ( 59),JJXND),(IZOQ( 60),JJMSH),
     $(IZOQ( 61),JJYLM),(IZOQ( 62),JJTDN),(IZOQ( 63),JJCRH),
     $(IZOQ( 64),JJFKR),(IZOQ( 65),JJWAV),(IZOQ( 66),JJRKQ),
     $(IZOQ( 67),JJRLQ),(IZOQ( 68),JJPNF),(IZOQ( 69),JJRRN),
     $(IZOQ( 70),JJRRC),(IZOQ( 71),JJTEX),(IZOQ( 72),JJYK ),
     $(IZOQ( 73),JJPAB),(IZOQ( 74),JJRKC),(IZOQ( 75),JJTKI),
     $(IZOQ( 76),JJYCO),(IZOQ( 77),JJYRA),(IZOQ( 78),JJYWA),
     $(IZOQ( 79),JJTER),(IZOQ( 80),JJAEL),(IZOQ( 81),JJYBC),
     $(IZOQ( 82),JJQIN),(IZOQ( 83),JJQOU),(IZOQ( 84),JJLHM),
     $(IZOQ( 85),JJAHM),(IZOQ( 86),JJBHM),(IZOQ( 87),JJYHM),
     $(IZOQ( 88),JJPKS),(IZOQ( 89),JJYDT),(IZOQ( 90),JJLDT),
     $(IZOQ( 91),JJADT),(IZOQ( 92),JJABD),(IZOQ( 93),JJLMM),
     $(IZOQ( 94),JJMLC),(IZOQ( 95),JJXM ),(IZOQ( 96),JJLMD),
     $(IZOQ( 97),JJDFD),(IZOQ( 98),JJALD),(IZOQ( 99),JJEPD),
     $(IZOQ(100),JJLXX),(IZOQ(101),JJLDR),(IZOQ(102),JJTKR),
     $(IZOQ(103),JJYKR),(IZOQ(104),JJPBA),(IZOQ(105),JJEP1),
     $(IZOQ(106),JJMCE),(IZOQ(107),JJACE),(IZOQ(108),JJR1W)
      equivalence
     $(IZOQ(109),JJCSH),(IZOQ(110),JJOR1),(IZOQ(111),JJNE0),
     $(IZOQ(112),JJGK ),(IZOQ(113),JJPBG),(IZOQ(114),JJPGB),
     $(IZOQ(115),JJMUF),(IZOQ(116),JJLCR),(IZOQ(117),JJICR),
     $(IZOQ(118),JJYCR),(IZOQ(119),JJMSS),(IZOQ(120),JJWTP),
     $(IZOQ(121),JJXIF),(IZOQ(122),JJAF ),(IZOQ(123),JJXK ),
     $(IZOQ(124),JJH1 ),(IZOQ(125),JJBDL),(IZOQ(126),JJPRF),
     $(IZOQ(127),JJWVK),(IZOQ(128),JJARK),(IZOQ(129),JJVXS),
     $(IZOQ(130),JJXMU),(IZOQ(131),JJCMU),(IZOQ(132),JJXDK),
     $(IZOQ(133),JJMDK),(IZOQ(134),JJCDK),(IZOQ(135),JJFRR),
     $(IZOQ(136),JJHJ ),(IZOQ(137),JJBTL),(IZOQ(138),JJFRS),
     $(IZOQ(139),JJCEQ),(IZOQ(140),JJH2N),(IZOQ(141),JJPF ),
     $(IZOQ(142),JJPFT),(IZOQ(143),JJCHI),(IZOQ(144),JJPIJ),
     $(IZOQ(145),JJCIJ),(IZOQ(146),JJXIS),(IZOQ(147),JJAS ),
     $(IZOQ(148),JJXIR),(IZOQ(149),JJXNC),(IZOQ(150),JJXIB),
     $(IZOQ(151),JJDGM),(IZOQ(152),JJDWV),(IZOQ(153),JJKSR),
     $(IZOQ(154),JJFON),(IZOQ(155),JJFVS),(IZOQ(156),JJRML),
     $(IZOQ(157),JJWVC),(IZOQ(158),JJARC),(IZOQ(159),JJYWC),
     $(IZOQ(160),JJBNL),(IZOQ(161),JJBNU),(IZOQ(162),JJBNY)
      equivalence
     $(IZOQ(163),JJTIJ),(IZOQ(164),JJVXN),(IZOQ(165),JJVR ),
     $(IZOQ(166),JJMCI),(IZOQ(167),JJWMU),(IZOQ(168),JJWDK),
     $(IZOQ(169),JJWSH),(IZOQ(170),JJZBK),(IZOQ(171),JJABK),
     $(IZOQ(172),JJVSB),(IZOQ(173),JJTRA),(IZOQ(174),JJJBN),
     $(IZOQ(175),JJGVL),(IZOQ(176),JJGVI),(IZOQ(177),JJVAD),
     $(IZOQ(178),JJZT ),(IZOQ(179),JJZI ),(IZOQ(180),JJVAM),
     $(IZOQ(181),JJVM ),(IZOQ(182),JJOLL),(IZOQ(183),JJCOL),
     $(IZOQ(184),JJNCO),(IZOQ(185),JJSWV),(IZOQ(186),JJCVX),
     $(IZOQ(187),JJVPR),(IZOQ(188),JJVHA),(IZOQ(189),JJRBL),
     $(IZOQ(190),JJGDZ),(IZOQ(191),JJGDT),(IZOQ(192),JJVEL),
     $(IZOQ(193),JJV1 ),(IZOQ(194),JJV2 ),(IZOQ(195),JJV3 ),
     $(IZOQ(196),JJVBM),(IZOQ(197),JJVCM),(IZOQ(198),JJVDM),
     $(IZOQ(199),JJZ1 ),(IZOQ(200),JJZ2 ),(IZOQ(201),JJZ3 ),
     $(IZOQ(202),JJVXI),(IZOQ(203),JJZIO),(IZOQ(204),JJACI),
     $(IZOQ(205),JJPEX),(IZOQ(206),JJHNV),(IZOQ(207),JJCIA),
     $(IZOQ(208),JJCKA),(IZOQ(209),JJDIO),(IZOQ(210),JJDLV),
     $(IZOQ(211),JJRNI),(IZOQ(212),JJRKH),(IZOQ(213),JJRLH),
     $(IZOQ(214),JJXRK),(IZOQ(215),JJXRL),(IZOQ(216),JJCXP)
      equivalence
     $(IZOQ(217),JJCXX),(IZOQ(218),JJRCX),(IZOQ(219),JJHEA),
     $(IZOQ(220),JJWNU),(IZOQ(221),JJXCA),(IZOQ(222),JJXCB),
     $(IZOQ(223),JJSQS),(IZOQ(224),JJOSF),(IZOQ(225),JJFMV),
     $(IZOQ(226),JJZEC),(IZOQ(227),JJWVA),(IZOQ(228),JJARA),
     $(IZOQ(229),JJFIW),(IZOQ(230),JJZGM),(IZOQ(231),JJICO),
     $(IZOQ(232),JJCQT),(IZOQ(233),JJCQA),(IZOQ(234),JJDTE),
     $(IZOQ(235),JJSA ),(IZOQ(236),JJZRN),(IZOQ(237),JJTCO),
     $(IZOQ(238),JJFCT),(IZOQ(239),JJSCW),(IZOQ(240),JJDAH),
     $(IZOQ(241),JJS  ),(IZOQ(242),JJAW ),(IZOQ(243),JJAEW),
     $(IZOQ(244),JJWRA),(IZOQ(245),JJQHI),(IZOQ(246),JJQSA),
     $(IZOQ(247),JJQST),(IZOQ(248),JJFCE),(IZOQ(249),JJPCE),
     $(IZOQ(250),JJHIJ),(IZOQ(251),JJFCJ),(IZOQ(252),JJSIJ),
     $(IZOQ(253),JJSET),(IZOQ(254),JJBXI),(IZOQ(255),JJWLA),
     $(IZOQ(256),JJFNA),(IZOQ(257),JJWLB),(IZOQ(258),JJFNB),
     $(IZOQ(259),JJWSL),(IZOQ(260),JJXCU),(IZOQ(261),JJWCU),
     $(IZOQ(262),JJAAT),(IZOQ(263),JJGMZ),(IZOQ(264),JJYAF),
     $(IZOQ(265),JJPMG),(IZOQ(266),JJBHZ),(IZOQ(267),JJHMN),
     $(IZOQ(268),JJCHN),(IZOQ(269),JJOHN)
C     .
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
C     .
      equivalence
     $(JZOQ(  1),JJMRJ),(JZOQ(  2),JJKIJ),(JZOQ(  3),JJLRJ),
     $(JZOQ(  4),JJRKS),(JZOQ(  5),JJRLS),(JZOQ(  6),JJIKW),
     $(JZOQ(  7),JJIBE),(JZOQ(  8),JJLIJ),(JZOQ(  9),JJNPQ),
     $(JZOQ( 10),JJLRQ),(JZOQ( 11),JJLCX),(JZOQ( 12),JJNLE),
     $(JZOQ( 13),JJISV),(JZOQ( 14),JJKZA),(JZOQ( 15),JJKZU),
     $(JZOQ( 16),JJMIJ),(JZOQ( 17),JJLCH)
C     .
C
C---- BERTH       as of 1990 Nov 20
      integer     LSHF,LADR,ISHF
      dimension   ISHF(7)
      common      /BERTH/ LSHF,LADR,ISHF
C     "Part-1 to Part-2" input shuffling data block.
C     (Allocated by GRUB.)
C     .
      equivalence
     $(ISHF( 1),IIIMR ),(ISHF( 2),IIILR ),(ISHF( 3),IIILZA),
     $(ISHF( 4),IIIBNL),(ISHF( 5),IIIBNU),(ISHF( 6),IIIBNE),
     $(ISHF( 7),IIINLP)
C     .
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
C     .
      equivalence
     $(KKK( 1),KKLTIT),(KKK( 2),KKMULT),(KKK( 3),KKKONT),
     $(KKK( 4),KKDAMP),(KKK( 5),KKCKSM),(KKK( 6),KKOPAC),
     $(KKK( 7),KKSCAT),(KKK( 8),KKBHSN),(KKK( 9),KKBHSD),
     $(KKK(10),KKBHS ),(KKK(11),KKCNXP),(KKK(12),KKTAUK),
     $(KKK(13),KKJNU ),(KKK(14),KKSCON),(KKK(15),KKB   ),
     $(KKK(16),KKBNMS),(KKK(17),KKFD  ),(KKK(18),KKACTO),
     $(KKK(19),KKCO  ),(KKK(20),KKACTB),(KKK(21),KKCB  ),
     $(KKK(22),KKT1  ),(KKK(23),KKTR  ),(KKK(24),KKS1  ),
     $(KKK(25),KKSR  ),(KKK(26),KKBHS1),(KKK(27),KKBHSR),
     $(KKK(28),KKCAPR),(KKK(29),KKCAPP),(KKK(30),KKSIGM),
     $(KKK(31),KKALBD),(KKK(32),KKRESN),(KKK(33),KKISLV),
     $(KKK(34),KKLPRD),(KKK(35),KKZABS),(KKK(36),KKZSCA),
     $(KKK(37),KKZSCR),(KKK(38),KKCNDT),(KKK(39),KKBULT),
     $(KKK(40),KKKTIT),(KKK(41),KKZBNM),(KKK(42),KKITS ),
     $(KKK(43),KKISWA),(KKK(44),KKISWE),(KKK(45),KKSIGS),
     $(KKK(46),KKDL  ),(KKK(47),KKCLO ),(KKK(48),KKKODE),
     $(KKK(49),KKTTAU),(KKK(50),KKTSCN),(KKK(51),KKTOPA),
     $(KKK(52),KKTNXP),(KKK(53),KKTFD ),(KKK(54),KKLAMD),
     $(KKK(55),KKLAMP),(KKK(56),KKKNTT),(KKK(57),KKZBDN),
     $(KKK(58),KKZAXA),(KKK(59),KKZAYA)
C     .
C
C---- POPDATA     as of 2007 Jan 12
      integer     NPI
      parameter   (NPI=14)
C     (Remember to recompile all users when changing NPI.)
      real*8      POPMSS
      integer     LZOQ,MRTP,NPOPS,MAXPOPL,LENPBL,MRTPA,MRTPM,LIMPOP,
     $            LENT,NAMKNT,LENPOP,ICKSM,IUPOP,IBLAD,IPSWICH,KAPNO
      character   NAMES*10,TNAMES*8,POPSYM*3,KLABPI*8,NLABPI*8,BLABPI*8
      dimension   LZOQ(5), MRTP(50),
     $            LIMPOP(NPI), NAMKNT(NPI), LENPOP(NPI), IBLAD(NPI),
     $            ICKSM(NPI),  IUPOP(NPI),  NAMES(NPI),  IPSWICH(NPI),
     $            POPSYM(NPI), KAPNO(NPI),  POPMSS(NPI), TNAMES(NPI),
     $            KLABPI(NPI), NLABPI(NPI), BLABPI(NPI)
C
      common      /POPS01/ NPOPS,MAXPOPL,LENT,LENPBL,MRTPM,MRTPA,ICKSM
      common      /POPS02/ POPMSS
      common      /POPS03/ LZOQ
      common      /POPS04/ MRTP
      common      /POPS05/ LENPOP
      common      /POPS06/ LIMPOP
      common      /POPS07/ NAMES
      common      /POPS08/ TNAMES
      common      /POPS09/ NAMKNT
      common      /POPS10/ IUPOP
      common      /POPS11/ IBLAD
      common      /POPS12/ IPSWICH
      common      /POPS13/ POPSYM
      common      /POPS14/ KAPNO
      common      /POPS15/ KLABPI
      common      /POPS16/ NLABPI
      common      /POPS17/ BLABPI
C
C     Population Data Blocks parameters and data.
C     .
C     NPOPS   = total number of population data blocks = NPI;
C     MAXPOPL = maximum value of LIMPOP;
C     LENT    = sum of LIMPOP;
C     LENPBL  = length of a Population Data Block;
C     LZOQ    = Population Data Blocks components index, (POPN and BD
C               are arrays of the form [N,LIMP]);
C     LENPOP  = number of non-LTE population levels in a set;
C     LIMPOP  = total number of population levels in a set;
C     NAMES   = population ion names;
C     NAMKNT  = number of characters in NAMES;
C     TNAMES  = truncated population ion names;
C     IUPOP   = populations update switches;
C     IBLAD   = file address of Population Block records;
C     IPSWICH = copy of population data print option setting;
C     POPMSS  = ion mass;
C     POPSYM  = element symbol of population ion, as in ELEMENT table;
C     KAPNO   = absorber (Kappa) number of population ion;
C     ICKSM   = index of "SENNA" checksum;
C     MRTP    = "ion-of-run"-to-"population-ion-model" level indices
C               (length must be .ge. MAXPOPL). [MRTP(I)=J means:
C               level J of the "ion-of-run" correseponds to level I of
C               the built-in "population-ion-model"];
C     KLABPI, NLABPI, BLABPI = names of population in data tables, as
C                              used in input statements.
C     .
      equivalence
     $(LZOQ( 1),LLNPOP),(LZOQ( 2),LLIUP ),(LZOQ( 3),LLPOPK),
     $(LZOQ( 4),LLPOPN),(LZOQ( 5),LLBD  )
      equivalence
     $(LENPOP( 1),NLH ),(LENPOP( 2),NLC ),(LENPOP( 3),NLS ),
     $(LENPOP( 4),NLZ ),(LENPOP( 5),NZ2 ),(LENPOP( 6),NAL ),
     $(LENPOP( 7),NMG ),(LENPOP( 8),NFE ),(LENPOP( 9),NNA ),
     $(LENPOP(10),NCA ),(LENPOP(11),NLO ),(LENPOP(12),NLU ),
     $(LENPOP(13),NO2 ),(LENPOP(14),NO3 )
      equivalence
     $(IUPOP( 1),JYDRO),(IUPOP( 2),JARBO),(IUPOP( 3),JILIC),
     $(IUPOP( 4),JELIU),(IUPOP( 5),JELI2),(IUPOP( 6),JLUMI),
     $(IUPOP( 7),JAGNE),(IUPOP( 8),JIRON),(IUPOP( 9),JODIU),
     $(IUPOP(10),JALCI),(IUPOP(11),JOXYG),(IUPOP(12),JULPH),
     $(IUPOP(13),JOXY2),(IUPOP(14),JOXY3)
C     .
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
C     .
      equivalence
     $(MML( 1),MMNAM),(MML( 2),MMLAM),(MML( 3),MMXI ),(MML( 4),MMCDW),
     $(MML( 5),MMY  ),(MML( 6),MMDPM),(MML( 7),MMCRD),(MML( 8),MMCVW),
     $(MML( 9),MMCSK),(MML(10),MMCRS),(MML(11),MMFND),(MML(12),MMDP ),
     $(MML(13),MMDW ),(MML(14),MMA  ),(MML(15),MMGTN),(MML(16),MMTAU),
     $(MML(17),MMCOP),(MML(18),MMCSF),(MML(19),MMPE ),(MML(20),MMFE ),
     $(MML(21),MMEP ),(MML(22),MMB  ),(MML(23),MMBS ),(MML(24),MMBC ),
     $(MML(25),MMCND),(MML(26),MMS  ),(MML(27),MMJBR),(MML(28),MMRHO),
     $(MML(29),MMRHW),(MML(30),MMORH),(MML(31),MMCDL),(MML(32),MMTS ),
     $(MML(33),MMSS ),(MML(34),MMDDL),(MML(35),MMGTS),(MML(36),MMJBC)
      equivalence
     $(MML(37),MMWSM),(MML(38),MMIFS),(MML(39),MMILS),(MML(40),MMDRO),
     $(MML(41),MMNED),(MML(42),MMXC ),(MML(43),MMXP ),(MML(44),MMGMA),
     $(MML(45),MMGTO),(MML(46),MMST ),(MML(47),MMBTR),(MML(48),MMFXI),
     $(MML(49),MMSB1),(MML(50),MMSB2),(MML(51),MMTAM),(MML(52),MMGMI),
     $(MML(53),MMPGL),(MML(54),MMPGD),(MML(55),MMDWN),(MML(56),MMIXB),
     $(MML(57),MMIXR),(MML(58),MMIXS),(MML(59),MMDL ),(MML(60),MMSTI),
     $(MML(61),MMSTE),(MML(62),MMSTK),(MML(63),MMXR ),(MML(64),MMLRI),
     $(MML(65),MMAW ),(MML(66),MMQHI),(MML(67),MMSN )
      equivalence
     $(MMP( 1),MMYAM),(MMP( 2),MMSNU),(MMP( 3),MMSCN),(MMP( 4),MMKCN),
     $(MMP( 5),MMJNU),(MMP( 6),MMSLF),(MMP( 7),MMSLR)
      equivalence
     $(MMT( 1),MMZAM),(MMT( 2),MMCIZ),(MMT( 3),MMCIA),(MMT( 4),MMCFZ),
     $(MMT( 5),MMCFA),(MMT( 6),MMBTI),(MMT( 7),MMBTF),(MMT( 8),MMCZA),
     $(MMT( 9),MMCAA),(MMT(10),MMBTA),(MMT(11),MMCZB),(MMT(12),MMCAB),
     $(MMT(13),MMFZB),(MMT(14),MMFAB),(MMT(15),MMTIB),(MMT(16),MMTFB),
     $(MMT(17),MMZAB),(MMT(18),MMAAB),(MMT(19),MMTAB)
C     .
C
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
C     .
      equivalence
     $(LPD( 1),LPNAM),(LPD( 2),LPPHI),(LPD( 3),LPBC ),(LPD( 4),LPJNU),
     $(LPD( 5),LPT1 ),(LPD( 6),LPSIG),(LPD( 7),LPWN ),(LPD( 8),LPKPC),
     $(LPD( 9),LPPHW),(LPD(10),LPALL),(LPD(11),LPXLL),(LPD(12),LPWH ),
     $(LPD(13),LPTNU),(LPD(14),LPT2 ),(LPD(15),LPT3 )
C     .
C
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
C     .
      equivalence
     $(LOD( 1),LONAM ),(LOD( 2),LOPHIW),(LOD( 3),LOKPC ),
     $(LOD( 4),LOPHI ),(LOD( 5),LOCWT ),(LOD( 6),LOWN  ),
     $(LOD( 7),LOBC  ),(LOD( 8),LOTNU ),(LOD( 9),LOJNU ),
     $(LOD(10),LOT1  ),(LOD(11),LOSIG ),(LOD(12),LOALL ),
     $(LOD(13),LOXLL ),(LOD(14),LOWH  ),(LOD(15),LOWWT ),
     $(LOD(16),LOT2  ),(LOD(17),LOT3  )
C     .
C
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
C     .
      equivalence
     $(ILB( 1),ILYWN),(ILB( 2),ILYEN),(ILB( 3),ILYDN),(ILB( 4),ILYAN),
     $(ILB( 5),ILYPN),(ILB( 6),ILYAA),(ILB( 7),ILYXX),(ILB( 8),ILYSA),
     $(ILB( 9),ILYSF),(ILB(10),ILYST),(ILB(11),ILYTN),(ILB(12),ILYP ),
     $(ILB(13),ILYPL),(ILB(14),ILYUL),(ILB(15),ILYVL),(ILB(16),ILYXL),
     $(ILB(17),ILYVC),(ILB(18),ILYEU)
C     .
C     !EJECT
C
C---- P A R T  2: Various Parameter Collections
C
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
C     .
      equivalence
     $(JZQ( 1),N  ),(JZQ( 2),NL ),(JZQ( 3),M  ),(JZQ( 4),KF ),
     $(JZQ( 5),NT ),(JZQ( 6),NSW),(JZQ( 7),L  ),(JZQ( 8),NLB),
     $(JZQ( 9),NFB),(JZQ(10),KK ),(JZQ(11),KBX),(JZQ(12),KKX),
     $(JZQ(13),LZM),(JZQ(14),NZM),(JZQ(15),MRR),(JZQ(16),NFL),
     $(JZQ(17),NWV),(JZQ(18),MMR),(JZQ(19),LF ),(JZQ(20),NTE),
     $(JZQ(21),NDT),(JZQ(22),MHM),(JZQ(23),NWS),(JZQ(24),JM ),
     $(JZQ(25),KNW),(JZQ(26),LDU),(JZQ(27),LLY),(JZQ(28),MLR),
     $(JZQ(29),MRS),(JZQ(30),MRX),(JZQ(31),NFH),(JZQ(32),NCR),
     $(JZQ(33),MLS),(JZQ(34),LG ),(JZQ(35),INK),(JZQ(36),KS ),
     $(JZQ(37),KR ),(JZQ(38),KB ),(JZQ(39),MQT),(JZQ(40),NSL),
     $(JZQ(41),NDR),(JZQ(42),NVX),(JZQ(43),NDV),(JZQ(44),NCP),
     $(JZQ(45),NAB),(JZQ(46),KWC),(JZQ(47),NVF),(JZQ(48),NXF),
     $(JZQ(49),KM ),(JZQ(50),NKA),(JZQ(51),NCL),(JZQ(52),NLN),
     $(JZQ(53),NCQ),(JZQ(54),NVH),(JZQ(55),NCB),(JZQ(56),NZE),
     $(JZQ(57),KWA),(JZQ(58),NGM)
C     .
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
C     .
      equivalence
     $(LUNITS( 1),LUIN ),(LUNITS( 2),LUMO ),(LUNITS( 3),LUAT ),
     $(LUNITS( 4),LURR ),(LUNITS( 5),LURO ),(LUNITS( 6),LUEO ),
     $(LUNITS( 7),LUDO ),(LUNITS( 8),LURS ),(LUNITS( 9),LUCA ),
     $(LUNITS(10),LUCR ),(LUNITS(11),LUIX ),(LUNITS(12),LUNC ),
     $(LUNITS(13),LUGI ),(LUNITS(14),LUCS ),(LUNITS(15),LUHB ),
     $(LUNITS(16),LUSD ),(LUNITS(17),LUKU ),(LUNITS(18),LUJO ),
     $(LUNITS(19),LUMA ),(LUNITS(20),LURE ),(LUNITS(21),LUSF ),
     $(LUNITS(22),LUJI ),(LUNITS(23),LUWM ),(LUNITS(24),lu024),
     $(LUNITS(25),LUIS ),(LUNITS(26),LUCM ),(LUNITS(27),LUSM ),
     $(LUNITS(28),LUSO ),(LUNITS(29),LUMR ),(LUNITS(30),LUPR ),
     $(LUNITS(31),lu031),(LUNITS(32),lu032),(LUNITS(33),lu033),
     $(LUNITS(34),lu034),(LUNITS(35),lu035),(LUNITS(36),LUPD ),
     $(LUNITS(37),LUKA )
      equivalence
     $(LUNITS( 1),NI   ),(LUNITS( 2),MODEL),(LUNITS( 3),MATOM),
     $(LUNITS( 5),NO   ),(LUNITS( 6),LX   ),(LUNITS( 8),MO   ),
     $(LUNITS(11),MQ   ),(LUNITS(17),KURU ),(LUNITS(18),JAYTO),
     $(LUNITS(20),MREST),(LUNITS(26),KOMPO),(LUNITS(22),JAYTI)
C     .
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
C     .
      equivalence
     $(DLIT( 1),ZERO  ),(DLIT( 2),ONE   ),(DLIT( 3),TWO   ),
     $(DLIT( 4),THREE ),(DLIT( 5),FOUR  ),(DLIT( 6),FIVE  ),
     $(DLIT( 7),SIX   ),(DLIT( 8),SEVEN ),(DLIT( 9),EIGHT ),
     $(DLIT(10),XNINE ),(DLIT(11),TEN   ),(DLIT(12),HALF  ),
     $(DLIT(13),THIRD ),(DLIT(14),CWARTR),(DLIT(15),FIFTH ),
     $(DLIT(16),SIXTH ),(DLIT(17),TWTHRD),(DLIT(18),EIGHTH),
     $(DLIT(19),TENTH )
C     .
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     .
      equivalence
     $(SYMBS(37),DOLLAR),(SYMBS(38),NUMERO),(SYMBS(39),PLUS  ),
     $(SYMBS(40),MINUS ),(SYMBS(41),SLASH ),(SYMBS(42),PERIOD),
     $(SYMBS(43),BLANK ),(SYMBS(44),EQUAL ),(SYMBS(45),STAR  ),
     $(SYMBS(46),PERCNT),(SYMBS(47),LBRAK ),(SYMBS(48),RBRAK ),
     $(SYMBS(49),LPAREN),(SYMBS(50),RPAREN),(SYMBS(51),LARROW),
     $(SYMBS(52),RARROW)
C     .
      character DOLLAR*1, NUMERO*1, PLUS*1, MINUS*1, SLASH*1, PERIOD*1,
     $          BLANK*1, EQUAL*1, STAR*1, PERCNT*1, LBRAK*1, RBRAK*1,
     $          LPAREN*1, RPAREN*1, LARROW*1, RARROW*1
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C
C---- ALTIMA      as of 2004 Mar 15
      real*8      ZZLALT,ZZSALT
      common      /ALTIMA/ ZZLALT,ZZSALT
C     Extreme values of NK, ND and BD.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C
C---- ESPY        as of 2004 May 18
      logical     ESPION
      common      /ESPY/ ESPION
C     "Values range" constraint switch
C     .
C
C---- EXPLIM      as of 1986 Apr 14
      real*8      EXPLIM
      common      /EXPLIM/ EXPLIM
C     Maximum argument for G-floating exponential function
C     .
C
C---- ROMAN       as of 1984 Apr 24
      character   ROMAN*5
      dimension   ROMAN(21)
      common      /ROMAN/ ROMAN
C     Roman numerals.
C     .
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
C     .
      equivalence
     $(RZQ(  1),ADS  ),(RZQ(  2),WZ   ),(RZQ(  3),RWKSI),
     $(RZQ(  4),AMASS),(RZQ(  5),PART ),(RZQ(  6),ABD  ),
     $(RZQ(  7),PW   ),(RZQ(  8),Y    ),(RZQ(  9),XNUK ),
     $(RZQ( 10),CWR  ),(RZQ( 11),CHOP ),(RZQ( 12),EXLYM),
     $(RZQ( 13),TGLYM),(RZQ( 14),HSEC ),(RZQ( 15),YH   ),
     $(RZQ( 16),CGR  ),(RZQ( 17),DLU  ),(RZQ( 18),TX   ),
     $(RZQ( 19),YL   ),(RZQ( 20),YPRE ),(RZQ( 21),TSM  ),
     $(RZQ( 22),DRLIM),(RZQ( 23),R1N  ),(RZQ( 24),TBAR ),
     $(RZQ( 25),WPOP ),(RZQ( 26),XLMT ),(RZQ( 27),XLME ),
     $(RZQ( 28),XKDST),(RZQ( 29),TDUST),(RZQ( 30),YFLUX)
      equivalence
     $(RZQ( 31),HEL  ),(RZQ( 32),XLMZ ),(RZQ( 33),XLMF ),
     $(RZQ( 34),WSM  ),(RZQ( 35),XLMA ),(RZQ( 36),XLMB ),
     $(RZQ( 37),XLMR ),(RZQ( 38),TLIMG),(RZQ( 39),BLIMG),
     $(RZQ( 40),WEP  ),(RZQ( 41),OPF  ),(RZQ( 42),RFAC ),
     $(RZQ( 43),WMN  ),(RZQ( 44),TMS  ),(RZQ( 45),WBD  ),
     $(RZQ( 46),RCCFE),(RZQ( 47),PRTLM),(RZQ( 48),CURMI),
     $(RZQ( 49),CURMA),(RZQ( 50),WTD  ),(RZQ( 51),TLTR ),
     $(RZQ( 52),XCOMX),(RZQ( 53),DDT  ),(RZQ( 54),XINCH),
     $(RZQ( 55),WMX  ),(RZQ( 56),SMP  ),(RZQ( 57),EIDIF),
     $(RZQ( 58),CUTFE),(RZQ( 59),WZM  ),(RZQ( 60),RFMAS)
      equivalence
     $(RZQ( 61),FABD ),(RZQ( 62),EPCBR),(RZQ( 63),TSMLL),
     $(RZQ( 64),TLRGE),(RZQ( 65),CVXM ),(RZQ( 66),PRDCV),
     $(RZQ( 67),CVXF ),(RZQ( 68),WFB  ),(RZQ( 69),CSFCT),
     $(RZQ( 70),CLNH ),(RZQ( 71),HTAU ),(RZQ( 72),PZERO),
     $(RZQ( 73),TML  ),(RZQ( 74),DELTB),(RZQ( 75),CEQMX),
     $(RZQ( 76),rzq76),(RZQ( 77),SMATC),(RZQ( 78),ELLED),
     $(RZQ( 79),EMXED),(RZQ( 80),VMNFE),(RZQ( 81),XQMAX),
     $(RZQ( 82),DQMIN),(RZQ( 83),DQMAX),(RZQ( 84),XJFE ),
     $(RZQ( 85),VSMLL),(RZQ( 86),CPRSS),(RZQ( 87),WPRSS),
     $(RZQ( 88),FZLIM),(RZQ( 89),CLOGG),(RZQ( 90),DZMSS)
      equivalence
     $(RZQ( 91),REFLM),(RZQ( 92),SBFEQ),(RZQ( 93),SBDMX),
     $(RZQ( 94),SBDMN),(RZQ( 95),ADMAS),(RZQ( 96),CHLIM),
     $(RZQ( 97),YCOL ),(RZQ( 98),XLCOD),(RZQ( 99),CCHX ),
     $(RZQ(100),CHEFL),(RZQ(101),CQM  ),(RZQ(102),XCL  ),
     $(RZQ(103),TAUCL),(RZQ(104),CVXS ),(RZQ(105),XMCOA),
     $(RZQ(106),VOITC),(RZQ(107),ZNDW ),(RZQ(108),WNJNK),
     $(RZQ(109),WBDIR),(RZQ(110),FBVMX),(RZQ(111),ASMCR),
     $(RZQ(112),ZXMIN),(RZQ(113),CFH  ),(RZQ(114),CVSB ),
     $(RZQ(115),HEABL),(RZQ(116),RFHEA),(RZQ(117),TRFLI),
     $(RZQ(118),EPTAU),(RZQ(119),WNUK ),(RZQ(120),HSBM )
      equivalence
     $(RZQ(121),HSBMN),(RZQ(122),HSBMX),(RZQ(123),HSBFQ),
     $(RZQ(124),PMSK ),(RZQ(125),CEDMN),(RZQ(126),CEDMX),
     $(RZQ(127),CEFEQ),(RZQ(128),FZION),(RZQ(129),FROSC),
     $(RZQ(130),FSTKM),(RZQ(131),FRCDL),(RZQ(132),FMCDL),
     $(RZQ(133),CSDW ),(RZQ(134),CLVLS),(RZQ(135),WAVMN),
     $(RZQ(136),WAVMX),(RZQ(137),CVZ  ),(RZQ(138),CDZ  ),
     $(RZQ(139),COMU ),(RZQ(140),BMWAC),(RZQ(141),SRCO ),
     $(RZQ(142),ALOMI),(RZQ(143),ALOMA),(RZQ(144),HNAJL),
     $(RZQ(145),CORMN),(RZQ(146),CORMX),(RZQ(147),FMVLM),
     $(RZQ(148),RCOMN),(RZQ(149),CWJ  ),(RZQ(150),PNH  )
      equivalence
     $(RZQ(151),CLM  ),(RZQ(152),CFHE ),(RZQ(153),SN1CC),
     $(RZQ(154),XLMD3),(RZQ(155),WSN1D),(RZQ(156),AOWXP),
     $(RZQ(157),CTCO ),(RZQ(158),CTMX ),(RZQ(159),SHCOP),
     $(RZQ(160),ZRCO ),(RZQ(161),SHCOC),(RZQ(162),XLMXC),
     $(RZQ(163),XLMXP),(RZQ(164),XLMD2),(RZQ(165),XLMCR),
     $(RZQ(166),WRTMN),(RZQ(167),WRTMX),(RZQ(168),YRATS),
     $(RZQ(169),XLMH ),(RZQ(170),SCVA ),(RZQ(171),SCVS ),
     $(RZQ(172),SCTA ),(RZQ(173),SCTS ),(RZQ(174),SCVB ),
     $(RZQ(175),SCPS ),(RZQ(176),CN1S ),(RZQ(177),DELLM),
     $(RZQ(178),zq178),(RZQ(179),zq179),(RZQ(180),zq180)
      equivalence
     $(RZQ(181),zq181),(RZQ(182),zq182),(RZQ(183),zq183)
      equivalence
     $(KZQ(  1),NDW  ),(KZQ(  2),MS   ),(KZQ(  3),NS   ),
     $(KZQ(  4),ISUB ),(KZQ(  5),IDFSW),(KZQ(  6),IRLS1),
     $(KZQ(  7),JSTIN),(KZQ(  8),IOMX ),(KZQ(  9),IRLSN),
     $(KZQ( 10),JBD  ),(KZQ( 11),IXSTA),(KZQ( 12),JRHO ),
     $(KZQ( 13),ILI  ),(KZQ( 14),NIL  ),(KZQ( 15),MFONT),
     $(KZQ( 16),LNLIM),(KZQ( 17),ITOPE),(KZQ( 18),IPEX ),
     $(KZQ( 19),LYMIT),(KZQ( 20),IHSLT),(KZQ( 21),NGRL ),
     $(KZQ( 22),NGRR ),(KZQ( 23),INFSM),(KZQ( 24),INLSM),
     $(KZQ( 25),METEP),(KZQ( 26),KNFRM),(KZQ( 27),KURIN),
     $(KZQ( 28),KINMX),(KZQ( 29),KININ),(KZQ( 30),MDTR1)
      equivalence
     $(KZQ( 31),MDTR2),(KZQ( 32),KKPR ),(KZQ( 33),KOLEV),
     $(KZQ( 34),JNUNC),(KZQ( 35),JSTCN),(KZQ( 36),IZOPT),
     $(KZQ( 37),JZOPT),(KZQ( 38),IVOIT),(KZQ( 39),NVOIT),
     $(KZQ( 40),JBDNC),(KZQ( 41),IMUCD),(KZQ( 42),M304 ),
     $(KZQ( 43),LSTMP),(KZQ( 44),NCOSW),(KZQ( 45),NTAN ),
     $(KZQ( 46),JBFSW),(KZQ( 47),IHEDF),(KZQ( 48),LDINT),
     $(KZQ( 49),LDTYP),(KZQ( 50),KUDNT),(KZQ( 51),JH1  ),
     $(KZQ( 52),JH2  ),(KZQ( 53),ISRCD),(KZQ( 54),IDRCD),
     $(KZQ( 55),NHTSW),(KZQ( 56),IONST),(KZQ( 57),IPR01),
     $(KZQ( 58),IPR02),(KZQ( 59),IPR03),(KZQ( 60),IPR04)
      equivalence
     $(KZQ( 61),MSKIP),(KZQ( 62),IPRFA),(KZQ( 63),IRPUN),
     $(KZQ( 64),IDEX ),(KZQ( 65),KALOR),(KZQ( 66),MCON ),
     $(KZQ( 67),NIASM),(KZQ( 68),KOMNV),(KZQ( 69),KOMNP),
     $(KZQ( 70),KOMNT),(KZQ( 71),KODNT),(KZQ( 72),JHEAS),
     $(KZQ( 73),MAMAS),(KZQ( 74),ISNUD),(KZQ( 75),IDWIN),
     $(KZQ( 76),ITRFI),(KZQ( 77),NSPED),(KZQ( 78),IFXDS),
     $(KZQ( 79),NVDFE),(KZQ( 80),NNDFE),(KZQ( 81),NZDFE),
     $(KZQ( 82),IPZER),(KZQ( 83),IHEAB),(KZQ( 84),IHDMP),
     $(KZQ( 85),IRTIS),(KZQ( 86),KOOLS),(KZQ( 87),MH2N ),
     $(KZQ( 88),ISCRS),(KZQ( 89),LYODS),(KZQ( 90),KDIAG)
      equivalence
     $(KZQ( 91),N1MET),(KZQ( 92),ISMBD),(KZQ( 93),IRUNT),
     $(KZQ( 94),NOION),(KZQ( 95),NERM ),(KZQ( 96),KARB ),
     $(KZQ( 97),ISOD ),(KZQ( 98),IPRDD),(KZQ( 99),IPRDF),
     $(KZQ(100),IPPOD),(KZQ(101),MN1  ),(KZQ(102),IDRDP),
     $(KZQ(103),KDRDP),(KZQ(104),KAPDB),(KZQ(105),MTHEI),
     $(KZQ(106),MDFV ),(KZQ(107),NCOPT),(KZQ(108),LSFGC),
     $(KZQ(109),NODCG),(KZQ(110),MNG1 ),(KZQ(111),IDFDM),
     $(KZQ(112),IDFDI),(KZQ(113),ISNDD),(KZQ(114),IDEDP),
     $(KZQ(115),IBRDP),(KZQ(116),ICXDP),(KZQ(117),NSPRD),
     $(KZQ(118),ICHDP),(KZQ(119),ISMSW),(KZQ(120),ICDIT)
      equivalence
     $(KZQ(121),IRATE),(KZQ(122),ICHSW),(KZQ(123),IHSSW),
     $(KZQ(124),IHSDP),(KZQ(125),IHSDD),(KZQ(126),IHSKM),
     $(KZQ(127),IHSSM),(KZQ(128),NDWM ),(KZQ(129),LHHSE),
     $(KZQ(130),LX2DS),(KZQ(131),LX3DS),(KZQ(132),KMMAX),
     $(KZQ(133),ISTRK),(KZQ(134),NBS  ),(KZQ(135),NANA1),
     $(KZQ(136),IHSSP),(KZQ(137),NANA2),(KZQ(138),LOGAS),
     $(KZQ(139),NECLP),(KZQ(140),NARB ),(KZQ(141),KAVNT),
     $(KZQ(142),KAVNP),(KZQ(143),KAVNZ),(KZQ(144),IXNCS),
     $(KZQ(145),IRFNC),(KZQ(146),JDMCI),(KZQ(147),JDMCE),
     $(KZQ(148),IDNRT),(KZQ(149),JHBFD),(KZQ(150),MOPRN)
      equivalence
     $(KZQ(151),IWSMD),(KZQ(152),LWNT ),(KZQ(153),KDFD1),
     $(KZQ(154),KDFGS),(KZQ(155),KDFGA),(KZQ(156),KDFGB),
     $(KZQ(157),ITN1R),(KZQ(158),LODCG),(KZQ(159),I4DFM),
     $(KZQ(160),I4DEQ),(KZQ(161),I4DIO),(KZQ(162),MSSPR),
     $(KZQ(163),NEFDF),(KZQ(164),IPDIJ),(KZQ(165),IPDEE),
     $(KZQ(166),IBTSW),(KZQ(167),KB1WA),(KZQ(168),KB1WB),
     $(KZQ(169),KDAMP),(KZQ(170),KBNDS),(KZQ(171),N1NUP),
     $(KZQ(172),MDFG ),(KZQ(173),INDRN),(KZQ(174),MKURU),
     $(KZQ(175),KHFFS),(KZQ(176),MTREF),(KZQ(177),KATNU),
     $(KZQ(178),ISCMP),(KZQ(179),KB1WS),(KZQ(180),KOELS)
      equivalence
     $(KZQ(181),JATOM),(KZQ(182),JATMO),(KZQ(183),IGMSW),
     $(KZQ(184),NLY  ),(KZQ(185),KCOAA),(KZQ(186),IWEIT),
     $(KZQ(187),IFALL),(KZQ(188),NQLYM),(KZQ(189),JXNCS),
     $(KZQ(190),JHLSK),(KZQ(191),NMLR ),(KZQ(192),JSSV ),
     $(KZQ(193),JNEDP),(KZQ(194),JEDIT),(KZQ(195),MBREC),
     $(KZQ(196),JATAW),(KZQ(197),IBNVW),(KZQ(198),NDSN1),
     $(KZQ(199),IXASM),(KZQ(200),MXPPI),(KZQ(201),MXTAP),
     $(KZQ(202),ITKZA),(KZQ(203),IDFDS),(KZQ(204),LDFD1),
     $(KZQ(205),NGNV ),(KZQ(206),IPIJG),(KZQ(207),ISMVE),
     $(KZQ(208),JSFEX),(KZQ(209),LOXDS),(KZQ(210),IORIC)
      equivalence
     $(KZQ(211),LHEDS),(KZQ(212),kz212),(KZQ(213),IGII ),
     $(KZQ(214),ITPRD),(KZQ(215),LEEDS),(KZQ(216),KXLYM),
     $(KZQ(217),LPVEL),(KZQ(218),LPMLR),(KZQ(219),KLDIN),
     $(KZQ(220),KLFIN),(KZQ(221),MSEDG),(KZQ(222),MSEDW),
     $(KZQ(223),MCEOF),(KZQ(224),MCIOF),(KZQ(225),INCEI),
     $(KZQ(226),ICIH1)
      equivalence
     $(QZQ(  1),QNAME),(QZQ(  2),QELSM),(QZQ(  3),QMODL),
     $(QZQ(  4),QATOM),(QZQ(  5),QALHD)
C     .
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
C     .
      equivalence
     $(PCON( 1),PLANCK),(PCON( 2),BOLZMN),(PCON( 3),CLIGHT),
     $(PCON( 4),RYDBRG),(PCON( 5),HEMASS),(PCON( 6),ELMASS),
     $(PCON( 7),FREQEV),(PCON( 8),HYMASS),(PCON( 9),SOLSGR),
     $(PCON(10),SOLRAD),(PCON(11),AUNIT ),(PCON(12),ELCHRG),
     $(PCON(13),ERGPEV),(PCON(14),EVPK  ),(PCON(15),STFBLZ),
     $(PCON(16),BOHRAD),(PCON(17),HYLYK ),(PCON(18),SOLDIA)
      equivalence
     $(TUNI( 1),PI    ),(TUNI( 2),ROOTPI),(TUNI( 3),FRQUNT),
     $(TUNI( 4),XMBARN),(TUNI( 5),CMPKM ),(TUNI( 6),ANGPCM),
     $(TUNI( 7),ROOT2 ),(TUNI( 8),ROOT3 ),(TUNI( 9),XLOGE ),
     $(TUNI(10),DGPRAD),(TUNI(11),DEBYE )
C     .
C
C---- SELGI       as of 1999 Sep 13
      integer     KNFMX
      parameter   (KNFMX=50)
C     (Remember to recompile all users when changing KNFMX)
      integer     KNTF,INF1,INF2,INF3,INF4
      real*8      FUJJ,FVAL
      dimension   INF1(KNFMX),INF2(KNFMX),INF3(KNFMX),INF4(KNFMX),
     $            FUJJ(KNFMX),FVAL(KNFMX)
      common      /SELGI1/ KNTF,INF1,INF2,INF3,INF4
      common      /SELGI2/ FUJJ,FVAL
C     Saves B-ratios computation fudging data, for later printing.
C     .
C
C---- OPTIONS     as of Jul 14 2014
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=346)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
C     .
      equivalence
     $(IQQ(  1),IQANA),(IQQ(  2),IQSCL),(IQQ(  3),IQSEB),
     $(IQQ(  4),IQLTD),(IQQ(  5),IQPH2),(IQQ(  6),IQECL),
     $(IQQ(  7),IQEPC),(IQQ(  8),IQSFO),(IQQ(  9),IQLGT),
     $(IQQ( 10),IQSTD),(IQQ( 11),IQCPA),(IQQ( 12),IQOPO),
     $(IQQ( 13),IQLYM),(IQQ( 14),IQCSW),(IQQ( 15),IQARD),
     $(IQQ( 16),IQHSE),(IQQ( 17),IQSET),(IQQ( 18),IQSUM),
     $(IQQ( 19),IQEVR),(IQQ( 20),IQKUP),(IQQ( 21),IQSTO),
     $(IQQ( 22),IQFLD),(IQQ( 23),IQSTI),(IQQ( 24),IQOST),
     $(IQQ( 25),IQDPW),(IQQ( 26),IQTAF),(IQQ( 27),IQCAP),
     $(IQQ( 28),IQFBO),(IQQ( 29),IQNUM),(IQQ( 30),IQDDP),
     $(IQQ( 31),IQSFS),(IQQ( 32),IQVTV),(IQQ( 33),IQLTE),
     $(IQQ( 34),IQNPL),(IQQ( 35),IQNHA),(IQQ( 36),IQHNP),
     $(IQQ( 37),IQAPC),(IQQ( 38),IQENH),(IQQ( 39),IQCSD),
     $(IQQ( 40),IQESD),(IQQ( 41),IQIRC),(IQQ( 42),IQPPR),
     $(IQQ( 43),IQTNO),(IQQ( 44),IQIGR),(IQQ( 45),IQCSP),
     $(IQQ( 46),IQCSF),(IQQ( 47),IQCGR),(IQQ( 48),IQHEP),
     $(IQQ( 49),IQFIN),(IQQ( 50),IQREF),(IQQ( 51),IQINC),
     $(IQQ( 52),IQCDP),(IQQ( 53),IQUTR),(IQQ( 54),IQMNT),
     $(IQQ( 55),IQEMI),(IQQ( 56),IQNES),(IQQ( 57),IQLID)
      equivalence
     $(IQQ( 58),IQEID),(IQQ( 59),IQABS),(IQQ( 60),IQGDS),
     $(IQQ( 61),IQIRL),(IQQ( 62),IQRSQ),(IQQ( 63),IQOPG),
     $(IQQ( 64),IQDNT),(IQQ( 65),IQCPL),(IQQ( 66),IQEMO),
     $(IQQ( 67),IQELO),(IQQ( 68),IQHMS),(IQQ( 69),IQHMP),
     $(IQQ( 70),IQNCP),(IQQ( 71),IQENL),(IQQ( 72),IQEN2),
     $(IQQ( 73),IQPRP),(IQQ( 74),IQSIP),(IQQ( 75),IQSEP),
     $(IQQ( 76),IQSTA),(IQQ( 77),IQUWT),(IQQ( 78),IQIBE),
     $(IQQ( 79),IQPTN),(IQQ( 80),IQCFX),(IQQ( 81),IQFLP),
     $(IQQ( 82),IQPWT),(IQQ( 83),IQSLY),(IQQ( 84),IQCOT),
     $(IQQ( 85),IQPAI),(IQQ( 86),IQGDP),(IQQ( 87),IQNDP),
     $(IQQ( 88),IQIJR),(IQQ( 89),IQLSC),(IQQ( 90),IQH2P),
     $(IQQ( 91),IQALP),(IQQ( 92),IQEMG),(IQQ( 93),IQCSG),
     $(IQQ( 94),IQALL),(IQQ( 95),IQRGR),(IQQ( 96),IQSPC),
     $(IQQ( 97),IQCPU),(IQQ( 98),IQMGP),(IQQ( 99),IQDT2),
     $(IQQ(100),IQDTP),(IQQ(101),IQORI),(IQQ(102),IQDEF),
     $(IQQ(103),IQSEC),(IQQ(104),IQEMS),(IQQ(105),IQTAS),
     $(IQQ(106),IQBRF),(IQQ(107),IQALY),(IQQ(108),IQRID),
     $(IQQ(109),IQISS),(IQQ(110),IQIRH),(IQQ(111),IQITA),
     $(IQQ(112),IQIRK),(IQQ(113),IQINN),(IQQ(114),IQIRW)
      equivalence
     $(IQQ(115),IQLYC),(IQQ(116),IQLYD),(IQQ(117),IQSPU),
     $(IQQ(118),IQSNB),(IQQ(119),IQGDM),(IQQ(120),IQROD),
     $(IQQ(121),IQEDP),(IQQ(122),IQEPS),(IQQ(123),IQWSR),
     $(IQQ(124),IQWSE),(IQQ(125),IQSUP),(IQQ(126),IQCRK),
     $(IQQ(127),IQINE),(IQQ(128),IQIBD),(IQQ(129),IQLND),
     $(IQQ(130),IQPBD),(IQQ(131),IQFEP),(IQQ(132),IQHSD),
     $(IQQ(133),IQCCR),(IQQ(134),IQSCD),(IQQ(135),IQORT),
     $(IQQ(136),IQIZZ),(IQQ(137),IQPD1),(IQQ(138),IQPD2),
     $(IQQ(139),IQFCD),(IQQ(140),IQCCI),(IQQ(141),IQIFF),
     $(IQQ(142),IQIDP),(IQQ(143),IQCFD),(IQQ(144),IQIVK),
     $(IQQ(145),IQESL),(IQQ(146),IQWDD),(IQQ(147),IQRSM),
     $(IQQ(148),IQSSP),(IQQ(149),IQSED),(IQQ(150),IQRED),
     $(IQQ(151),IQPPU),(IQQ(152),IQJNT),(IQQ(153),IQBLN),
     $(IQQ(154),IQNCJ),(IQQ(155),IQPRE),(IQQ(156),IQTOP),
     $(IQQ(157),IQPSG),(IQQ(158),IQPBS),(IQQ(159),IQPD0),
     $(IQQ(160),IQEPU),(IQQ(161),IQBED),(IQQ(162),IQEGR),
     $(IQQ(163),IQTNG),(IQQ(164),IQPPF),(IQQ(165),IQUVP),
     $(IQQ(166),IQCSB),(IQQ(167),IQPPJ),(IQQ(168),IQQSE),
     $(IQQ(169),IQEXA),(IQQ(170),IQKOP),(IQQ(171),IQCPC)
      equivalence
     $(IQQ(172),IQSHF),(IQQ(173),IQVSW),(IQQ(174),IQIXD),
     $(IQQ(175),IQPED),(IQQ(176),IQEDD),(IQQ(177),IQFEL),
     $(IQQ(178),IQCFE),(IQQ(179),IQPDC),(IQQ(180),IQRTA),
     $(IQQ(181),IQKSP),(IQQ(182),IQHMJ),(IQQ(183),IQHMO),
     $(IQQ(184),IQCPS),(IQQ(185),IQPSW),(IQQ(186),IQESW),
     $(IQQ(187),IQMSP),(IQQ(188),IQLFD),(IQQ(189),IQRIJ),
     $(IQQ(190),IQOSH),(IQQ(191),IQFXV),(IQQ(192),IQAPF),
     $(IQQ(193),IQLNB),(IQQ(194),IQKLC),(IQQ(195),IQKLD),
     $(IQQ(196),IQXRC),(IQQ(197),IQXRI),(IQQ(198),IQKRD),
     $(IQQ(199),IQROP),(IQQ(200),IQPFC),(IQQ(201),IQINH),
     $(IQQ(202),IQFDD),(IQQ(203),IQSOD),(IQQ(204),IQRWO),
     $(IQQ(205),IQINI),(IQQ(206),IQPD3),(IQQ(207),IQISF),
     $(IQQ(208),IQEBI),(IQQ(209),IQSOP),(IQQ(210),IQCLP),
     $(IQQ(211),IQD2D),(IQQ(212),IQND2),(IQQ(213),IQITD),
     $(IQQ(214),IQVES),(IQQ(215),IQJLY),(IQQ(216),IQWSP),
     $(IQQ(217),IQMIX),(IQQ(218),IQSOI),(IQQ(219),IQAMD),
     $(IQQ(220),IQADP),(IQQ(221),IQVLG),(IQQ(222),IQVDP),
     $(IQQ(223),IQCOD),(IQQ(224),IQCCP),(IQQ(225),IQCOC),
     $(IQQ(226),IQCRD),(IQQ(227),IQSTM),(IQQ(228),IQOPP)
      equivalence
     $(IQQ(229),IQPGA),(IQQ(230),IQHNM),(IQQ(231),IQCHR),
     $(IQQ(232),IQISO),(IQQ(233),IQDIR),(IQQ(234),IQDFA),
     $(IQQ(235),IQNVT),(IQQ(236),IQPPS),(IQQ(237),IQBNC),
     $(IQQ(238),IQO2P),(IQQ(239),IQVLP),(IQQ(240),IQJIN),
     $(IQQ(241),IQRCO),(IQQ(242),IQPDV),(IQQ(243),IQSCP),
     $(IQQ(244),IQGNV),(IQQ(245),IQBDC),(IQQ(246),IQSRJ),
     $(IQQ(247),IQAMP),(IQQ(248),IQATP),(IQQ(249),IQINP),
     $(IQQ(250),IQLSG),(IQQ(251),IQCGP),(IQQ(252),IQBDG),
     $(IQQ(253),IQTEG),(IQQ(254),IQACP),(IQQ(255),IQLYA),
     $(IQQ(256),IQANU),(IQQ(257),IQAHS),(IQQ(258),IQAOP),
     $(IQQ(259),IQASP),(IQQ(260),IQADA),(IQQ(261),IQAPP),
     $(IQQ(262),IQOPE),(IQQ(263),IQSDI),(IQQ(264),IQAMB),
     $(IQQ(265),IQVLS),(IQQ(266),IQDSM),(IQQ(267),IQA1P),
     $(IQQ(268),IQA1D),(IQQ(269),IQSSD),(IQQ(270),IQHSV),
     $(IQQ(271),IQBSM),(IQQ(272),IQAN1),(IQQ(273),IQHBR),
     $(IQQ(274),IQHST),(IQQ(275),IQGTS),(IQQ(276),IQCXP),
     $(IQQ(277),IQCXD),(IQQ(278),IQCIJ),(IQQ(279),IQPIJ),
     $(IQQ(280),IQTPL),(IQQ(281),IQHEA),(IQQ(282),IQCHP),
     $(IQQ(283),IQSMG),(IQQ(284),IQSMT),(IQQ(285),IQIWA)
      equivalence
     $(IQQ(286),IQAIW),(IQQ(287),IQLSP),(IQQ(288),IQDIC),
     $(IQQ(289),IQDIL),(IQQ(290),IQWNM),(IQQ(291),IQTRP),
     $(IQQ(292),IQPZT),(IQQ(293),IQDAS),(IQQ(294),IQSLV),
     $(IQQ(295),IQSLP),(IQQ(296),IQSLG),(IQQ(297),IQIXT),
     $(IQQ(298),IQIXW),(IQQ(299),IQTRC),(IQQ(300),IQTCP),
     $(IQQ(301),IQBQD),(IQQ(302),IQICK),(IQQ(303),IQAVK),
     $(IQQ(304),IQSTW),(IQQ(305),IQCOE),(IQQ(306),IQEND),
     $(IQQ(307),IQALO),(IQQ(308),IQALD),(IQQ(309),IQRGS),
     $(IQQ(310),IQSSM),(IQQ(311),IQBRD),(IQQ(312),IQIRE),
     $(IQQ(313),IQLCP),(IQQ(314),IQLCD),(IQQ(315),IQROS),
     $(IQQ(316),IQDLA),(IQQ(317),IQMXC),(IQQ(318),IQUET),
     $(IQQ(319),IQNRS),(IQQ(320),IQCOA),(IQQ(321),IQUTM),
     $(IQQ(322),IQSBP),(IQQ(323),IQQHR),(IQQ(324),IQINB),
     $(IQQ(325),IQBNV),(IQQ(326),IQZCP),(IQQ(327),IQOXP),
     $(IQQ(328),IQCEF),(IQQ(329),IQPCP),(IQQ(330),IQCXL),
     $(IQQ(331),IQCXU),(IQQ(332),IQGSM),(IQQ(333),IQLXL),
     $(IQQ(334),IQRKE),(IQQ(335),IQFLW),(IQQ(336),IQFBP),
     $(IQQ(337),IQPMH),(IQQ(338),IQPIP),(IQQ(339),IQLNC),
     $(IQQ(340),IQLNU),(IQQ(341),IQLBD),(IQQ(342),IQPFU)
      equivalence
     $(IQQ(343),IQAED),(IQQ(344),IQOTL),(IQQ(345),IQO3P),
     $(IQQ(346),IQACC)
C     .
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
C     .
      equivalence
     $(REST( 1),WTPZ ),(REST( 2),R1GD ),(REST( 3),YALYM),
     $(REST( 4),XLCOW),(REST( 5),RFXNC),(REST( 6),YASYM),
     $(REST( 7),YAFUL)
      equivalence
     $(LEST( 1),KSHEL),(LEST( 2),IOVER),(LEST( 3),ITER ),
     $(LEST( 4),NSHL ),(LEST( 5),MHL  ),(LEST( 6),KPRSW),
     $(LEST( 7),NPT  ),(LEST( 8),NRPMX),(LEST( 9),NH2CS),
     $(LEST(10),KZERO),(LEST(11),KTKIN),(LEST(12),LGGIN),
     $(LEST(13),J304I),(LEST(14),NTK  ),(LEST(15),LFLX ),
     $(LEST(16),MSFQR),(LEST(17),MOMET),(LEST(18),MSFQM),
     $(LEST(19),ITHSL),(LEST(20),MSFRT),(LEST(21),MSFGR),
     $(LEST(22),JPOP ),(LEST(23),J304S),(LEST(24),LITER),
     $(LEST(25),NOTX ),(LEST(26),KNZGM),(LEST(27),NOTA ),
     $(LEST(28),NXRW ),(LEST(29),NONC ),(LEST(30),KMASN),
     $(LEST(31),KMUS ),(LEST(32),KAMB ),(LEST(33),LDLMX),
     $(LEST(34),NEWZ ),(LEST(35),NLFDB),(LEST(36),NVSB ),
     $(LEST(37),KVSB ),(LEST(38),KTRAS),(LEST(39),KDGV ),
     $(LEST(40),NCOW ),(LEST(41),KIMS ),(LEST(42),IVNH ),
     $(LEST(43),MLSFP),(LEST(44),LCOW ),(LEST(45),NGDZ ),
     $(LEST(46),KGDT ),(LEST(47),KVLG ),(LEST(48),KCRH ),
     $(LEST(49),KDFA ),(LEST(50),NVSBP),(LEST(51),JIBR ),
     $(LEST(52),JDDL ),(LEST(53),LDLMU),(LEST(54),MFMV )
      equivalence
     $(LEST(55),MCXK ),(LEST(56),KNEGA),(LEST(57),KX2OK),
     $(LEST(58),KX3OK),(LEST(59),JHEAB),(LEST(60),KBTMX),
     $(LEST(61),KRTMX),(LEST(62),KSTMX),(LEST(63),MPROM),
     $(LEST(64),IDGMZ),(LEST(65),KTECH),(LEST(66),KVXVA),
     $(LEST(67),MWNSV),(LEST(68),KZXST),(LEST(69),KFCEU),
     $(LEST(70),KFELE),(LEST(71),KCHIJ),(LEST(72),KOXOK),
     $(LEST(73),KHEOK),(LEST(74),KALTG),(LEST(75),les75),
     $(LEST(76),KDZIN),(LEST(77),KEEOK),(LEST(78),IGKSW),
     $(LEST(79),NCINM),(LEST(80),KCHKI),(LEST(81),KLYNF),
     $(LEST(82),KXNUC)
      equivalence
     $(QEST( 1),QIONM)
C     .
C
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
C     .
      equivalence
     $(MEST( 1),JDCES),(MEST( 2),JDCEG),(MEST( 3),JDCEI),
     $(MEST( 4),JDCEV),(MEST( 5),JDCEJ),(MEST( 6),JDCIV),
     $(MEST( 7),JDCIJ),(MEST( 8),JDCRD),(MEST( 9),JDAIJ),
     $(MEST(10),JDCVW),(MEST(11),JDCSK),(MEST(12),JDCRS),
     $(MEST(13),JDCPI),(MEST(14),JDRCP),(MEST(15),JDCER),
     $(MEST(16),JDXNU),(MEST(17),JDNUK),(MEST(18),JDPSW),
     $(MEST(19),JDCEA),(MEST(20),JDCIC),(MEST(21),JDKNT),
     $(MEST(22),JDCIA),(MEST(23),JDCP1),(MEST(24),JDRP1),
     $(MEST(25),JDCIS),(MEST(26),JDCIW),(MEST(27),JDCPN),
     $(MEST(28),JDCEW)
C     .
C
C---- MATRIX      as of 2006 Sep 06
      integer     PRNSW,EDJSW,KNTIN,KNTED
      real*8      CRITJ,TIMIN,TIMED
      common      /MATRIX1/ PRNSW,EDJSW,KNTIN,KNTED
      common      /MATRIX2/ CRITJ,TIMIN,TIMED
C
C     Control parameters for matrix inversion and determinants.
C
C     PRNSW = 1: print matrix messages; = 0: do not.
C     EDJSW = 1: edit out "junk" using CRITJ; = 0: do not.
C     KNTIN      count of calls to INVERS.
C     KNTED      count of calls to DETERM.
C     CRITJ      "junk" criterion for EDJSW.
C     TIMIN      total time for all matrix inversions.
C     TIMED      total time for all determinants.
C     .
C
C---- MATMAG      as of 1989 Jan 25
      integer     MSTMAX
      parameter   (MSTMAX=10)
C     (Remember to recompile all users when changing MSTMAX)
      integer     MATKNT,NMAT,MATSNO
      real*8      ELLRGE,ELSMLL,ELRNGE
      character   MATNAM*50
      dimension   ELLRGE(MSTMAX),ELSMLL(MSTMAX),NMAT(MSTMAX),
     $            ELRNGE(MSTMAX),MATNAM(MSTMAX)
      common      /MATMAG1/ MATNAM
      common      /MATMAG2/ MATKNT,NMAT
      common      /MATMAG3/ MATSNO
      common      /MATMAG4/ ELLRGE,ELSMLL,ELRNGE
C     Elements range characteristics of the MSTMAX most extreme
C     matrices, (collected when MAMAS=1).
C     .
C
C---- CHECKS      as of 1989 Jan 25
      integer     NCKSUM,NCKSM
      real*8      CSUM
      character   TSUM*40
      parameter   (NCKSUM=1000)
      dimension   CSUM(NCKSUM), TSUM(NCKSUM)
      common      /CKSUM1/ NCKSM
      common      /CKSUM2/ CSUM
      common      /CKSUM3/ TSUM
C     Strategic array checksums, for debugging.
C     .
C
C---- IBIS        as of 2003 May 30
      integer     MXIBIS,NTMX,IBADR,IBKOD,IBNIT,IBLEN,IBIN1,IBIN2,
     $            IBITE,IBLIT,IBITH,IBIOV,NITS,NIBIS
      character   IBSRC*10, IBNAM*10
      parameter   (MXIBIS=1000)
      parameter   (NTMX=14)
C     (Remember to recompile all users when changing MXIBIS or NTMX)
      dimension   IBADR(MXIBIS), IBKOD(MXIBIS), IBNIT(MXIBIS),
     $            IBLEN(MXIBIS), IBIN1(MXIBIS), IBIN2(MXIBIS),
     $            IBITE(MXIBIS), IBLIT(MXIBIS), IBITH(MXIBIS),
     $            IBIOV(MXIBIS), IBSRC(MXIBIS), IBNAM(MXIBIS),
     $            NITS(NTMX)
      common      /IBIS1/ NIBIS, IBADR, IBKOD, IBNIT, IBLEN, IBIN1,
     $                           IBIN2, IBITE, IBLIT, IBITH, IBIOV
      common      /IBIS2/        IBSRC, IBNAM
      common      /IBIS3/ NITS
C     Control information for iterative summary data.
C
C         NITS = counts of iteration summary data records for:
C
C      1: TAU(IU,IL),    2: CHECK(L)       3: RHO(IU,IL)
C      4: RK(KOLEV)      5: ND(L)          6: RHOWT(IU,IL)
C      7: BD(KOLEV)      8: NE             9: CHI(IU,IL)
C     10: Z             11: S(IU,IL)      12: NH
C     13: TDST          14: NK
C     .
      equivalence
     $(NITS( 1),NTAITR),(NITS( 2),NCKITR),(NITS( 3),NRHITR),
     $(NITS( 4),NRKITR),(NITS( 5),NNDITR),(NITS( 6),NRWITR),
     $(NITS( 7),NBKITR),(NITS( 8),NNEITR),(NITS( 9),NQHITR),
     $(NITS(10),NZZITR),(NITS(11),NSSITR),(NITS(12),NNHITR),
     $(NITS(13),NTDITR),(NITS(14),NNKITR)
C     .
C
C---- DICOM       as of 2000 Mar 01
      integer     NAPWRA,NAPWRB,NAPKNT
      real*8      APARAD,APETA,APCDP,APWRA,APWRB,APCI,APEI
      dimension   APCI(20), APEI(20)
      common      /DICOM1/ NAPWRA,NAPWRB,NAPKNT
      common      /DICOM2/ APARAD,APETA,APCDP,APWRA,APWRB,APCI,APEI
C     Data for recombinations.
C     .
C
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C
C---- BAMBI       as of 1998 Apr 22
      integer     IPDPAR
      real*8      APDPAR
      dimension   APDPAR(10)
      common      /BAMBI1/ IPDPAR
      common      /BAMBI2/ APDPAR
C     Parameters for "original" d coefficients
C     .
      equivalence
     $(APDPAR( 1),TEXP  ),(APDPAR( 2),XICA  ),(APDPAR( 3),XICB  ),
     $(APDPAR( 4),XICC  ),(APDPAR( 5),DIFAC ),(APDPAR( 6),DTFAC ),
     $(APDPAR( 7),XICD  ),(APDPAR( 8),CD22  ),(APDPAR( 9),CD33  ),
     $(APDPAR(10),CD44  )
C     .
C
C---- ERODIUM     as of 2007 Apr 13
      integer     IPDFON
      real*8      APDFON,APDPPR
      dimension   APDFON(9), APDPPR(6)
      common      /ERODIUM1/ IPDFON
      common      /ERODIUM2/ APDFON,APDPPR
C     Parameters for "improved" d coefficients.
C     .
C     !EJECT
C
C---- P A R T  3: Miscellaneous Data
C
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C
C---- TABLET      as of 2007 Feb 21
      integer     KONLIM,KONWAL
      parameter   (KONLIM=20000)
      real*8      CONTIT,CONWAV
      integer     NUMKON,NUMTRU,NMKUSE,KONADR,KONTYP,KONLIC,KONNSH
      dimension   CONTIT(KONLIM),CONWAV(KONLIM),KONADR(KONLIM),
     $            KONTYP(KONLIM),KONLIC(KONLIM),KONNSH(KONLIM)
      common      /TABLET0/ KONWAL,NUMKON,NUMTRU,NMKUSE
      common      /TABLET1/ CONWAV
      common      /TABLET2/ CONTIT
      common      /TABLET3/ KONADR
      common      /TABLET4/ KONTYP
      common      /TABLET5/ KONLIC
      common      /TABLET6/ KONNSH
C
C     Index, and other data, for Continuum Data Blocks.
C
C     KONWAL - (= KONLIM)
C     NUMKON - total number of Blocks
C     NUMTRU - number of line-specific Blocks ( .le. NUMKON)
C     NMKUSE - number of Blocks to be used for SIAM scratch storage
C
C     CONTIT - Block name (also called "Header Code" or XLTIT,SLTIT)
C     CONWAV - wavelength (Angstroms)
C     KONADR - file address of Block
C     KONTYP - Block code (labelled-common "kwack" via subroutine BEECH)
C     KONNSH - number of supplementary headers (shared blocks only)
C     KONLIC - line transition descriptor, = 100*iu+il (if needed)
C     .
C
C---- CHICLE      as of 2005 Dec 21
      integer     LIMBG,NUMBG
      parameter   (LIMBG=10000)
      real*8      BWAV,BIHZ,BIAN
      dimension   BWAV(LIMBG),BIHZ(LIMBG),BIAN(LIMBG)
      common      /CHICLE1/ NUMBG
      common      /CHICLE2/ BWAV
      common      /CHICLE3/ BIHZ
      common      /CHICLE4/ BIAN
C     Computed background spectrum at "non-line" wavelength
C     .
C
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C
C---- ILION       as of 1987 Aug 20
      integer     NUMSCT
      common      /ILION/ NUMSCT
C     Next available identifier for printout sections.
C     .
C
C---- ARCHER      as of 2004 May 12
      integer     NNKOD, NNKODS
      parameter   (NNKOD=3)
C     (Be sure to recompile POD when changing NNKOD.)
      dimension   NNKODS(NNKOD)
      common      /ARCHER/ NNKODS
C     Diana/Orion Data Blocks control parameters.
C
C     (These parameters are packed and unpacked by "POD".)
C       LLXI - frequency index.
C       IRAY - angle or ray index (Orion only: expanding atmosphere);
C              (in the spherical case, Shell rays are enumerated first,
C              followed by Disk rays).
C       NRAY - number of depth levels intersected by this ray;
C              (differs from N only for Shell rays).
C     .
      equivalence
     $(NNKODS( 1),LLXI  ),(NNKODS( 2),IRAY  ),(NNKODS( 3),NRAY  )
C     .
C
C---- INDEX       as of 1990 Nov 26
      integer     MILTI,LIMIT,NULSIG,INC
      parameter   (MILTI=50)
C     (Remember to recompile ADAM and GRUB when changing MILTI.)
      dimension   INC(MILTI,MILTI)
      common      /INDEX/ LIMIT,NULSIG,INC
C     Data and controls for the index-mapping routines.
C     .
C
C---- THULE       as of 1999 Dec 07
      integer     MAXLV,MXTRA
      parameter   (MAXLV=50)
      parameter   (MXTRA=(MAXLV*(MAXLV-1))/2)
C     (Remember to recompile ADAM when changing MAXLV.)
      integer     LMTRA,NUMTRN,NUMBLK,LINNAM,LI1ADR,LI2ADR,LI3ADR
      dimension   LI1ADR(MXTRA),LI2ADR(MXTRA),LI3ADR(MXTRA)
      dimension   LINNAM(MXTRA)
      common      /THULE0/ LMTRA,NUMTRN,NUMBLK
      common      /THULE1/ LINNAM
      common      /THULE2/ LI1ADR
      common      /THULE3/ LI2ADR
      common      /THULE4/ LI3ADR
C
C     Indices and Names of the Line Intensity Data Blocks.
C     LMTRA  -    = MAXLV
C     NUMTRN -    number of transitions (i.e. AIJ .ne. 0), which is
C                 the number of Line Intensity Data Blocks allocated;
C     NUMBLK -    number of Line Intensity Data Blocks in actual use
C                 (i.e. number of radiative and passive transitions);
C     LINNAM -    block name, = 100*IU+IL;
C     LI1ADR -    block address in scratch memory, part 1;
C     LI2ADR -    block address in scratch memory, part 2;
C     LI3ADR -    block address in scratch memory, part 3.
C     .
C
C---- LIMBO       as of 2006 Jul 11
      integer     MXLEV,MAXTR,LIMTR
      parameter   (MXLEV=50)
      parameter   (MAXTR=(MXLEV*(MXLEV-1))/2)
      integer     LINIU ,LINIL ,LINKLN,LINPRD,LINPRO,LINMSE,
     $            LINMSF,LINDAM,LININK,LINTPS,LININR,LINFLX,
     $            LINLDL,LININT,LINPRN,LINFDB,LINOML,LINSBG,
     $            LINKBT,LINKRT,LINKST,LINKM
C     (Remember to recompile ADAM when changing MXLEV.)
      dimension   LINIU (MAXTR), LINIL (MAXTR), LINKLN(MAXTR),
     $            LINPRD(MAXTR), LINPRO(MAXTR), LINMSE(MAXTR),
     $            LINMSF(MAXTR), LINDAM(MAXTR), LININK(MAXTR),
     $            LINTPS(MAXTR), LININR(MAXTR), LINFLX(MAXTR),
     $            LINLDL(MAXTR), LININT(MAXTR), LINPRN(MAXTR),
     $            LINFDB(MAXTR), LINOML(MAXTR), LINSBG(MAXTR),
     $            LINKBT(MAXTR), LINKRT(MAXTR), LINKST(MAXTR),
     $            LINKM (MAXTR)
C
C     Line Source Function Calculation control parameters.
C
      common      /LIMBO10/ LIMTR
C     LIMTR  -    = MAXTR
      common      /LIMBO11/ LINIU
C     LINIU  -    UPPER level index of transition or line
      common      /LIMBO12/ LINIL
C     LINIL  -    LOWER level index of transition or line
      common      /LIMBO13/ LINKLN
C     LINKLN -    line type code:
C                 =0 for NO transition,
C                 =1 for RADIATIVE transition,
C                 =2 for PASSIVE transition,
C                 =3 for OPTICALLY-THIN transition,
C                 =4 for TWO-PHOTON transition,
C                 =5 for OPTICALLY-THICK transition
      common      /LIMBO14/ LINPRD
C     LINPRD -    partial redistribution calculation control:
C                 =0 if PRD calculation IS NOT required,
C                 =1 if Kneer-Heasley PRD (option PRDMETH=off)
C                 =2 if Hubeny-Lites PRD (option PRDMETH=on)
      common      /LIMBO15/ LINPRO
C     LINPRO -    emergent profiles calculation control:
C                 =0 if NO profiles are required,
C                 =1 if only "REGULAR" profile is required,
C                 =2 if only "ECLIPSE" profile is required,
C                 =3 if BOTH types of profile are required
      common      /LIMBO16/ LINMSE
C     LINMSE -    statistical equilibrium method selector:
C                 =0 for NOVA,
C                 =1 for Complex/UPPER,
C                 =2 for Complex/LOWER,
C                 =3 for CHAIN,
C                 =4 for VAMOS
      common      /LIMBO17/ LINMSF
C     LINMSF -    line source function method selector:
C                 =0 for RT (Ray tracing),
C                 =1 for QR, DIRECT (Quadratic representation),
C                 =2 for QR, MAPPED,
C                 =3 for GR, (General ray tracing)
      common      /LIMBO18/ LINDAM
C     LINDAM -    damping parameter components selector
      common      /LIMBO19/ LININK
C     LININK -    input opacity signal:
C                 =1 if there ARE input values of continuous opacity,
C                 =0 if there ARE NOT input values of continuous opacity
      common      /LIMBO20/ LINTPS
C     LINTPS -    source function solution method selector:
C                 =0 for FULL solution (involving matrix inversion),
C                 =1 for DIRECT solution (involving level propulations),
C                 =2 for ESCAPE PROBABILITY approximation
      common      /LIMBO21/ LININR
C     LININR -    Rho recomputation control:
C                 =0 if iteratively-recalculated Rho should be used,
C                 =1 if only input values of Rho should be used
      common      /LIMBO22/ LINFLX
C     LINFLX -    Line Flux calculation control:
C                 =1 if it IS required,
C                 =0 if it IS NOT required
      common      /LIMBO23/ LINLDL
C     LINLDL -    number of component lines:
C                 =1 if this is a SINGLE line,
C                 >1 if this is a BLENDED line
      common      /LIMBO24/ LININT
C     LININT -    Source Function frequency integration range:
C                 =0 if it is a HALF profile,
C                 =1 if it is a WHOLE profile
      common      /LIMBO25/ LINPRN
C     LINPRN -    Source Function calculation printout switch:
C                 =0 for NO printout nor graph,
C                 =1 for REGULAR Source Function printout and graph
      common      /LIMBO26/ LINFDB
C     LINFDB -    Source Function calculation background data switch:
C                 =0 for CONSTANT background
C                 (line core values used for every wavelength),
C                 =1 for FREQUENCY-DEPENDENT background
C                 (values are calculated for every wavelength)
      common      /LIMBO27/ LINOML
C     LINOML -    "Line Background Opacity" control
C                 (i.e. Composite, Statistical, or Averaged Opacity):
C                 =0 if this opacity should be suppressed at the
C                    wavelengths of this transition
C                 =1 if this opacity should NOT be suppressed
      common      /LIMBO28/ LINSBG
C     LINSBG -    Blended Line profile plot mode switch
C                 =0 for plotting as blend only,
C                 =1 for additonal separate plots of each component
      common      /LIMBO29/ LINKBT
C     LINKBT -    length of input table XIBLUT
      common      /LIMBO30/ LINKRT
C     LINKRT -    length of input table XIREDT
      common      /LIMBO31/ LINKST
C     LINKST -    length of input table XISYMT
      common      /LIMBO32/ LINKM
C     LINKM  -    length of actual tables XI and DL
C     .
C
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
C     .
      equivalence
     $(LINKDS( 1),IU   ),(LINKDS( 2),IL   ),(LINKDS( 3),KLIN ),
     $(LINKDS( 4),ICE  ),(LINKDS( 5),IPRO ),(LINKDS( 6),METSE),
     $(LINKDS( 7),METSF),(LINKDS( 8),IBRSW),(LINKDS( 9),INKSW),
     $(LINKDS(10),LSFT ),(LINKDS(11),ILFLX),(LINKDS(12),LDL  ),
     $(LINKDS(13),LINT ),(LINKDS(14),LSFP ),(LINKDS(15),IFDB ),
     $(LINKDS(16),ISBG ),(LINKDS(17),KBT  ),(LINKDS(18),KRT  ),
     $(LINKDS(19),KST  ),(LINKDS(20),KTRN ),(LINKDS(21),LOML ),
     $(LINKDS(22),kds22)
C     .
C
C---- PITCH       as of 2006 Jun 14
      integer     MXTRP
      parameter   (MXTRP=10)
C     (Remember to recompile all users when changing MXTRP)
      integer     NTRP,IPRDTR,ILNPRD,IBCADR,IKPADR,IJNADR,IRXADR,
     $                 ISGADR,IXDADR,IYDADR,IZDADR
      dimension   IPRDTR(MXTRP),ILNPRD(MXTRP),IBCADR(MXTRP),
     $            IKPADR(MXTRP),IJNADR(MXTRP),IRXADR(MXTRP),
     $            ISGADR(MXTRP),IXDADR(MXTRP),IYDADR(MXTRP),
     $            IZDADR(MXTRP)
C
      common      /PITCH/ NTRP,IPRDTR,ILNPRD,IBCADR,IKPADR,IJNADR,
     $                         IRXADR,ISGADR,IXDADR,IYDADR,IZDADR
C
C     Record addresses of PRD-data-arrays for up to MXTRP transitions.
C     .
C
C---- LEVDES      as of 1988 May 13
      integer     MILTY,LIMLV
      parameter   (MILTY=50)
C     (Remember to recompile ADAM when changing MILTY.)
      character   LEVDES*8
      dimension   LEVDES(MILTY)
      common      /LEVDES/ LIMLV,LEVDES
C
C     Term designations for levels of the ion of the run.
C     .
C
C---- SHEEPY      as of 1998 Jan 13
      integer     LINKS
      parameter   (LINKS=32)
C     (Be sure to recompile all users when changing LINKS!)
      real*8      TYME,TYME0,XNRW,XNRW0,XNRR,XNRR0,XNBW,XNBW0,
     $            XNBR,XNBR0
      integer     KALLS,MEMSIW,MEMSW
      character   SECNAM*8,SECTIT*32
      dimension   TYME(LINKS),XNRW(LINKS),XNRR(LINKS),XNBW(LINKS),
     $            XNBR(LINKS),KALLS(LINKS),MEMSIW(LINKS),MEMSW(LINKS),
     $            SECNAM(LINKS),SECTIT(LINKS)
      common      /STATS0/ TYME0,  TYME
      common      /STATS1/ XNRW0,  XNRW
      common      /STATS2/ XNRR0,  XNRR
      common      /STATS3/ XNBW0,  XNBW
      common      /STATS4/ XNBR0,  XNBR
      common      /MEMSC/  MEMSW,  MEMSIW
      common      /KALLS/  KALLS
      common      /SECDAT/ SECNAM, SECTIT
C---- Processing-Section data, and performance statistics for this run
C     (kept separately for each section).
C     .
C     SECNAM : module name for section entry;
C     SECTIT : brief section description;
C     TYME   : CPU time, in seconds;
C     XNRW   : count of random-access records written;
C     XNRR   : count of random-access records read;
C     XNBW   : count of random-access data bytes written;
C     XNBR   : count of random-access data bytes read;
C     MEMSW  : maximum amounts of r*8 scratch storage;
C     MEMSIW : maximum amounts of i*4 scratch storage; and
C     KALLS  : number of times invoked.
C     .
C
C---- CLOCK       as of 1998 Apr 02
      character   BEGDAT*11, ENDDAT*11, BEGTIM*8, ENDTIM*8
      common      /CLOCK/ BEGDAT,ENDDAT,BEGTIM,ENDTIM
C     .
C
C---- MACTAN      as of 1998 Apr 03
      real*8      FILDAT
      dimension   FILDAT(11)
      common      /MACTAN/ FILDAT
C---- Control parameters for the PANDORA random-access scratch file.
C     .
C
C---- ICON        as of 1999 Mar 30
      integer     ICBNCH,MXICON,MXIADR
      parameter   (ICBNCH=10)
      parameter   (MXICON=50*ICBNCH)
      parameter   (MXIADR=1000)
C     (Remember to recompile all users when changing any parameter!)
      integer     ICADRS,NIADR,NICON
      real*8      SSBUFF
      logical     ICSTRT, ICFULL
      dimension   ICADRS(MXIADR),SSBUFF(MXICON+ICBNCH)
      common      /ICON1/ NICON,NIADR,ICADRS
      common      /ICON2/ SSBUFF
      common      /ICON3/ ICSTRT, ICFULL
C     Buffer, and record addresses, and control parameters,
C     for saving/restoring Spectrum Summary data.
C     .
C
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
C
C---- RARTRY      as of 1998 Jun 02
      integer     LIMTRY
      common      /RARTRY/ LIMTRY
C     Control parameter for RAFAEL (random access I/O).
C     .
C
C---- DATAFIL     as of 1984 Apr 19
      integer     KIWILFN
      common      /DATAFIL/ KIWILFN
C     Number of unit from which to read input statements.
C     .
C
C---- CARDFIL     as of 1984 Apr 19
      integer     KIWIOUT
      common      /CARDFIL/ KIWIOUT
C     Number of unit to which to copy input statements.
C     .
C
C---- INCARD      as of 1984 Apr 23
      integer     LAST,LUINP,IRD,IPR,LUOUT,KARD,KART
      character   BUFFER*80
      common      /MAORI/  BUFFER
      common      /KAURI/  LAST
      common      /NUDEEL/ LUINP,IRD,IPR,LUOUT,KARD,KART
C     Storage and controls for reading input statements:
C     BUFFER - input line buffer;
C     LAST   - input buffer scan pointer;
C     LUINP,IRD,IPR,LUOUT,KARD,KART - "NUDEAL" control
C     parameters, q.v.
C     .
C
C---- VOISTA      as of 1984 Apr 24
      integer     KOUNVC
      common      /VOISTA/ KOUNVC
C     Counts number of times Voigt Function is evaluated.
C     .
C
C---- DIVIDER     as of 2003 Sep 29
      real*8      VSMLDV
      integer     MESDIV,MESDVZ,KNTDIV,KNTDVZ
      common      /DIVIDE1/ VSMLDV
      common      /DIVIDE2/ MESDIV,MESDVZ,KNTDIV,KNTDVZ
C     Parameters for subroutine "DIVIDE":
C       VSMLDV - replacement for B in A/B when B=0;
C       MESDIV - switch for A/0 error messages;
C       MESDVZ - switch for 0/0 error messages;
C       KNTDIV - number of times A/0 was detected;
C       KNTDVZ - number of times 0/0 was detected.
C     .
C
C---- WEITIM      as of 1990 Sep 05
      integer     KALLAM,KALPHI
      real*8      TIMLAM,TIMPHI
      common      /WEITIM1/ KALLAM,KALPHI
      common      /WEITIM2/ TIMLAM,TIMPHI
C     Weight matrix timing data:
C     1) KALLAM   - number of "Lambda-1" operator calculations;
C        TIMLAM   - total time (sec) for (1) calls.
C     2) KALPHI   - number of "Phi" operator calculations;
C        TIMPHI   - total time (sec) for (2) calls.
C     .
C
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
C     .
      equivalence
     $(KAKODS( 1),KAK1 ),(KAKODS( 2),KAK2 ),(KAKODS( 3),KAK3 ),
     $(KAKODS( 4),KTYPE)
C     .
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C
C---- SOLO        as of 2001 Aug 28
      integer     NLINSO,KLENSO
      parameter   (NLINSO=250)
      parameter   (KLENSO=108)
C     (REMEMBER to recompile JAY when changing these parameters!)
      integer     LNTRSO
      parameter   (LNTRSO=NLINSO*KLENSO)
      character   TREND*(LNTRSO), WSOLO*(KLENSO)
      common      /SOLO/ TREND,WSOLO
C     Working storage for Iterative Summary.
C     .
C
C---- KWACK       as of 2006 Mar 14
      integer     MWKS,NWKS,KSWPR,KSWSH
      parameter   (MWKS=27)
      logical     KWKS
      dimension   KWKS(MWKS),KSWPR(MWKS),KSWSH(MWKS)
C     (Need to revise     TURKOIS     when changing MWKS ! )
      common      /KWACK1/ NWKS,KWKS
      common      /KWACK2/ KSWPR
      common      /KWACK3/ KSWSH
C---- Codes describing "continuum" wavelengths
C
C      1: regular (constant background) line center, printed;
C      2: "additional" wavelength, no Eclipse;
C      3: "additional" wavelength, with Eclipse;
C      4: line source function background, PRD;
C      5: rates integrations, regular;
C      6: additional photoionization;
C      7: H- calculation;
C      8: dust temperature adjustment procedure;
C      9: HSE calculation;
C     10: "Lyman" calculation (level-K-to-continuum integration);
C     11: incident coronal radiation;
C     12: rates integrations, K-shell;
C     13: composite line opacity, no Eclipse;
C     14: miscellaneous;
C     15: composite line opacity, with Eclipse;
C     16: line source function background, FDB;
C     17: actual CO-lines opacity, fundamental;
C     18: FDB line center, printed;
C     19: regular (constant background) line center, not printed;
C     20: actual CO-lines opacity, first overtone;
C     21: actual CO-lines opacity, band limit;
C     22: actual CO-lines opacity, rotational;
C     23: actual CO-lines opacity, second overtone.
C     24: PRD line center, printed;
C     25: PRD line center, not printed;
C     26: FDB line center, not printed;
C     27: standard background.
C     .
C
C---- EXTRSW      as of 1992 Apr 01
      real*8      XTRALMU,XTRALML
      common      /EXTRSW/ XTRALMU,XTRALML
C     Parameters controlling the extra(inter)polation of input data
C     from an auxiliary Z-scale to the Z-scale of the run.
C     XTRALMU is the allowed upper limit for extrapolated values;
C     XTRALML is the allowed lower limit for extrapolated values.
C     .
C
C---- BASH        as of 1984 Apr 19
      integer            JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
      common      /BASH/ JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
C     Control data for "BEIGE".
C     .
C
C---- SUKU        as of 1988 Apr 22
      integer     NAF
      parameter   (NAF=5)
      integer     KFLOPN,KODEGN
      character   FILNMS*8, FILSPEC*60
      dimension   KFLOPN(NAF),FILNMS(NAF)
      common      /SUKU1/ KFLOPN
      common      /SUKU2/ FILNMS
      common      /SUKU3/ KODEGN,FILSPEC
C     Names and in-use codes for the main and the auxiliary
C     input files.
C     1=INPUT,  2=MODEL,  3=ATOM,  4=RESTART,  5=GENERAL.
C     .
C
C---- EYE         as of 2005 Jan 28
      integer     LSTKSEE,KODESEE,NSTKSEE,LUNSEE,LENHEAD
      logical     DOSEE,DOSTK,DOCHK
      character   HEADSEE*32, STCKSEE*32
      parameter   (LSTKSEE=100)
      dimension   STCKSEE(LSTKSEE)
      common      /EYE1/ DOSEE,DOSTK,DOCHK
      common      /EYE2/ KODESEE,NSTKSEE,LUNSEE,LENHEAD
      common      /EYE3/ HEADSEE,STCKSEE
C     Control parameters for Hi/Bye/Abort System (revised).
C     KODESEE =  0: nothing;
C             =  1: ABORT traceback;
C             =  2: type to screen;
C             =  3: write to file;
C             = 98: check X ?!, to screen
C             = 99: check X ?!, to file
C     HEADSEE = name of highest node for 2 or 3.
C     .
C
C---- KNTPEFE     as of 2003 Nov 20
      integer     KNTPF
      parameter   (KNTPF=5)
C     The number of alternative methods for computing PE and FE.
C     (Used in JOAN, SWALLOW.)
C     .
C
C---- KNTLYEP     as of 1988 Feb 10
      integer     KNTLE
      parameter   (KNTLE=4)
C     The number of alternative methods for computing Lyman EPn.
C     (Used in CASSIA, MODOC.)
C     .
C
C---- KNTKUPL     as of 1993 Sep 15
      integer     KNTKU
      parameter   (KNTKU=5)
C     Control parameter for "Line" opacity plots.
C     (Used in CADMOS, MAYA, SILAS.)
C     .
C
C---- ZINDEX      as of 1984 Apr 24
      integer     MAUXZI
      common      /ZINDEX/ MAUXZI
C     Auxiliary Z-scale index, for input processing.
C     .
C
C---- THETYS      as of 2000 Feb 11
      integer     MINTHE
      parameter   (MINTHE=3)
C     Number of "intermediate points" for the "standard"
C     second-order integration scheme.
C     .
C
C---- ALVIN       as of 1995 Aug 08
      integer     LIMSCL
      parameter   (LIMSCL=11)
C     The number of TAU-scales to be printed fancily.
C     (Used in PICTURE, etc.)
C     .
C
C---- NICNAC      as of 1988 Feb 10
      integer     KFS,KFN
      dimension   KFN(3)
      common      /NICNAC/ KFS,KFN
C     Error counts, for Continuous Emergent Intensity printouts.
C     .
C
C---- TORUS       as of 2000 Feb 07
      integer     KTRKAS,KTRKIS,KTRKIL
      common      /TORUS/ KTRKAS,KTRKIS,KTRKIL
C     Tau-reduction data from some weight matrix calculations.
C     .
C
C---- RAGU        as of 2000 Mar 02
      real*8      RASAHA,RASQR,RASQRA,RASQC,RASQCA,RARRC,RACRR,RADRC,
     $            RAWRR,RADEP,RAH,RATSQR,RATSQC,RASQS,RAX
      common      /RAGU/ RASAHA,RASQR,RASQRA,RASQC,RASQCA,RARRC,RACRR,
     $                   RADRC,RAWRR,RADEP,RATSQR,RATSQC,RASQS,RAH,RAX
C     Intermediate results of recombination calculation.
C     .
C
C---- VERSION     as of 2006 May 16
      real*8      VERSION
      integer     NVDSCR
      character   VDSCRPT*63
      dimension   VDSCRPT(45)
      common      /VERSION1/ VERSION
      common      /VERSION2/ NVDSCR
      common      /VERSION3/ VDSCRPT
C     Identifier and description of this version of PANDORA.
C     (Values set by subroutine AARDVRK.)
C     .
C
C---- THUJA       as of 1988 Feb 16
      real*8      THUSN,THUC1,THUC2,THUC3
      common      /THUJA/ THUSN,THUC1,THUC2,THUC3
C     Intermediates for subroutine CHUCK.
C     .
C
C---- TAIMEN      as of 1988 Feb 16
      real*8      TAIVS,TAIC1,TAIC2,TAIC3,TAIC4
      common      /TAIMEN/ TAIVS,TAIC1,TAIC2,TAIC3,TAIC4
C     Intermediates for subroutine CHICK.
C     .
C
C---- TIGARA      as of 1988 Feb 17
      real*8      TIGTIS,TIGES2,TIGES3,TIGES4
      common      /TIGARA/ TIGTIS,TIGES2,TIGES3,TIGES4
C     Intermediates for subroutine CLOVIS.
C     .
C
C---- SIRIT       as of 1988 Apr 22
      real*8      DORTIS,DORE2S,DORE3S,DORE4S
      common      /SIRIT/ DORTIS,DORE2S,DORE3S,DORE4S
C     Intermediates for subroutine DORIS.
C     .
C
C---- BURNET      as of 1995 Sep 08
      integer     NURBET,KERMED
      parameter   (NURBET=12)
      dimension   KERMED(NURBET)
      common      /BURNET/ KERMED
C     Counts of error messages from EDITH, for various contexts:
C      1 - "optical depth"          2 - basic b-ratios
C      3 - PRD QSF                  4 - whole-profile S
C      5 - line source function     6 - J-bar
C      7 - "Lyman" EP1              8 - "Lyman" RK
C      9 - b-values                10 - net radiative bracket - "rho"
C     11 - GTN or TAU-integrand    12 - S-from-N
C     .
C
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
C
C---- SENGE       as of 1989 Dec 18
      integer     NNDCK
      common      /SENGE/ NNDCK
C     Unique "serial number" for GORSE checksums.
C     .
C
C---- SOBOLEV     as of 1993 Aug 20
      integer     MDSOB
      common      /SOBOLEV/ MDSOB
C     Counter value for Sobolev escape probability calculation.
C     .
C
C---- LOOPER      as of 2006 May 05
      integer     NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      real*8      EMOO,WVLTRN
      logical     EXPAND,SPHERE,VXZERO,FLOBRD
      common      /LOOPER1/ NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      common      /LOOPER2/ EMOO,WVLTRN
      common      /LOOPER3/ EXPAND,SPHERE,VXZERO,FLOBRD
C
C     Emergent Profiles calculations control data (simplified version).
C
C     NVEL   : number of velocity tables
C     NVY    : current value of velocity-loop index, 1.le.NVY.le.NVEL
C              (i.e. index of current velocity set)
C     JVEL   : code describing current velocity set (i.e. KVEL(NVY) )
C              =     1 : VSB
C              =     2 : VXS
C              =     3 : VADD     (from AMDIFF and/or VELGRAD)
C              = 100+j : VXN(j)
C              = 200+j : VXN(j) + VADD
C              = 300+j : VAX(j)
C              = 400+j : VFB(j)
C              = 500+j : VFB(j) + VADD
C
C     LFBV   : number of viewing positions (front only, or back also)
C     LFB    : current value of views-loop index, 1 .le. LFB .le. LFBV
C              = 1 - front-face
C              = 2 - back-face
C
C     MF     : current value of look-angles-loop index, 1.le.MF.le.LF
C     MUK    : is .gt. 0 if line intensity profile must be printed
C              [when MUK > 0, then EMOO = EMU(MUK) ]
C     EMOO   : current value of look-angle
C     WVLTRN : wavelength (Angstroms) (i.e. at Delta-Lambda = 0).
C
C     VXZERO : tells whether the current velocity =0, or not
C     EXPAND : tells whether the procedures for expanding atmospheres
C              should be used (set up in SWEET)
C     SPHERE : tells whether this is a spherical, as opposed to
C              plane-parallel, atmosphere
C     FLOBRD : tells whether to use the flow-broadening procedure
C     .
C
C---- GAUSH10     as of 1990 Nov 29
      real*8      XGH10,AGH10
      dimension   XGH10(10), AGH10(10)
      common      /GAUSH10/ XGH10,AGH10
C     Roots and weights for 10-point Gauss-Hermite quadrature:
C     Integral(0,inf) [ exp(-x**2) * f{x} dx ] =
C     sum(k=1,10) [ AGH10(k) * f{XGH10(k)} ].
C     .
C
C---- WAGRAM      as of 1997 Nov 19
      real*8      XWGRTC
      integer     IWGRBS
      common      /WAGRAM1/ IWGRBS
      common      /WAGRAM2/ XWGRTC
C     Control parameters for scattering albedo analysis (LINECOMP):
C     IWGRBS - size of wavelength batch, from which 2 are picked,
C     XWGRTC - minimum TAU criterion.
C     .
C
C---- SWABORT     as of 1996 Nov 27
      logical     SWDELA, SWPEND
      common      /SWABORT/ SWDELA, SWPEND
C     Control switches for subroutine ABORT:
C     SWDELA - delay an abort;
C     SWPEND - a delayed abort is pending.
C     .
C
C---- CARGOT      as of 1996 Dec 10
      logical     ESCARGO
      common      /CARGOT/ ESCARGO
C     Input reading signal from subroutine CARMEN.
C     .
C
C---- NARITO      as of 2005 Jul 22
      integer     NTMXSW
C     (Remember to recompile users when changing NTMXSW.)
      parameter   (NTMXSW=10000)
C     Upper limit for standard rates integration wavelengths.
C     .
C
C---- NOTHER      as of 2004 Jun 25
      integer     NIAUGM
      parameter   (NIAUGM=3000)
C     (Be sure to recompile all users when changing NIAUGM ! )
C     Upper limit for total profile data points for coincident
C     background lines.
C     .
C
C---- STORPO      as of 2005 Feb 03
      logical     WRLDHO, WRLDPR, WRLDTY
      common      /STORPO/ WRLDHO,WRLDPR,WRLDTY
C     Storage management debug printout control.
C     (See input parameter WORLDLY in Part B.)
C     .
C
C---- KONOUT      as of 2004 Jan 08
      integer     KONLUN,KONLUR,KONLUD,KONHED
      common      /KONOUT/ KONLUN,KONLUR,KONLUD,KONHED
C     Logical output unit numbers for Continuum Calculations.
C             *** Initialized in PARLOR. ***
C
C     KONLUN: potential output unit number
C     KONLUR: unit number for regular output
C
C     KONLUD: =1 if dump output is authorized
C     KONHED: =1 if wavelength header has already been printed
C     .
C
C---- HILYLI      as of 2005 Dec 22
      integer     LYLINO
      common      /HILYLI/ LYLINO
C     Index of upper level, if this is an H Lyman line wavelength
C     (see subroutine ISTUR).
C     .
C
C---- SPONNE      as of 2000 Dec 18
      integer     N1SKNT,NKSKNT
      common      /SPONNE/ N1SKNT,NKSKNT
C     Control parameter for Special-N1,NK iterative summary
C     .
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C
C---- MISHED      as of 2003 Dec 11
      parameter   (MSHLNG=50)
      integer     MSHCNO,MSHLNG
      character   MSHCLR*40
      dimension   MSHCLR(MSHLNG)
      common      /MISHED1/ MSHCNO
      common      /MISHED2/ MSHCLR
C     Caller stack for MESHED/MASHED messaging system.
C     .
C
C---- TANGELO     as of 2007 Mar 26
      integer     LCISW,NCISW,KCISW,KCISWD,LCESW,NCESW,KCESW,KCESWD
      parameter   (LCISW=6, LCESW=7)
      dimension   KCISW(LCISW), KCISWD(LCISW)
      dimension   KCESW(LCESW), KCESWD(LCESW)
      common      /TANGELO1/ NCISW,KCISW,KCISWD
      common      /TANGELO2/ NCESW,KCESW,KCESWD
C     Control switches for default CI & CE calculations.
C CI: 1 SHAH, 2 AR, 3 VORONOV, 4 VS, 5 JOHNSON, 6 CLARK
C CE: 1 SCHOLZ, 2 PB, 3 VS, 4 JOHNSON, 5 SEATON, 6 VREGE, 7 AGGRWL
C     (The default configurations are set up in subroutine SULTANA.)
C     .
C     !EJECT
C
C---- P A R T  4: Opacity Paraphernalia
C
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C
C---- FORGO       as of 2007 Jan 12
      integer     MLINCC,NLINCC,LLINCC
      parameter   (MLINCC=18)
      dimension   LLINCC(MLINCC)
      common      /FORGO/ NLINCC,LLINCC
C     List of continuum contributors omitted from "true" continuum
C     .
C
C---- XRAYLIM     as of 1986 Mar 06
      real*8      XRAYLO,XRAYHI
      common      /XRAYLIM/ XRAYLO,XRAYHI
C     Wavelength limits (Angstroms) for X-ray opacity.
C     .
C
C---- LYMALIM     as of 2005 Jul 08
      real*8      XKWAVU,XKWAVL
      common      /LYMALIM/ XKWAVU,XKWAVL
C     Wavelength limits (Angstroms) for "Lyman" calculation.
C     .
C
C---- SENNA       as of 2007 Jan 12
      parameter   (LCSBA=54)
      real*8      CSBA,CSBO
      integer     LCSBA,NCSBA
      character   LABSBA*16
      dimension   CSBA(LCSBA),CSBO(LCSBA),LABSBA(LCSBA)
      common      /SENNA1/ NCSBA
      common      /SENNA2/ CSBA
      common      /SENNA3/ LABSBA
      common      /SENNA4/ CSBO
C     Checksums of quantities used to compute the various components of
C     the background absorption and the background emisssion.
C     These checksums         M  U  S  T        be updated whenever the
C              corresponding quantities are recomputed.
C
C        1 TE                 2 V                  3 XNE
C        4 HND                5 BDHM               6 TDUST
C        7 H2N                8 CON
C        9 H(nlev)           10 H(bd)             11 H(nk)
C       12 He-I(nlev)        13 He-I(bd)          14 He-I(nk)
C       15 HE-II(nlev)       16 He-II(bd)         17 He-II(nk)
C       18 C-I(nlev)         19 C-I(bd)           20 C-I(nk)
C       21 Si-I(nlev)        22 Si-I(bd)          23 Si-I(nk)
C       24 Al-I(nlev)        25 Al-I(bd)          26 Al-I(nk)
C       27 Mg-I(nlev)        28 Mg-I(bd)          29 Mg-I(nk)
C       30 Fe-I(nlev)        31 Fe-I(bd)          32 Fe-I(nk)
C       33 Na-I(nlev)        34 Na-I(bd)          35 Na-I(nk)
C       36 Ca-I(nlev)        37 Ca-I(bd)          38 Ca-I(nk)
C       39 VM
C       40 O-I(nlev)         41 O-I(bd)           42 O-I(nk)
C       43 H1
C       44 S-I(nlev)         45 S-I(bd)           46 S-I(nk)
C       47 CHN               48 OHN
C       49 O-II(nlev)        50 O-II(bd)          51 O-II(nk)
C       52 O-III(nlev)       53 O-III(bd)         54 O-III(nk)
C     .
C
C---- FERGO       as of 2004 Aug 05
      parameter   (MHYL=24)
      integer     MHYL, IUHY, ILHY
      real*8      HYWVL, HYWLO, HYWHI, HYNUU, HYNUL
      dimension   HYWVL(MHYL), HYWLO(MHYL), HYWHI(MHYL), HYNUU(MHYL),
     $            HYNUL(MHYL), IUHY(MHYL),  ILHY(MHYL)
      common      /FERGO0/ HYWVL,HYWLO,HYWHI
      common      /FERGO1/ HYNUU,HYNUL
      common      /FERGO2/ IUHY,ILHY
C     Data for Hydrogen Lyman lines in the background.
C     .
C
C---- FIRGO       as of 2005 Jul 07
      parameter   (MHEE=4)
      integer     MHEE, IUHEE, ILHEE
      real*8      HEEMAS, HEEWVL, HEEWLO, HEEWHI, HEENUU, HEENUL
      real*8      HEEAUL, HEEPU,  HEEPL,  HEECRD, HEECVW, HEECSK
      real*8      HEESKE
      dimension   HEEWVL(MHEE), HEEWLO(MHEE), HEEWHI(MHEE),
     $            HEENUU(MHEE), HEENUL(MHEE), HEEPU(MHEE),
     $            HEEPL(MHEE),  HEEAUL(MHEE), HEECRD(MHEE),
     $            HEECVW(MHEE), HEECSK(MHEE),
     $            IUHEE(MHEE),  ILHEE(MHEE)
      common      /FIRGO0/ HEEMAS,HEESKE
      common      /FIRGO1/ HEEWVL,HEEWLO,HEEWHI
      common      /FIRGO2/ HEENUU,HEENUL,HEEPU,HEEPL
      common      /FIRGO3/ HEEAUL,HEECRD,HEECVW,HEECSK
      common      /FIRGO4/ IUHEE,ILHEE
C     Data for Helium lines in the background.
C     .
C
C---- FARGO       as of 2004 Jul 19
      parameter   (MHEL=7, LHEL=7)
      integer     MHEL, LHEL, IUHE, ILHE, LDLHE
      real*8      HEMAS, HESKE, HEWVL, HEWLO, HEWHI, HENUU, HENUL, HEAUL
      real*8      HEPU,  HEPL,  HEDDL, HECDL, HECRD, HECVW, HECSK
      dimension   HEWVL(MHEL), HEWLO(MHEL), HEWHI(MHEL), HENUU(MHEL),
     $            HENUL(MHEL), HEPU(MHEL),  HEPL(MHEL),  HEAUL(MHEL),
     $            IUHE(MHEL),  ILHE(MHEL),  LDLHE(MHEL)
      dimension   HEDDL(LHEL,MHEL), HECDL(LHEL,MHEL),
     $            HECRD(LHEL,MHEL), HECVW(LHEL,MHEL), HECSK(LHEL,MHEL)
      common      /FARGO0/ HEMAS,HESKE
      common      /FARGO1/ HEWVL,HEWLO,HEWHI
      common      /FARGO2/ HENUU,HENUL,HEPU,HEPL
      common      /FARGO3/ HEAUL,HEDDL,HECDL,HECRD,HECVW,HECSK
      common      /FARGO4/ IUHE,ILHE,LDLHE
C     Data for Helium-II lines in the background.
C     .
C
C---- FURGO       as of 2004 Jun 11
      parameter   (MOXL=11)
      integer     MOXL, IUOX, ILOX
      real*8      OXMAS, OXSKE, OXWVL, OXWLO, OXWHI, OXNUU, OXNUL
      real*8      OXPU,  OXPL,  OXAUL, OXCRD, OXCVW, OXCSK
      dimension   OXWVL(MOXL), OXWLO(MOXL), OXWHI(MOXL), OXNUU(MOXL),
     $            OXNUL(MOXL), OXPU(MOXL),  OXPL(MOXL),  OXAUL(MOXL),
     $            OXCRD(MOXL), OXCVW(MOXL), OXCSK(MOXL),
     $            IUOX(MOXL),  ILOX(MOXL)
      common      /FURGO0/ OXMAS,OXSKE
      common      /FURGO1/ OXWVL,OXWLO,OXWHI
      common      /FURGO2/ OXNUU,OXNUL,OXPU,OXPL
      common      /FURGO3/ OXAUL,OXCRD,OXCVW,OXCSK
      common      /FURGO4/ IUOX,ILOX
C     Data for Oxygen-I lines in the background.
C     .
C
C---- WURGO       as of 2007 Jan 25
      parameter   (MX2L=1, LX2L=3)
      integer     MX2L, LX2L, IUX2, ILX2, LDLX2
      real*8      X2MAS, X2SKE, X2WVL, X2WLO, X2WHI, X2NUU, X2NUL, X2AUL
      real*8      X2PU,  X2PL,  X2DDL, X2CDL, X2CRD, X2CVW, X2CSK
      dimension   X2WVL(MX2L), X2WLO(MX2L), X2WHI(MX2L), X2NUU(MX2L),
     $            X2NUL(MX2L), X2PU(MX2L),  X2PL(MX2L),  X2AUL(MX2L),
     $            IUX2(MX2L),  ILX2(MX2L),  LDLX2(MX2L)
      dimension   X2DDL(LX2L,MX2L), X2CDL(LX2L,MX2L),
     $            X2CRD(LX2L,MX2L), X2CVW(LX2L,MX2L), X2CSK(LX2L,MX2L)
      common      /WURGO0/ X2MAS,X2SKE
      common      /WURGO1/ X2WVL,X2WLO,X2WHI
      common      /WURGO2/ X2NUU,X2NUL,X2PU,X2PL
      common      /WURGO3/ X2AUL,X2DDL,X2CDL,X2CRD,X2CVW,X2CSK
      common      /WURGO4/ IUX2,ILX2,LDLX2
C     Data for Oxygen-II lines in the background.
C     .
C
C---- WARGO       as of 2007 Jan 18
      parameter   (MX3L=2, LX3L=6)
      integer     MX3L, LX3L, IUX3, ILX3, LDLX3
      real*8      X3MAS, X3SKE, X3WVL, X3WLO, X3WHI, X3NUU, X3NUL, X3AUL
      real*8      X3PU,  X3PL,  X3DDL, X3CDL, X3CRD, X3CVW, X3CSK
      dimension   X3WVL(MX3L), X3WLO(MX3L), X3WHI(MX3L), X3NUU(MX3L),
     $            X3NUL(MX3L), X3PU(MX3L),  X3PL(MX3L),  X3AUL(MX3L),
     $            IUX3(MX3L),  ILX3(MX3L),  LDLX3(MX3L)
      dimension   X3DDL(LX3L,MX3L), X3CDL(LX3L,MX3L),
     $            X3CRD(LX3L,MX3L), X3CVW(LX3L,MX3L), X3CSK(LX3L,MX3L)
      common      /WARGO0/ X3MAS,X3SKE
      common      /WARGO1/ X3WVL,X3WLO,X3WHI
      common      /WARGO2/ X3NUU,X3NUL,X3PU,X3PL
      common      /WARGO3/ X3AUL,X3DDL,X3CDL,X3CRD,X3CVW,X3CSK
      common      /WARGO4/ IUX3,ILX3,LDLX3
C     Data for Oxygen-III lines in the background.
C     .
C
C---- LIFFEY      as of 2005 Nov 02
      real*8      FLNRML
      dimension   FLNRML(15)
      common      /LIFFEY/ FLNRML
C     Background H Ly alpha & beta normalization factor for the
C     current value of wavelength (only #2 and #3 can differ from 1)
C     (FLNRML is set up by GROAN)
C     .
C
C---- LOOM        as of 1994 Dec 16
      integer     JCOLMF,KCOLMF,JCOLMO,KCOLMO,JCOLMS,KCOLMS,
     $            JCOLMR,KCOLMR,JCOMAX,KCOMAX
      parameter   (JCOLMF=111, KCOLMF=20)
      parameter   (JCOLMO=111, KCOLMO=13)
      parameter   (JCOLMS=111, KCOLMS=12)
      parameter   (JCOLMR=53,  KCOLMR=21)
      parameter   (JCOMAX=max(JCOLMF,JCOLMO,JCOLMS,JCOLMR))
      parameter   (KCOMAX=max(KCOLMF,KCOLMO,KCOLMS,KCOLMR))
C     (Be sure to recompile all users of LOOM when changing any
C     of the above parameter values!)
      real*8      WNUPF,WNDNF,WNUPO,WNDNO,WNUPS,WNDNS,WNROT,
     $            FUPF ,FDNF ,FUPO ,FDNO ,FUPS ,FDNS ,FROT ,
     $            EXEN,WVCOLO,WVCOHI,RC1213
      integer     JCOFUN,KCOFUN,JCOOVR,KCOOVR,JCOSEC,KCOSEC,
     $            JCOROT,KCOROT,ISOSLCT,METHCOW,METHCOF
      logical     FUNDCO,OVERCO,SECNCO,ROTACO
C
      dimension   WNUPF(JCOLMF,KCOLMF,2), WNDNF(JCOLMF,KCOLMF,2),
     $            WNUPO(JCOLMO,KCOLMO,2), WNDNO(JCOLMO,KCOLMO,2),
     $            WNUPS(JCOLMS,KCOLMS,2), WNDNS(JCOLMS,KCOLMS,2),
     $            WNROT(JCOLMR,KCOLMR,2),
     $            FUPF(JCOLMF,KCOLMF,2),  FDNF(JCOLMF,KCOLMF,2),
     $            FUPO(JCOLMO,KCOLMO,2),  FDNO(JCOLMO,KCOLMO,2),
     $            FUPS(JCOLMS,KCOLMS,2),  FDNS(JCOLMS,KCOLMS,2),
     $            FROT(JCOLMR,KCOLMR,2),
     $            EXEN(JCOMAX+1,KCOMAX,2)
      dimension   FUNDCO(2),OVERCO(2),SECNCO(2),ROTACO(2),
     $            JCOFUN(2),KCOFUN(2),JCOOVR(2),KCOOVR(2),
     $            JCOSEC(2),KCOSEC(2),JCOROT(2),KCOROT(2)
C
C     Carbon Monoxide lines:  wavenumbers and oscillator strengths.
C
C     WNROT, etc., are for rotational lines     : j'-j=+1, v'-v=0;
C     WNUPF, etc., are for fundamental lines    : j'-j=+1, v'-v=1;
C     WNDNF, etc., are for fundamental lines    : j'-j=-1, v'-v=1;
C     WNUPO, etc., are for first overtone lines : j'-j=+1, v'-v=2;
C     WNDNO, etc., are for first overtone lines : j'-j=-1, v'-v=2;
C     WNUPS, etc., are for second overtone lines: j'-j=+1, v'-v=3;
C     WNDNS, etc., are for second overtone lines: j'-j=-1, v'-v=3.
C
C     This is how the array indices relate to the quantum numbers:
C
C     1. index J corresponds to quantum number j -
C                in WNROT, etc., j = J-1, j' = J
C                in WNUPF, etc., j = J-1, j' = J
C                in WNDNF, etc., j = J  , j' = J-1
C                in WNUPO, etc., j = J-1, j' = J
C                in WNDNO, etc., j = J  , j' = J-1
C                in WNUPS, etc., j = J-1, j' = J
C                in WNDNS, etc., j = J  , j' = J-1
C
C     2. index K corresponds to quantum number v (=nu) -
C                in WNROT, etc.,                 v = K-1, v' = K-1
C                in WNUPF, etc. and WNDNF, etc., v = K-1, v' = K
C                in WNUPO, etc. and WNDNO, etc., v = K-1, v' = K+1
C                in WNUPS, etc. and WNDNS, etc., v = K-1, v' = K+2
C
C     3. index L identifies the Carbon isotope -
C                L=1 for Carbon-12, L=2 for Carbon-13.
C
C     Wavenumbers and energies from:
C        Farrenq, R., Guelachvili, G., Sauval, A.J., Grevesse, N.,
C        and Farmer, C.B. 1991,
C        J.Molec.Spectrosc., 149, 375
C       or
C        Coxon, J.A., and Hajigeorgiou, P.G. 1992,
C        Can.J.Phys., 70, 40-54
C       or
C        Goorvitch, 1994, Ap.J.Suppl.
C     f-values from:
C        Chackerian, C., Jr, and Tipping, R. H. 1983
C        J.Molec.Spectros., 99, 431
C       or
C        "improved" Chackerian
C       or
C        Goorvitch, 1994, Ap.J.Suppl.
C
      common      /LOOM1 / EXEN
      common      /LOOM2 / WNROT,FROT
      common      /LOOM3 / WNUPF,FUPF,WNDNF,FDNF
      common      /LOOM4 / WNUPO,FUPO,WNDNO,FDNO
      common      /LOOM5 / WNUPS,FUPS,WNDNS,FDNS
      common      /LOOM6 / JCOFUN,KCOFUN,JCOOVR,KCOOVR,JCOSEC,KCOSEC,
     $                     JCOROT,KCOROT
      common      /LOOM7 / WVCOLO,WVCOHI
      common      /LOOM8 / FUNDCO,OVERCO,SECNCO,ROTACO
C
C     (These parameters are computed by subroutines WEAVE and FOCA.)
C
      common      /LOOM9 / RC1213
      common      /LOOM10/ ISOSLCT,METHCOW,METHCOF
C
C     RC1213  is the C(12)/C(13) isotopic abundance ratio.
C     ISOSLCT controls which isotopes are included in this calculation:
C     ISOSLCT = 1 means: C(12) only;
C     ISOSLCT = 2 means: C(13) only; and
C     ISOSLCT = 3 means: both C(12) and C(13).
C
C     METHCOF = 1 means: use f-values from Chackerian & Tipping (1983);
C     METHCOF = 2 means: use f-values from "new" Chackerian data;
C     METHCOF = 3 means: use f-values from Goorvitch (1994).
C
C     METHCOW = 1 means: use energies from Farrenq et al.;
C     METHCOW = 2 means: use energies from Coxon & Hajigeorgiou;
C     METHCOW = 3 means: use wave numbers & energies from
C                                                    Goorvitch (1994).
C     .
C
C---- ARABIS      as of 1989 Feb 22
      integer     NCOOP,NCOPR
      real*8      COOTM
      common      /ARABIS1/ NCOOP,NCOPR
      common      /ARABIS2/ COOTM
C     NCOOP = number of "bona fide" CO opacity values requested;
C     NCOPR = number of Voigt profile evaluations for CO opacity;
C     COOTM = total time (sec) for "bona fide" values.
C     .
C
C---- NIVELON     as of 1994 May 31
      integer     KMXBND,KMXWAV
      common      /NIVELON/ KMXBND,KMXWAV
C     Limits for tables of Composite Lines Opacity data.
C     .
C
C---- APULIA      as of 1994 Nov 02
      real*8      RAYSLM
      common      /APULIA/ RAYSLM
C     Wavelength crossover for Rayleigh scattering computations.
C     .
C     !EJECT
C
C---- P A R T  5: Special Tables
C
C---- ELEMENT     as of 1998 Aug 17
      integer     NELX
      parameter   (NELX=50)
C     (Remember to recompile all users when changing NELX)
      real*8      ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      integer     LATNO,LDEFR,NMT,NMTMAX
      logical     LATEM
      character   ELSYM*3, ELSUB*3
      dimension   ELSYM(NELX),ELSUB(NELX),ELABD(NELX),ELCHI(NELX),
     $            ELLU1(NELX),ELLU2(NELX),ELABL(NELX),ELDEF(NELX),
     $            LATNO(NELX),LDEFR(NELX),LATEM(NELX)
C
      common      /ELEMNT0/ NMT,NMTMAX
      common      /ELEMNT1/ ELSYM,ELSUB
      common      /ELEMNT2/ ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      common      /ELEMNT3/ LATNO,LDEFR
      common      /ELEMNT4/ LATEM
C
C     Element data tables:
C             ELSYM - element symbol;
C             ELSUB - (Scratch storage for I.D. symbols);
C             ELABD - abundance (w.r.t. Hydrogen);
C             ELCHI - Chi, i.e. ionization potential;
C             ELLU1 - U-I partition function;
C             ELLU2 - U-II partition function;
C             ELABL - logarithmic abundance;
C             ELDEF - defaults values of logarithmic abundance;
C             LATNO - atomic number; and
C             LDEFR - default values sources codes.
C             LATEM - "metal" designator
C     .
C
C---- ELEGANT     as of 2002 Mar 26
      integer     LNAM,LABR,NELE
      character   ENAMES*12,EABBRS*2
      dimension   LNAM(92),LABR(92),ENAMES(92),EABBRS(92)
      common      /ELEGNT1/ NELE,LNAM,LABR
      common      /ELEGNT2/ ENAMES,EABBRS
C     "ENAMES" is a list of element names, and "LNAM" specifies
C     the number of characters in each name.
C     "EABBRS" is the corresponding list of element symbols, and
C     "LABR" specifies the number of characters in each symbol.
C     .
C
C---- IONDATA     as of 2007 Jan 12
C     Data tables for the built-in models of "non-LTE" ions; these
C     models are used to compute bound-free absorption and emission.
C     Hydrogen (of course!) is treated as a special case.
      integer     NPION,NIONL,NDPNT
C
      parameter   (NPION=14)
C     NPION     = number of ion models, as follows:
C     01: H      02: C-I    03: Si-I   04: He-I   05: He-II  06: Al-I
C     07: Mg-I   08: Fe-I   09: Na-I   10: Ca-I   11: O-I    12: S-I
C     13: O-II   14: O-III
C
      parameter   (NIONL=15)
C     NIONL     = maximum number of levels in each model
C
      parameter   (NDPNT=150)
C     NDPNT     = maximum length of tables specifying the wavelength-
C     dependence of the absorption in each continuum, by
C     piece-wise linear approximation.
C
C     REMEMBER to recompile all users when changing NPION, NIONL, NDPNT.
C
      real*8      PILEVL,XLMTHR,CCPLEV,SCPLEV,XLMTAB,RCPTAB
      integer     LIMDAT,MAXDATL,LEND,NPTABL
      character   LLABEL*16
      logical     LLPRNT
      dimension   LIMDAT(            NPION), LLPRNT(            NPION),
     $            PILEVL(      NIONL,NPION), XLMTHR(      NIONL,NPION),
     $            NPTABL(      NIONL,NPION), CCPLEV(      NIONL,NPION),
     $            SCPLEV(      NIONL,NPION), LLABEL(      NIONL,NPION),
     $            XLMTAB(NDPNT,NIONL,NPION), RCPTAB(NDPNT,NIONL,NPION)
C
C     LIMDAT    = actual number of levels in each model (LIMDAT should
C                 equal LIMPOP in labelled common POPDATA)
C     MAXDATL   = maximum value of LIMDAT
C     LEND      = sum of LIMDAT
C     LLABEL    = "term designation" of each level of each model
C     LLPRNT    = data tables print switch
C     PILEVL    = statistical weight of each level of each model
C     XLMTHR    = threshhold wavelengths of continua
C     NPTABL    = actual number of data points in absorption data table
C                 of each level of each model (used only when > 0)
C     CCPLEV    = threshhold absorption factors
C     SCPLEV    = exponent of power-law wavelength dependence of
C                 absorption of each level of each model (used only
C                 when > 0; should be > 0 when corresponding NPTABL = 0)
C     XLMTAB    = wavelength values for which RCPTAB is specified
C     RCPTAB    = absorption in the continuum of a level of a model
C                 (At wavelengths < XLMTAB(NPTABL), a power law
C                  with exponent = 3 is used.)
C
      common      /IODAT01/ MAXDATL,LEND
      common      /IODAT02/ LIMDAT
      common      /IODAT03/ LLPRNT
      common      /IODAT04/ PILEVL
      common      /IODAT05/ XLMTHR
      common      /IODAT06/ NPTABL
      common      /IODAT07/ CCPLEV
      common      /IODAT08/ SCPLEV
      common      /IODAT09/ LLABEL
      common      /IODAT10/ XLMTAB
      common      /IODAT11/ RCPTAB
C     .
C     !EJECT
C
C     I N I T I A L I Z A T I O N   O F   L A B E L L E D   C O M M O N
C
C
      integer   LACK,MIL2
      parameter (LACK=-317)
      parameter (MIL2=MILTI*MILTI)
C
      integer   ILEVH,ILEVC,ILEVSI,ILEVHE1,ILEVHE2,ILEVAL,ILEVMG,
     $          ILEVFE,ILEVNA,ILEVCA,ILENTP,ILEVO,ILEVS,ILEVO2,
     $          ILEVO3
      parameter (ILEVH  =NIONL)
      parameter (ILEVC  =8)
      parameter (ILEVSI =8)
      parameter (ILEVHE1=13)
      parameter (ILEVHE2=8)
      parameter (ILEVAL =8)
      parameter (ILEVMG =8)
      parameter (ILEVFE =8)
      parameter (ILEVNA =8)
      parameter (ILEVCA =8)
      parameter (ILEVO  =14)
      parameter (ILEVS  =8)
      parameter (ILEVO2 =8)
      parameter (ILEVO3 =8)
      parameter (ILENTP =ILEVH+ILEVC+ILEVSI+ILEVHE1+ILEVHE2+ILEVAL+
     $                   ILEVMG+ILEVFE+ILEVNA+ILEVCA+ILEVO+ILEVS+
     $                   ILEVO2+ILEVO3)
C
C
C
C
C---------------------------- for MANAGER
      data IBSCR,IZOQ /LACK, 269*LACK/
C
C---------------------------- for MINIGER
      data JBSCR,JZOQ /LACK, 17*LACK/
C
C---------------------------- for BERTH
      data LSHF,LADR,ISHF /LACK, 0, 7*LACK/
C
C---------------------------- for COBLOCK
      data MIKLEN,KKK /LACK, NKKK*LACK/
C
C---------------------------- for POPDATA
      data NPOPS /NPI/
      data LIMPOP /ILEVH, ILEVC, ILEVSI, ILEVHE1, ILEVHE2, ILEVAL,
     $             ILEVMG, ILEVFE, ILEVNA, ILEVCA, ILEVO, ILEVS,
     $             ILEVO2, ILEVO3/
      data MAXPOPL /15/
      data LENT /111/
      data IUPOP /NPI*0/
      data LENPOP /NPI*0/
      data NAMES /'HYDROGEN', 'CARBON', 'SILICON', 'HELIUM',
     $            'HELIUM2', 'ALUMINUM', 'MAGNESIUM', 'IRON',
     $            'SODIUM', 'CALCIUM', 'OXYGEN', 'SULFUR',
     $            'OXYGEN2', 'OXYGEN3'/
      data TNAMES /'HYDROGEN', 'CARBON', 'SILICON', 'HELIUM',
     $             'HELIUM2', 'ALUMINUM', 'MAGNESIU', 'IRON',
     $             'SODIUM', 'CALCIUM', 'OXYGEN', 'SULFUR',
     $             'OXYGEN2', 'OXYGEN3'/
      data NAMKNT /8, 6, 7, 6, 7, 8, 9, 4, 6, 7, 6, 6, 7, 7/
      data POPSYM /'H', 'C', 'SI', 'HE', 'HE2', 'AL', 'MG', 'FE',
     $             'NA', 'CA', 'O', 'S', 'O2', 'O3'/
      data KAPNO /5, 10, 7, 20, 21, 12, 8, 17, 18, 19, 38, 6,
     $            42, 43/
      data MRTPM /50/
      data MRTPA,MRTP /0, 50*0/
      data ICKSM /9, 18, 21, 12, 15, 24, 27, 30, 33, 36, 40, 44,
     $            49, 52/
      data POPMSS /1.008D0, 12.0111D0, 28.086D0, 4.0026D0, 4.0026D0,
     $             26.9815D0, 24.305D0, 55.847D0, 22.9898D0, 40.08D0,
     $             15.999D0, 32.06D0, 15.999D0, 15.999D0/
      data KLABPI /'NP      ', 'CK      ', 'SIK     ', 'HEK     ',
     $             'HE2K    ', 'ALK     ', 'MGK     ', 'FEK     ',
     $             'NAK     ', 'CAK     ', 'OK      ', 'SK      ',
     $             'O2K     ', 'O3K     '/
      data NLABPI /'HN      ', 'CN      ', 'SIN     ', 'HEN     ',
     $             'HE2N    ', 'ALN     ', 'MGN     ', 'FEN     ',
     $             'NAN     ', 'CAN     ', 'ON      ', 'SN      ',
     $             'O2N     ', 'O3N     '/
      data BLABPI /'BDH     ', 'BDC     ', 'BDSI    ', 'BDHE    ',
     $             'BDHE2   ', 'BDAL    ', 'BDMG    ', 'BDFE    ',
     $             'BDNA    ', 'BDCA    ', 'BDO     ', 'BDS     ',
     $             'BDO2    ', 'BDO3    '/
C
C---------------------------- for ELIZA
      data LI1LEN,MML /LACK, 67*LACK/
      data LI2LEN,MMP /LACK, 7*LACK/
      data LI3LEN,MMT /LACK, 19*LACK/
C
C---------------------------- for PERBLOC
      data LPDLEN,LPD /LACK, 15*LACK/
C
C---------------------------- for ORIBLOC
      data LODLEN,LOD /LACK, 17*LACK/
C
C---------------------------- for LYSTER
      data NLL,LENLYB,ILB /NLLY, LACK, 18*LACK/
      data INIHLL,INDPTH  /2*.false./
C
C---------------------------- for COUNTS
      integer N  , NL , M  , KF
      data    N  , NL , M  , KF  /0, 2, 0, 0/
      integer NT , NSW, L  , NLB
      data    NT , NSW, L  , NLB /0, 0, 0, 0/
      integer NFB, KK , KBX, KKX
      data    NFB, KK , KBX, KKX /6, 0, 0, 0/
      integer LZM, NZM, MRR, NFL
      data    LZM, NZM, MRR, NFL /0, 0, 0, 0/
      integer NWV, MMR, LF , NTE
      data    NWV, MMR, LF , NTE /0, 0, 0, 0/
      integer NDT, MHM, NWS, JM
      data    NDT, MHM, NWS, JM  /0, 0, 0, 0/
      integer KNW, LDU, LLY, MLR
      data    KNW, LDU, LLY, MLR /53, 0, 0, 0/
      integer MRS, MRX, NCR
      data    MRS, MRX, NCR      /0, 0, 0/
      integer MLS, LG , INK, KS
      data    MLS, LG , INK, KS  /0, 0, 0, 0/
      integer KR , KB , MQT, NSL
      data    KR , KB , MQT, NSL /0, 0, 0, 2/
      integer NDR, NVX, NDV, NCP
      data    NDR, NVX, NDV, NCP /0, 0, 0, 0/
      integer NAB, KWC, NVF, NXF
      data    NAB, KWC, NVF, NXF /0, 0, 30, 1000/
      integer KM , NKA, NCL, NLN
      data    KM , NKA, NCL, NLN /0, 2, 0, 0/
      integer NCQ, NVH, NCB, NZE
      data    NCQ, NVH, NCB, NZE /0, 0, 0, 0/
      integer KWA, NGM
      data    KWA, NGM           /0, 0/
C
C---------------------------- for LUNITS
      integer LUIN,LUMO,LUAT,LURE,LURR,LUMR,LUPR,LURO,LUEO,LUDO,
     $        LURS,LUCA,LUPD,LUSO,LUSM,LUCR,LUNC,LUKU,LUJO,LUIS,
     $        LUMA,LUJI,LUCM,LUIX,LUCS,LUGI,LUKA,LUHB,LUSD,LUSF,
     $        LUWM
      data LUMA /01/
      data LUGI /02/
      data LUIN /03/
      data LUMO /04/
      data LUAT /07/
      data LURE /08/
      data LUJI /09/
      data LUKU /10/
      data LUCM /11/
      data LUKA /12/
      data LURO /15/
      data LUEO /15/
      data LURS /15/
      data LUDO /18/
      data LURR /19/
      data LUMR /20/
      data LUPR /21/
      data LUJO /22/
      data LUSO /23/
      data LUCR /24/
      data LUNC /25/
      data LUSM /26/
      data LUSF /27/
      data LUPD /28/
      data LUCA /29/
      data LUIS /30/
      data LUIX /31/
      data LUCS /32/
      data LUWM /97/
      data LUSD /98/
      data LUHB /99/
C
C---------------------------- for DLIT
C     DLITINIT    as of 1984 Apr 24
      data        DLIT /
     $ 0.D0, 1.D0, 2.D0, 3.D0, 4.D0, 5.D0, 6.D0, 7.D0, 8.D0, 9.D0,
     $ 1.D1, 5.D-1, 3.3333333333333333D-1, 2.5D-1, 2.D-1,
     $ 1.666666666666667D-1, 6.666666666666667D-1, 1.25D-1, 1.D-1 /
C     .
C
C---------------------------- for SYMBS
C---- SYMBINIT    as of 1984 Apr 24
      data        SYMBS/
     $ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
     $ 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
     $ 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     $ '$', '#', '+', '-', '/', '.', ' ', '=', '*', '%', '[', ']',
     $ '(', ')', '<', '>'/
C     .
C
C---------------------------- for ULTIMA
C---- ULTINIT     as of 2004 Mar 09
      data        ZZLARGE /1.D+300/
      data        ZZSMALL /1.D-300/
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C
C---------------------------- for ALTIMA
      data ZZLALT, ZZSALT /1.D+200, 1.D-200/
C
C---------------------------- for ESPY
      data ESPION /.false./
C
C---------------------------- for EXPLIM
      data EXPLIM /7.D2/
C
C---------------------------- for ROMAN
      data ROMAN /
     $ 'I    ', 'II   ', 'III  ', 'IV   ', 'V    ', 'VI   ', 'VII  ',
     $ 'VIII ', 'IX   ', 'X    ', 'XI   ', 'XII  ', 'XIII ', 'XIV  ',
     $ 'XV   ', 'XVI  ', 'XVII ', 'XVIII', 'XIX  ', 'XX   ', 'LARGE'/
C
C---------------------------- for ARGUS
      real*8 ADS  , WZ   , RWKSI
      data   ADS  , WZ   , RWKSI /0.D0, 5.D-1, 0.D0/
      real*8 AMASS, PART , ABD
      data   AMASS, PART , ABD   /-3.17D0, -3.17D0, -3.17D0/
      real*8 PW   , Y    , XNUK
      data   PW   , Y    , XNUK  /1.D0, 5.D-1, 0.D0/
      real*8 CWR  , CHOP , EXLYM
      data   CWR  , CHOP , EXLYM /1.D-1, 1.D0, 1.D1/
      real*8 TGLYM, HSEC , YH
      data   TGLYM, HSEC , YH    /1.D2, 1.D0, 0.D0/
      real*8 CGR  , DLU  , TX
      data   CGR  , DLU  , TX    /0.D0, 1.D0, 0.D0/
      real*8 YL   , YPRE , TSM
      data   YL   , YPRE , TSM   /0.D0, 0.D0, 1.D-4/
      real*8 DRLIM, R1N  , TBAR
      data   DRLIM, R1N  , TBAR  /1.D-2, 0.D0, 5.D-1/
      real*8 WPOP , XLMT , XLME
      data   WPOP , XLMT , XLME  /-1.D0, 3.D-1, 1.D-4/
      real*8 XKDST, TDUST, YFLUX
      data   XKDST, TDUST, YFLUX /0.D0, 2.D2, 5.D-1/
      real*8 HEL  , XLMZ , XLMF
      data   HEL  , XLMZ , XLMF  /1.D0, 2.5D3, 1.D-5/
      real*8 WSM  , XLMA , XLMB
      data   WSM  , XLMA , XLMB  /0.D0, 3.D-1, 1.D4/
      real*8 XLMR , TLIMG, BLIMG
      data   XLMR , TLIMG, BLIMG /1.D4, 0.D0, -1.301D0/
      real*8 WEP  , OPF  , RFAC
      data   WEP  , OPF  , RFAC  /1.D0, 1.D0, 1.D0/
      real*8 WMN  , TMS  , WBD
      data   WMN  , TMS  , WBD   /1.D-1, 5.D0, -1.D0/
      real*8 RCCFE, PRTLM, CURMI
      data   RCCFE, PRTLM, CURMI /1.D-1, 3.D0, 1.682D3/
      real*8 CURMA, WTD  , TLTR
      data   CURMA, WTD  , TLTR  /9.D3, 1.D0, 1.3D0/
      real*8 XCOMX, DDT  , XINCH
      data   XCOMX, DDT  , XINCH /3.D0, 1.D-2, 1.D-1/
      real*8 WMX  , SMP  , EIDIF
      data   WMX  , SMP  , EIDIF /7.D-1, 3.D-1, 1.D-4/
      real*8 CUTFE, WZM  , RFMAS
      data   CUTFE, WZM  , RFMAS /1.D-8, 8.D-1, 0.D0/
      real*8 FABD , EPCBR, TSMLL
      data   FABD , EPCBR, TSMLL /1.D0, 0.D0, 1.D-10/
      real*8 TLRGE, CVXM , PRDCV
      data   TLRGE, CVXM , PRDCV /2.D2, 0.D0, 1.D-1/
      real*8 CVXF , WFB  , CSFCT
      data   CVXF , WFB  , CSFCT /2.5D1, 6.D-1, 1.D-5/
      real*8 CLNH , HTAU , PZERO
      data   CLNH , HTAU , PZERO /2.D0, 1.D0, 0.D0/
      real*8 TML  , DELTB, CEQMX
      data   TML  , DELTB, CEQMX /3.D1, 1.D-1, 1.D6/
      real*8 SMATC, ELLED
      data   SMATC, ELLED /0.D0, 2.4D-11/
      real*8 EMXED, VMNFE, XQMAX
      data   EMXED, VMNFE, XQMAX /1.D-6, 3.D-3, 2.D2/
      real*8 DQMIN, DQMAX, XJFE
      data   DQMIN, DQMAX, XJFE  /1.D-2, 2.D0, 2.D-20/
      real*8 VSMLL, CPRSS, WPRSS
      data   VSMLL, CPRSS, WPRSS /1.D-100, 0.D0, 1.D0/
      real*8 FZLIM, CLOGG, DZMSS
      data   FZLIM, CLOGG, DZMSS /1.5D0, 0.D0, 1.D-2/
      real*8 REFLM, SBFEQ, SBDMX
      data   REFLM, SBFEQ, SBDMX /0.D0, 1.D-3, 1.D-2/
      real*8 SBDMN, ADMAS, CHLIM
      data   SBDMN, ADMAS, CHLIM /1.D-3, 0.D0, 5.D-1/
      real*8 YCOL , XLCOD, CCHX
      data   YCOL , XLCOD, CCHX  /0.D0, 0.D0, 1.D0/
      real*8 CHEFL, CQM  , XCL
      data   CHEFL, CQM  , XCL   /0.D0, 0.D0, 3.5D0/
      real*8 TAUCL, CVXS , XMCOA
      data   TAUCL, CVXS , XMCOA /1.D4, 0.D0, 1.D0/
      real*8 VOITC, ZNDW , WNJNK
      data   VOITC, ZNDW , WNJNK /1.D-6, 0.D0, 0.D0/
      real*8 WBDIR, FBVMX, ASMCR
      data   WBDIR, FBVMX, ASMCR /1.D0, 1.D2, 1.D-3/
      real*8 ZXMIN, CFH  , CVSB
      data   ZXMIN, CFH  , CVSB  /1.D-1, 0.D0, 0.D0/
      real*8 HEABL, RFHEA, TRFLI
      data   HEABL, RFHEA, TRFLI /3.D0, 1.D0, 1.1D0/
      real*8 EPTAU, WNUK , HSBM
      data   EPTAU, WNUK , HSBM  /5.D0, 0.D0, 2.D1/
      real*8 HSBMN, HSBMX, HSBFQ
      data   HSBMN, HSBMX, HSBFQ /1.D-5, 1.D3, 1.D-1/
      real*8 PMSK , CEDMN, CEDMX
      data   PMSK , CEDMN, CEDMX /1.D0, 1.D-6, 1.D3/
      real*8 CEFEQ, FZION, FROSC
      data   CEFEQ, FZION, FROSC /1.D-2, 1.D0, 1.D-2/
      real*8 FSTKM, FRCDL, FMCDL
      data   FSTKM, FRCDL, FMCDL /1.D0, 1.D-2, 1.D-1/
      real*8 CSDW , CLVLS, WAVMN
      data   CSDW , CLVLS, WAVMN /1.D0, 2.D0, 0.D0/
      real*8 WAVMX, CVZ  , CDZ
      data   WAVMX, CVZ  , CDZ   /0.D0, 1.D300, 1.D0/
      real*8 COMU , BMWAC, SRCO
      data   COMU , BMWAC, SRCO  /0.D0, 1.D-1, 1.D-1/
      real*8 ALOMI, ALOMA, HNAJL
      data   ALOMI, ALOMA, HNAJL /0.D0, 0.D0, 1.D20/
      real*8 CORMN, CORMX, FMVLM
      data   CORMN, CORMX, FMVLM /-1.D0, -1.D0, 1.D-4/
      real*8 RCOMN, CWJ  , PNH
      data   RCOMN, CWJ  , PNH   /1.D-10, 5.D-1, 0.D0/
      real*8 CLM  , CFHE , SN1CC
      data   CLM  , CFHE , SN1CC /1.D0, 0.D0, 1.D-8/
      real*8 XLMD3, WSN1D, AOWXP
      data   XLMD3, WSN1D, AOWXP /1.D-2, 1.D0, 0.D0/
      real*8 CTCO , CTMX , SHCOP
      data   CTCO , CTMX , SHCOP /0.D0, 2.D-1, 4.D2/
      real*8 ZRCO , SHCOC, XLMXC
      data   ZRCO , SHCOC, XLMXC /-5.D2, 1.D2, 3.D0/
      real*8 XLMXP, XLMD2, XLMCR
      data   XLMXP, XLMD2, XLMCR /2.D0, 1.D-2, 8.5D1/
      real*8 WRTMN, WRTMX, YRATS
      data   WRTMN, WRTMX, YRATS /1.D2, 2.D4, 0.D0/
      real*8 XLMH , SCVA , SCVS
      data   XLMH , SCVA , SCVS  /9.5D2, 1.D1, 5.D2/
      real*8 SCTA , SCTS , SCVB
      data   SCTA , SCTS , SCVB  /2.D2, 1.D2, 1.D1/
      real*8 SCPS , CN1S , DELLM
      data   SCPS , CN1S , DELLM /5.D2, 1.D-2, 1.D-6/
C
      integer NDW  , MS   , NS
      data    NDW  , MS   , NS    /-1, 0, 0/
      integer ISUB , IDFSW, IRLS1
      data    ISUB , IDFSW, IRLS1 /1, 0, 1/
      integer JSTIN, IOMX , IRLSN
      data    JSTIN, IOMX , IRLSN /0, 1, 1/
      integer JBD  , IXSTA, JRHO
      data    JBD  , IXSTA, JRHO  /0, 1, 1/
      integer ILI  , NIL  , MFONT
      data    ILI  , NIL  , MFONT /0, 2, 1/
      integer LNLIM, ITOPE, IPEX
      data    LNLIM, ITOPE, IPEX  /8, 0, 0/
      integer LYMIT, IHSLT, NGRL
      data    LYMIT, IHSLT, NGRL  /1, 1, 0/
      integer NGRR , INFSM, INLSM
      data    NGRR , INFSM, INLSM /0, 0, 0/
      integer METEP, KNFRM, KURIN
      data    METEP, KNFRM, KURIN /3, 2, 0/
      integer KINMX, KININ, MDTR1
      data    KINMX, KININ, MDTR1 /0, 5, 10/
      integer MDTR2, KKPR , KOLEV
      data    MDTR2, KKPR , KOLEV /20, 0, 1/
      integer JNUNC, JSTCN, IZOPT
      data    JNUNC, JSTCN, IZOPT /0, 0, 1/
      integer JZOPT, IVOIT, NVOIT
      data    JZOPT, IVOIT, NVOIT /0, 1, 1/
      integer JBDNC, IMUCD, M304
      data    JBDNC, IMUCD, M304  /0, 1, 10/
      integer LSTMP, NCOSW, NTAN
      data    LSTMP, NCOSW, NTAN  /0, 1, 4/
      integer JBFSW, IHEDF, LDINT
      data    JBFSW, IHEDF, LDINT /1, 0, 5/
      integer LDTYP, KUDNT, JH1
      data    LDTYP, KUDNT, JH1   /1, 0, 0/
      integer JH2  , ISRCD, IDRCD
      data    JH2  , ISRCD, IDRCD /0, 1, 1/
      integer NHTSW, IONST, IPR01
      data    NHTSW, IONST, IPR01 /2, 1, 1/
      integer IPR02, IPR03, IPR04
      data    IPR02, IPR03, IPR04 /5, 10, 15/
      integer MSKIP, IPRFA, IRPUN
      data    MSKIP, IPRFA, IRPUN /0, 1, 1/
      integer IDEX , KALOR, MCON
      data    IDEX , KALOR, MCON  /10, 1, 0/
      integer NIASM, KOMNV, KOMNP
      data    NIASM, KOMNV, KOMNP /20, 0, 0/
      integer KOMNT, KODNT, JHEAS
      data    KOMNT, KODNT, JHEAS /0, 0, 0/
      integer MAMAS, ISNUD, IDWIN
      data    MAMAS, ISNUD, IDWIN /0, 0, 0/
      integer ITRFI, NSPED, IFXDS
      data    ITRFI, NSPED, IFXDS /0, 1, 0/
      integer NVDFE, NNDFE, NZDFE
      data    NVDFE, NNDFE, NZDFE /-1, -1, -1/
      integer IPZER, IHEAB, IHDMP
      data    IPZER, IHEAB, IHDMP /0, 0, 0/
      integer IRTIS, KOOLS, MH2N
      data    IRTIS, KOOLS, MH2N  /2, 0, 0/
      integer ISCRS, LYODS, KDIAG
      data    ISCRS, LYODS, KDIAG /0, 0, 5/
      integer N1MET, ISMBD, IRUNT
      data    N1MET, ISMBD, IRUNT /2, 0, 0/
      integer NOION, NERM , KARB
      data    NOION, NERM , KARB  /0, 10, 1/
      integer ISOD , IPRDD, IPRDF
      data    ISOD , IPRDD, IPRDF /0, 1, 1/
      integer IPPOD, MN1  , IDRDP
      data    IPPOD, MN1  , IDRDP /0, 0, 0/
      integer KDRDP, KAPDB, MTHEI
      data    KDRDP, KAPDB, MTHEI /0, 0, 1/
      integer MDFV , NCOPT, LSFGC
      data    MDFV , NCOPT, LSFGC /1, 0, 1/
      integer NODCG, MNG1 , IDFDM
      data    NODCG, MNG1 , IDFDM /-1, 0, 1/
      integer IDFDI, ISNDD, IDEDP
      data    IDFDI, ISNDD, IDEDP /0, 0, 0/
      integer IBRDP, ICXDP, NSPRD
      data    IBRDP, ICXDP, NSPRD /0, 0, 0/
      integer ICHDP, ISMSW, ICDIT
      data    ICHDP, ISMSW, ICDIT /0, 0, 1/
      integer IRATE, ICHSW, IHSSW
      data    IRATE, ICHSW, IHSSW /0, 1, 0/
      integer IHSDP, IHSDD, IHSKM
      data    IHSDP, IHSDD, IHSKM /0, 0, 100/
      integer IHSSM, NDWM,  LHHSE
      data    IHSSM, NDWM,  LHHSE /2000, -1, 1/
      integer LX2DS, LX3DS, KMMAX
      data    LX2DS, LX3DS, KMMAX /0, 0, 0/
      integer ISTRK, NBS  , NANA1
      data    ISTRK, NBS  , NANA1 /0, 2, 1/
      integer IHSSP, NANA2, LOGAS
      data    IHSSP, NANA2, LOGAS /0, 5, 0/
      integer NECLP, NARB , KAVNT
      data    NECLP, NARB , KAVNT /10, 0, 0/
      integer KAVNP, KAVNZ, IXNCS
      data    KAVNP, KAVNZ, IXNCS /0, 0, 0/
      integer IRFNC, JDMCI, JDMCE
      data    IRFNC, JDMCI, JDMCE /0, 0, 0/
      integer IDNRT, JHBFD, MOPRN
      data    IDNRT, JHBFD, MOPRN /1, 0, 1/
      integer IWSMD, LWNT , KDFD1
      data    IWSMD, LWNT , KDFD1 /0, 1, 1/
      integer KDFGS, KDFGA, KDFGB
      data    KDFGS, KDFGA, KDFGB /0, -1, -1/
      integer ITN1R, LODCG, I4DFM
      data    ITN1R, LODCG, I4DFM /10, -1, 1/
      integer I4DEQ, I4DIO, MSSPR
      data    I4DEQ, I4DIO, MSSPR /0, 1, 1/
      integer NEFDF, IPDIJ, IPDEE
      data    NEFDF, IPDIJ, IPDEE /1, 0, 0/
      integer IBTSW, KB1WA, KB1WB
      data    IBTSW, KB1WA, KB1WB /0, 0, 0/
      integer KDAMP, KBNDS, N1NUP
      data    KDAMP, KBNDS, N1NUP /0, 1, 1/
      integer MDFG , INDRN, MKURU
      data    MDFG , INDRN, MKURU /1, 1, 1/
      integer KHFFS, MTREF, KATNU
      data    KHFFS, MTREF, KATNU /1, 0, 0/
      integer ISCMP, KB1WS, KOELS
      data    ISCMP, KB1WS, KOELS /1, 2, 1/
      integer JATOM, JATMO, IGMSW
      data    JATOM, JATMO, IGMSW /0, 0, 0/
      integer NLY  , KCOAA, IWEIT
      data    NLY  , KCOAA, IWEIT /15, 0, 0/
      integer IFALL, NQLYM, JXNCS
      data    IFALL, NQLYM, JXNCS /1, 0, 0/
      integer JHLSK, NMLR,  JSSV
      data    JHLSK, NMLR,  JSSV  /1, 0, 0/
      integer JNEDP, JEDIT, MBREC
      data    JNEDP, JEDIT, MBREC /0, 0, 1/
      integer JATAW, IBNVW, NDSN1
      data    JATAW, IBNVW, NDSN1 /0, 0, 0/
      integer IXASM, MXPPI, MXTAP
      data    IXASM, MXPPI, MXTAP /0, 5, 100/
      integer ITKZA, IDFDS, LDFD1
      data    ITKZA, IDFDS, LDFD1 /1, 1, 0/
      integer NGNV , IPIJG, ISMVE
      data    NGNV , IPIJG, ISMVE /0, 1, 1/
      integer JSFEX, LOXDS, IORIC
      data    JSFEX, LOXDS, IORIC /1, 0, 1/
      integer LHEDS, IGII
      data    LHEDS, IGII  /0, 1/
      integer ITPRD, LEEDS, KXLYM
      data    ITPRD, LEEDS, KXLYM /4, 0, 0/
      integer LPVEL, LPMLR, KLDIN
      data    LPVEL, LPMLR, KLDIN /1, 1, 1/
      integer KLFIN, MSEDG, MSEDW
      data    KLFIN, MSEDG, MSEDW /1, 1, 3/
      integer MCEOF, MCIOF, INCEI
      data    MCEOF, MCIOF, INCEI /0, 0, 0/
      integer ICIH1
      data    ICIH1 /1/
C
      character QNAME*8, QELSM*8, QMODL*8
      data      QNAME,   QELSM,   QMODL /'NONAME', 'ZZ', '!NONAME!'/
      character QATOM*8, QALHD*8
      data      QATOM,   QALHD          /'!NONAME!', ' '/
C
C---------------------------- for SELGI
      data INF1,INF2 /KNFMX*0, KNFMX*0/
      data INF3,INF4 /KNFMX*0, KNFMX*0/
      data KNTF /0/
      data FUJJ,FVAL /KNFMX*0.D0, KNFMX*0.D0/
C
C---------------------------- for OPTIONS
      data IQQ /NOOPT*LACK/
C
C     (0: off, 1: on.)
      data      IQD /
C      001-020
     $ 0,0,0,1,1, 0,0,0,1,0,  0,0,0,0,0, 0,0,1,0,0,
C      021-040
     $ 0,0,0,0,0, 0,0,0,1,0,  0,1,0,1,1, 1,1,0,0,0,
C      041-060
     $ 1,1,0,1,1, 1,1,0,0,0,  0,0,0,0,0, 1,0,0,0,0,
C      061-080
     $ 1,0,0,0,0, 0,0,0,0,0,  0,0,0,0,0, 1,1,0,1,0,
C      081-100
     $ 0,0,1,0,0, 0,0,1,0,0,  0,0,0,0,1, 0,0,0,0,0,
C      101-120
     $ 0,0,0,0,0, 0,0,1,0,0,  0,1,1,0,0, 0,0,0,1,0,
C      121-140
     $ 0,0,1,1,0, 0,1,1,0,0,  0,0,0,0,0, 1,0,0,0,1,
C      141-160
     $ 0,0,0,1,0, 0,0,0,1,0,  0,0,0,0,1, 0,0,0,0,0,
C      161-180
     $ 0,1,1,0,1, 0,0,1,0,0,  0,0,0,0,0, 0,0,0,0,0,
C      181-200
     $ 0,0,0,1,0, 0,0,0,1,0,  0,0,0,1,0, 1,0,0,1,0,
C      201-220
     $ 1,0,0,1,0, 0,0,0,0,0,  0,1,1,0,0, 1,1,0,0,0,
C      221-240
     $ 0,0,0,0,0, 0,1,1,1,1,  0,1,1,1,1, 1,0,0,1,0,
C      241-260
     $ 1,0,0,1,0, 0,1,1,1,1,  1,1,1,0,0, 0,0,0,0,0,
C      261-280
     $ 0,1,0,1,0, 0,1,0,0,1,  0,1,1,1,0, 1,0,0,0,1,
C      281-300
     $ 0,1,1,1,1, 0,1,0,1,0,  0,0,0,0,0, 0,1,1,1,0,
C      301-320
     $ 0,1,0,1,0, 0,0,1,0,0,  0,1,0,0,0, 1,1,0,1,0,
C      321-340
     $ 0,0,0,1,1, 0,0,0,1,0,  0,1,0,0,0, 0,1,0,1,1,
C      341-360
     $ 0,1,0,0,0,1/
C
      data      IQT /
C      001-020
     $ 1,1,4,1,2, 2,1,2,2,3,  1,1,2,2,4, 2,1,1,1,1,
C      021-040
     $ 1,4,1,1,1, 1,1,1,1,4,  2,3,2,1,2, 1,1,2,4,4,
C      041-060
     $ 3,1,4,1,1, 2,1,1,2,2,  2,1,2,2,2, 2,4,4,1,2,
C      061-080
     $ 2,2,1,4,1, 1,1,2,1,1,  2,2,1,1,1, 1,3,2,2,2,
C      081-100
     $ 1,1,2,2,1, 4,1,1,1,1,  1,1,1,1,1, 1,3,1,2,1,
C      101-120
     $ 1,4,1,1,1, 2,1,1,1,1,  1,1,1,1,1, 4,1,1,4,1,
C      121-140
     $ 4,2,2,2,1, 1,1,1,4,4,  1,4,2,3,2, 1,4,4,4,2,
C      141-160
     $ 2,4,4,3,2, 4,2,1,2,2,  3,3,4,2,1, 2,1,2,4,3,
C      161-180
     $ 2,1,2,1,2, 2,1,2,2,1,  1,2,2,4,1, 4,2,2,4,1,
C      181-200
     $ 1,1,3,2,2, 3,1,1,1,1,  3,3,4,2,4, 2,4,4,1,1,
C      201-220
     $ 1,4,4,2,1, 4,3,2,1,1,  4,2,1,2,3, 1,1,4,2,4,
C      221-240
     $ 2,4,4,1,2, 4,4,1,4,2,  2,3,2,3,1, 1,2,1,1,3,
C      241-260
     $ 3,4,3,2,2, 4,1,1,1,1,  1,1,1,1,1, 1,1,1,1,1,
C      261-280
     $ 1,2,2,1,2, 2,1,4,4,2,  2,2,2,4,2, 1,4,1,1,1,
C      281-300
     $ 2,1,1,1,1, 1,1,1,1,1,  1,1,3,3,1, 1,3,3,2,1,
C      301-320
     $ 1,1,2,2,2, 2,2,1,1,2,  4,2,2,4,1, 3,3,3,2,3,
C      321-340
     $ 2,1,1,1,1, 2,1,2,1,2,  2,2,2,2,2, 1,2,1,2,2,
C      341-360
     $ 1,1,3,2,1,2/
C
      data (ONAME(I),I=1,50)/
     $ 'ANALYSIS', 'SCALE', 'SEBUG', 'LTEDATA', 'PHASE2',
     $ 'ECLIPSE', 'EPCOMP', 'SPHOUT', 'LIGHT', 'TRANSAV',
     $ 'ADDCOPR', 'OPAPRNT', 'LYMAN', 'CSWITCH', 'ARHODMP',
     $ 'HSE', 'SETIME', 'SUMMARY', 'EVERY', 'KURPRNT',
     $ 'STANPRNT', 'FELEDMP', 'STIMPRNT', 'RATEPRNT', 'DPDWPRNT',
     $ 'TAUPRNT', 'CARPRNT', 'BDPRNT', 'POPPRNT', 'DRDMP',
     $ 'SPHERE', 'VTV', 'LTE', 'POPGRAF', 'NHADJ',
     $ 'HNPRNT', 'APHICOPR', 'ENHANCE', 'CSFDMP', 'ECLIDMP',
     $ 'INSCARD', 'PASSPRNT', 'PRODMP', 'INTGRAF', 'CSFPRNT',
     $ 'CSF', 'CHKGRAF', 'HELPRNT', 'FINITE', 'REFLECT'/
      data (ONAME(I),I=51,100)/
     $ 'INCIDNT', 'RATECOPR', 'USETRIN', 'MONOTAU', 'EMERINT',
     $ 'NESWICH', 'LINTDMP', 'EMINDMP', 'OPASUM', 'GDS',
     $ 'ILRT', 'RSQUARE', 'OPAGRAF', 'TAUDMP', 'LINECOPR',
     $ 'EMIPRNT', 'ELECPRNT', 'HMS', 'HMSCOPR', 'PRDPRNT',
     $ 'ENL', 'ENL2', 'PRDCOPR', 'SILPRNT', 'SEPRNT',
     $ 'STANDARD', 'USEWTAB', 'INBED', 'PTN', 'CONFLUX',
     $ 'FELEPRNT', 'WTABPRNT', 'SLYR', 'COLTEMP', 'APHIPRNT',
     $ 'GDSDMP', 'INDPRNT', 'INTRPRNT', 'LSCALE', 'HEL2PRNT',
     $ 'ALUPRNT', 'EMIGRAF', 'CSFGRAF', 'ALL', 'RATEGRAF',
     $ 'SPECSUM', 'CONSAV', 'MAGPRNT', 'DUSTYPE', 'DUSTCOPR'/
      data (ONAME(I),I=101,150)/
     $ 'ORIGIN', 'FLUXDMP', 'SECOMP', 'EMISUM', 'TAUSUM',
     $ 'RHOFUDGE', 'ALLY', 'RATEFULL', 'ITERS', 'ITERRHO',
     $ 'ITERTAU', 'ITERRK', 'ITERN', 'ITERRWT', 'LYMCOPR',
     $ 'LYMDMP', 'STATPRNT', 'NBPRNT', 'GDMP', 'RHBPRDT',
     $ 'EPDMP', 'EPSNEDC', 'WATESTR', 'WATESTE', 'SULPRNT',
     $ 'COMPRK', 'ITERNE', 'ITERB', 'LNUMDMP', 'BDMP',
     $ 'FEPRNT', 'HSEDMP', 'CALCOOL', 'COOLSAV', 'ORT',
     $ 'ITERZ', 'PERDMP1', 'PERDMP2', 'HFFCOOLD', 'COOLINT',
     $ 'INCIFRNT', 'ITDMP', 'CNFLXDMP', 'IVALICK', 'SEDITIF',
     $ 'WNDMP', 'RSMOOTH', 'SQSMPRNT', 'SEDIT', 'RHEDIT'/
      data (ONAME(I),I=151,200)/
     $ 'PROSAV', 'JNTRPOL', 'BLENDMP', 'USENCJ', 'RCOMPRNT',
     $ 'SPHETAU', 'SPHEGEOM', 'POPBSW', 'PERDMP0', 'ECLISAV',
     $ 'BEDIT', 'ECLIGRAF', 'TANG', 'PARTPRNT', 'PARTVAR',
     $ 'CSFB', 'JNUPRNT', 'QSFEDIT', 'EXPAND', 'KOMPRNT',
     $ 'COMPCOPR', 'SNUSHFT', 'VSWITCH', 'INDXDMP', 'PED',
     $ 'PEDDMP', 'FELEC', 'FELE', 'PDCHECK', 'RATEALL',
     $ 'KSHLCOPR', 'HMSJPRNT', 'HMSONLY', 'CPSW', 'EPSW',
     $ 'METSW', 'METPRNT', 'LFDPRNT', 'RIJPRNT', 'ORSHORT',
     $ 'FLUXSAV', 'SPECSAV', 'LONGNBM', 'COOLCOM', 'COMCRID',
     $ 'COOLXRAY', 'XRAYCRID', 'KROSSID', 'RHBPRNT', 'FDBCOPR'/
      data (ONAME(I),I=201,250)/
     $ 'ITERNH', 'FDBDMP', 'SOBDMP', 'RHOWOPT', 'MITPRNT',
     $ 'PERDMP3', 'WISFILE', 'EMERBACK', 'SODPRNT', 'CALPRNT',
     $ 'DUSTDMP', 'DUSTEMP', 'ITERTD', 'VESCAPE', 'JLYSAV',
     $ 'WAVEPRNT', 'MAKIX', 'SOBINT', 'AMDIFF', 'AMDDMP',
     $ 'VELGRAD', 'VELGDMP', 'CODMP', 'COCOPR', 'COOLCO',
     $ 'COCRID', 'STAUREDM', 'OPTPRNT', 'PEGTNALL', 'HENORM',
     $ 'CALHEAT', 'ISCRS', 'DOION', 'DIFFANA', 'NVOIT',
     $ 'IXSTA', 'JBDNC', 'OXY2PRNT', 'VLGPRNT', 'JSTIN',
     $ 'RABDAT', 'IRUNT', 'TOPE', 'GNVCALC', 'BDCALC',
     $ 'PESRJALL', 'ATMOPRNT', 'ATOMPRNT', 'INDAPRNT', 'LSFGRAF'/
      data (ONAME(I),I=251,300)/
     $ 'CHKPRNT', 'BDGRAF', 'TEGRAF', 'ACSFPRNT', 'ALYMPRNT',
     $ 'APOPPRNT', 'AHSEPRNT', 'AOPTPRNT', 'ALSFPRNT', 'AINDPRNT',
     $ 'APRFPRNT', 'OPANEG', 'SDIRECT', 'AMBPRNT', 'VELS',
     $ 'DSMOOTH', 'ADN1PRNT', 'ADN1DMP', 'SQSMDMP', 'HSEV',
     $ 'BSMOOTH', 'AMDN1', 'HBROAD', 'HSTSUMM', 'GTNSTIM',
     $ 'CHXPRNT', 'CHXDMP', 'CIJPRNT', 'PIJPRNT', 'TAUPLOT',
     $ 'HEABD', 'COLHPRNT', 'SUMGRAF', 'SUMTREND', 'INTAPRNT',
     $ 'AINTPRNT', 'LSFPRNT', 'DIDHC', 'DIDHL', 'WAVENUMB',
     $ 'TRPRNT', 'ZPRNT', 'ATOMSAV', 'SLFSAV', 'SLFPRNT',
     $ 'SLFGRAF', 'INPEX', 'INPEXW', 'TRUECONT', 'TRUECOPR'/
      data (ONAME(I),I=301,NOOPT)/
     $ 'BDQPRDT', 'ITERCHK', 'AVCON', 'STKWATT', 'COCLIPSE',
     $ 'NEDIT', 'AVELOP', 'AVOPRNT', 'RATESUMM', 'SSMOOTH',
     $ 'BRATDMP', 'IRHWED', 'LINECOMP', 'LINECDMP', 'RHBPRSM',
     $ 'DELABORT', 'MCINPUT', 'INTEDIT', 'NRSMOOTH', 'COMOPAN',
     $ 'USETSM', 'STANCOPR', 'ITERCHI', 'INNBPRNT', 'PDETPRNT',
     $ 'ZCOMP', 'OXYPRNT', 'CEFACTS', 'PROCPRNT', 'CHEXLO',
     $ 'CHEXUP', 'GTNSMTH', 'CHEXLOL', 'RKINCR', 'FLWBROAD',
     $ 'FLWBPRNT', 'PRDMETH', 'PRDITER', 'CLNORM', 'ULNORM',
     $ 'LBDPRNT', 'LSFFULL', 'AEDIT', 'OPTHINL', 'OXY3PRNT',
     $ 'ALLCICE'/
C
C---------------------------- for MISC
      real*8  R1GD , XLCOW, YALYM
      data    R1GD , XLCOW, YALYM /0.D0, 0.D0, 0.D0/
      real*8  WTPZ , RFXNC, YAFUL
      data    WTPZ , RFXNC, YAFUL /0.D0, 0.D0, 0.D0/
      real*8  YASYM
      data    YASYM /0.D0/
C
      integer KSHEL, IOVER, ITER
      data    KSHEL, IOVER, ITER  /0, 0, 0/
      integer NSHL , MHL  , KPRSW
      data    NSHL , MHL  , KPRSW /0, 0, 0/
      integer NPT  , NRPMX, NH2CS
      data    NPT  , NRPMX, NH2CS /0, 0, 0/
      integer KZERO, KTKIN, LGGIN
      data    KZERO, KTKIN, LGGIN /0, 0, 0/
      integer J304I, NTK  , LFLX
      data    J304I, NTK  , LFLX  /0, 0, 0/
      integer MSFQR, MOMET, MSFQM
      data    MSFQR, MOMET, MSFQM /0, 0, 0/
      integer ITHSL, MSFRT, MSFGR
      data    ITHSL, MSFRT, MSFGR /0, 0, 0/
      integer JPOP , J304S, LITER
      data    JPOP , J304S, LITER /0, 0, 0/
      integer NOTX , KNZGM, NOTA
      data    NOTX , KNZGM, NOTA  /0, 0, 0/
      integer NXRW , NONC , KMASN
      data    NXRW , NONC , KMASN /0, 0, 0/
      integer KMUS , KAMB , LDLMX
      data    KMUS , KAMB , LDLMX /0, 0, 1/
      integer NEWZ , NLFDB, NVSB
      data    NEWZ , NLFDB, NVSB  /0, 0, 0/
      integer KVSB , KTRAS, KDGV
      data    KVSB , KTRAS, KDGV  /0, 0, 0/
      integer NCOW , KIMS , IVNH
      data    NCOW , KIMS , IVNH  /0, 0, 0/
      integer MLSFP, LCOW , NGDZ
      data    MLSFP, LCOW , NGDZ  /0, 0, 0/
      integer KGDT , KVLG , KCRH
      data    KGDT , KVLG , KCRH  /0, 0, 0/
      integer KDFA , NVSBP, JIBR
      data    KDFA , NVSBP, JIBR  /0, 0, 0/
      integer JDDL , LDLMU, MFMV
      data    JDDL , LDLMU, MFMV  /0, 1, 0/
      integer MCXK , KNEGA, KX2OK
      data    MCXK , KNEGA, KX2OK /0, 0, 1/
      integer KX3OK, JHEAB, KBTMX
      data    KX3OK, JHEAB, KBTMX /1, 0, 0/
      integer KRTMX, KSTMX, MPROM
      data    KRTMX, KSTMX, MPROM /0, 0, 0/
      integer IDGMZ, KTECH, KVXVA
      data    IDGMZ, KTECH, KVXVA /0, 0, 0/
      integer MWNSV, KZXST, KFCEU
      data    MWNSV, KZXST, KFCEU /0, 0, 0/
      integer KFELE, KCHIJ, KOXOK
      data    KFELE, KCHIJ, KOXOK /0, 0, 1/
      integer KHEOK, KALTG
      data    KHEOK, KALTG        /1, 0/
      integer KDZIN, KEEOK, IGKSW
      data    KDZIN, KEEOK, IGKSW /0, 1, 1/
      integer NCINM, KCHKI, KLYNF
      data    NCINM, KCHKI, KLYNF /7, 0, 0/
      integer KXNUC
      data    KXNUC /0/
C
      character*8 QIONM
      data        QIONM /'unknown'/
C
C---------------------------- for APOLLO
      data MEST /28*0/
C
C---------------------------- for MATRIX
      data PRNSW,EDJSW,KNTIN,KNTED /0, 0, 0, 0/
      data TIMIN,TIMED /0.D0, 0.D0/
      data CRITJ /1.D-50/
C
C---------------------------- for CHECKS
      data NCKSM /0/
C
C---------------------------- for IBIS
      data NIBIS /0/
      data NITS  /NTMX*0/
C
C---------------------------- for DICOM
      data NAPWRA,NAPWRB,NAPKNT /1, 2, 0/
      data APARAD,APETA,APCDP,APWRA,APWRB /5*0.D0/
      data APCI /20*0.D0/
      data APEI /20*0.D0/
C
C---------------------------- for XINGU
      data NAMXED( 1),BXED( 1),AXED( 1) /'HE ',4.3511D1,2.8000D-1/
      data NAMXED( 2),BXED( 2),AXED( 2) /'C  ',5.3578D1,5.7200D+0/
      data NAMXED( 3),BXED( 3),AXED( 3) /'N  ',5.4287D1,3.6600D+0/
      data NAMXED( 4),BXED( 4),AXED( 4) /'O  ',5.4790D1,2.4750D+0/
      data NAMXED( 5),BXED( 5),AXED( 5) /'NA ',5.5954D1,1.0000D+0/
      data NAMXED( 6),BXED( 6),AXED( 6) /'MG ',5.6066D1,3.4700D+1/
      data NAMXED( 7),BXED( 7),AXED( 7) /'AL ',5.6322D1,2.3936D+1/
      data NAMXED( 8),BXED( 8),AXED( 8) /'SI ',5.6596D1,1.9000D+1/
      data NAMXED( 9),BXED( 9),AXED( 9) /'S  ',5.6690D1,1.1300D+1/
      data NAMXED(10),BXED(10),AXED(10) /'CA ',5.7046D1,7.5000D+1/
C
C---------------------------- for BAMBI
C     (In subroutine TEAL)
C
C---------------------------- for JUAN
C     (In subroutine PEGEL)
C
C---------------------------- for LOOM
      data JCOROT  /2*JCOLMR/
      data KCOROT  /2*KCOLMR/
      data JCOFUN  /2*JCOLMF/
      data KCOFUN  /2*KCOLMF/
      data JCOOVR  /2*JCOLMO/
      data KCOOVR  /2*KCOLMO/
      data JCOSEC  /2*JCOLMS/
      data KCOSEC  /2*KCOLMS/
      data WVCOLO,WVCOHI /2.292D4, 8.941D4/
      data RC1213  /9.0D1/
      data ISOSLCT /3/
      data METHCOF /3/
      data METHCOW /3/
C
C---------------------------- for TABLET
      data KONWAL /KONLIM/
      data NUMKON,NUMTRU,NMKUSE /0,0,0/
C
C---------------------------- for ISOLA
      data WAVEDEL /1.D-14/
C
C---------------------------- for ARCHER
      data NNKODS /NNKOD*LACK/
C
C---------------------------- for INDEX
      data LIMIT,NULSIG /MILTI, -524288/
      data INC /MIL2*LACK/
C
C---------------------------- for THULE
      data LMTRA  /MXTRA/
      data NUMTRN /0/
      data NUMBLK /0/
      data LINNAM /MAXTR*LACK/
      data LI1ADR /MAXTR*0/
      data LI2ADR /MAXTR*0/
      data LI3ADR /MAXTR*0/
C
C---------------------------- for LIMBO
      data LIMTR  /MAXTR/
      data LINIU  /MAXTR*LACK/
      data LINIL  /MAXTR*LACK/
      data LINKLN /MAXTR*0/
      data LINPRD /MAXTR*0/
      data LINPRO /MAXTR*0/
      data LINMSE /MAXTR*LACK/
      data LINMSF /MAXTR*0/
      data LINDAM /MAXTR*15/
      data LININK /MAXTR*0/
      data LINTPS /MAXTR*0/
      data LININR /MAXTR*0/
      data LINFLX /MAXTR*0/
      data LINLDL /MAXTR*1/
      data LININT /MAXTR*0/
      data LINPRN /MAXTR*0/
      data LINFDB /MAXTR*0/
      data LINOML /MAXTR*0/
      data LINSBG /MAXTR*0/
      data LINKBT /MAXTR*0/
      data LINKRT /MAXTR*0/
      data LINKST /MAXTR*0/
C
C---------------------------- for LINUS
      data LINKDS /22*LACK/
C
C---------------------------- for PITCH
      data NTRP /0/
C
C---------------------------- for LEVDES
      data LIMLV,LEVDES /MILTY, MILTY*' '/
C
C---------------------------- for SHEEPY
      data TYME0,TYME /0.D0, LINKS*0.D0/
      data XNRW0,XNRW /0.D0, LINKS*0.D0/
      data XNRR0,XNRR /0.D0, LINKS*0.D0/
      data XNBW0,XNBW /0.D0, LINKS*0.D0/
      data XNBR0,XNBR /0.D0, LINKS*0.D0/
      data MEMSW  /LINKS*0/
      data MEMSIW /LINKS*0/
      data KALLS  /LINKS*0/
      data SECNAM( 1),SECTIT( 1)/'DORE',  'Read input, part 1         '/
      data SECNAM( 2),SECTIT( 2)/'TAY',   'Read input, part 2         '/
      data SECNAM( 3),SECTIT( 3)/'COQUET','Input printing             '/
      data SECNAM( 4),SECTIT( 4)/'SEVERN','Populations Initialization '/
      data SECNAM( 5),SECTIT( 5)/'TERN',  'Precalculations            '/
      data SECNAM( 6),SECTIT( 6)/'DART',  'Continuum Initialization   '/
      data SECNAM( 7),SECTIT( 7)/'OUSE',  'Continuum Calculations     '/
      data SECNAM( 8),SECTIT( 8)/'ALDE',  'Rates                      '/
      data SECNAM( 9),SECTIT( 9)/'BURE',  'H minus                    '/
      data SECNAM(10),SECTIT(10)/'DEE',   'Dust Temperature           '/
      data SECNAM(11),SECTIT(11)/'ORRIN', 'Damping Parameter          '/
      data SECNAM(12),SECTIT(12)/'REDE',  'B-ratios Initialization    '/
      data SECNAM(13),SECTIT(13)/'TAMAR', 'Line Source Functions      '/
      data SECNAM(14),SECTIT(14)/'TWEED', 'Lyman                      '/
      data SECNAM(15),SECTIT(15)/'AIRE',  'Hydrostatic Equilibrium    '/
      data SECNAM(16),SECTIT(16)/'CLYDE', 'NE Updating                '/
      data SECNAM(17),SECTIT(17)/'COLNE', 'Number Densities           '/
      data SECNAM(18),SECTIT(18)/'YARE',  'Restart Data               '/
      data SECNAM(19),SECTIT(19)/'HULL',  'Line Post-Processing       '/
      data SECNAM(20),SECTIT(20)/'THURSO','Continuum Summary          '/
      data SECNAM(21),SECTIT(21)/'BLYTH', 'Cooling and Heating Rates  '/
      data SECNAM(22),SECTIT(22)/'CARRON','Iterations Summaries       '/
      data SECNAM(23),SECTIT(23)/'AVON',  'Line Emission              '/
      data SECNAM(24),SECTIT(24)/'STOUR', 'Continuum Emission         '/
      data SECNAM(25),SECTIT(25)/'SWALE', 'Spectrum Summary           '/
      data SECNAM(26),SECTIT(26)/'NAR',   'Read restart JNU for PRD   '/
      data SECNAM(27),SECTIT(27)/'BRAY',  'Processing for Input-TAUK  '/
      data SECNAM(28),SECTIT(28)/'NENE',  'Gas Parameters             '/
      data SECNAM(29),SECTIT(29)/'URE',   'Passive Jnu Shuffling      '/
      data SECNAM(30),SECTIT(30)/'MEIG',  'Spectrum File Start-Up     '/
      data SECNAM(31),SECTIT(31)/'EDEN',  'Contributions Summary      '/
      data SECNAM(32),SECTIT(32)/'DOVEY' ,'Line Opacities Start-Up    '/
C
C---------------------------- for ICON
      data ICSTRT,ICFULL /.true., .false./
      data NIADR,NICON /0, 0/
C
C---------------------------- for URANUS
      data LIMPID /LEMUR/
      data OPNAM /LEMUR*0.D0/
      data IUOP,ILOP,NBOP,MBOP /4*LACK/
      data KEROP,INDOP /LEMUR*LACK, LEMUR*LACK/
C
C---------------------------- for DATAFIL
      data KIWILFN /LACK/
C
C---------------------------- for CARDFIL
      data KIWIOUT /0/
C
C---------------------------- for INCARD
      data LAST,LUINP,IRD,IPR,LUOUT,KARD,KART /LACK, 6*0/
C
C---------------------------- for VOISTA
      data KOUNVC /0/
C
C---------------------------- for ARABIS
      data NCOOP,NCOPR /2*0/
      data COOTM /0.D0/
C
C---------------------------- for WEITIM
      data KALLAM,KALPHI /2*0/
      data TIMLAM,TIMPHI /2*0.D0/
C
C---------------------------- for KAPPA
      data KAKODS /KAKOD*LACK/
C
C---------------------------- for SOLO
      data TREND /' '/
C
C---------------------------- for KWACK
      data NWKS  /MWKS/
      data KSWPR /MWKS*0/
      data KSWSH /MWKS*0/
C
C---------------------------- for EXTRSW
      data XTRALMU,XTRALML /1.D100, 1.D-100/
C
C---------------------------- for BASH
      data JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA /6*LACK/
C
C---------------------------- for SUKU
      data FILNMS  /'INPUT', 'MODEL', 'ATOM', 'RESTART', 'GENERAL'/
      data KFLOPN  /1, 0, 0, 0, 0/
      data KODEGN  /0/
      data FILSPEC /' '/
C
C---------------------------- for EYE
      data DOSEE,DOSTK,NSTKSEE /.false., .true., 0/
      data KODESEE,HEADSEE /1, ' '/
C
C---------------------------- for THUJA
      data THUSN /0.D0/
C
C---------------------------- for TORUS
      data KTRKAS,KTRKIS,KTRKIL /-1, 0, 0/
C
C---------------------------- for TAIMEN
      data TAIVS /0.D0/
C
C---------------------------- for TIGARA
      data TIGTIS,TIGES2,TIGES3,TIGES4 /0.D0, 1.D0, 5.D-1,
     $                                  3.333333333333333D-1/
C
C---------------------------- for SIRIT
      data DORTIS,DORE2S,DORE3S,DORE4S /4*0.D0/
C
C---------------------------- for BURNET
      data KERMED /NURBET*1/
C
C---------------------------- for SALKA
      data GENLAB /4*' '/
C
C---------------------------- for SENGE
      data NNDCK /0/
C
C---------------------------- for SOBOLEV
      data MDSOB /30/
C
C---------------------------- for LOOPER
      data NVEL,NVY,JVEL,LFBV,LFB,MF /6*0/
C
C---------------------------- for SPONNE
      data N1SKNT,NKSKNT /0, 0/
C
C---------------------------- for NOTIFY
      data MSSLIN /4*' '/
C
C---------------------------- for GAUSH10
      data XGH10 /
     $ 2.4534070830090D-01, 7.3747372854540D-01, 1.2340762153953D+00,
     $ 1.7385377121166D+00, 2.2549740020893D+00, 2.7888060584281D+00,
     $ 3.3478545673832D+00, 3.9447640401156D+00, 4.6036824495507D+00,
     $ 5.3874808900112D+00  /
      data AGH10 /
     $ 4.6224366960060D-01, 2.8667550536280D-01, 1.0901720602000D-01,
     $ 2.4810520887460D-02, 3.2437733422380D-03, 2.2833863601630D-04,
     $ 7.8025564785320D-06, 1.0860693707690D-07, 4.3993409922730D-10,
     $ 2.2293936455340D-13  /
C
C---------------------------- for NIVELON
      data KMXBND /100/
      data KMXWAV /10000/
C
C---------------------------- for APULIA
      data RAYSLM /1.425D3/
C
C---------------------------- for WAGRAM
      data IWGRBS /10/
      data XWGRTC /1.D-1/
C
C---------------------------- for FERGO
      data IUHY /2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
     $           17, 18, 19, 20, 21, 22, 23, 24, 25/
      data ILHY /MHYL*1/
C
C---------------------------- for FIRGO
      data HEEMAS /4.D0/
      data HEESKE /1.D0/
      data IUHEE  /5, 11, 12, 13/
      data ILHEE  /1,  1,  1,  1/
      data HEENUU /5.1304982D0, 5.5824194D0, 5.7399108D0, 5.8142519D0/
      data HEENUL /0.D0,        0.D0,        0.D0,        0.D0/
      data HEEPU  /3.D0, 3.D0, 64.D0, 100.D0/
      data HEEPL  /1.D0, 1.D0,  1.D0,   1.D0/
      data HEEAUL /1.8D9, 5.66D8, 2.46D8, 1.28D8/
      data HEECRD /1.628D-5, 4.43D-6, 1.78D-6, 9.03D-7/
      data HEECVW /4.426D-6, 1.353D-5, 3.148D-5, 6.211D-5/
      data HEECSK /1.63D-8, 3.34D-7, 7.048D-5, 1.28D-6/
C
C---------------------------- for FARGO
      data HEMAS /4.0D0/
      data HESKE /1.0D0/
      data HENUU /9.8685464D0, 11.696147D0, 12.335753D0, 12.63187D0,
     $            12.792657D0, 12.335753D0, 12.792657D0/
      data HENUL /5*0.0D0, 2*9.8685464D0/
      data HEPU  /8.D0, 18.D0, 32.D0, 50.D0, 72.D0, 32.D0, 72.D0/
      data HEPL  /5*2.0D0, 2*8.D0/
      data HEAUL /7.518D9, 8.91D8, 2.044D8, 6.598D7, 2.63D7,
     $            1.347D8, 1.56D7/
      data IUHE  /2, 3, 4, 5, 6, 4, 6/
      data ILHE  /5*1, 2*2/
      data LDLHE /5*1, 7, 7/
      data (HEDDL(I,1),I=1,LHEL) /LHEL*0.D0/
      data (HEDDL(I,2),I=1,LHEL) /LHEL*0.D0/
      data (HEDDL(I,3),I=1,LHEL) /LHEL*0.D0/
      data (HEDDL(I,4),I=1,LHEL) /LHEL*0.D0/
      data (HEDDL(I,5),I=1,LHEL) /LHEL*0.D0/
      data (HEDDL(I,6),I=1,LHEL) /-0.011D0, -0.004D0, -0.001D0, 0.007D0,
     $                             0.072D0, 0.076D0, 0.086D0/
      data (HEDDL(I,7),I=1,LHEL) /-0.002D0, 0.D0, 0.003D0, 0.005D0,
     $                             0.059D0, 0.06D0, 0.062D0/
      data (HECDL(I,1),I=1,LHEL) /LHEL*1.D0/
      data (HECDL(I,2),I=1,LHEL) /LHEL*1.D0/
      data (HECDL(I,3),I=1,LHEL) /LHEL*1.D0/
      data (HECDL(I,4),I=1,LHEL) /LHEL*1.D0/
      data (HECDL(I,5),I=1,LHEL) /LHEL*1.D0/
      data (HECDL(I,6),I=1,LHEL) /0.34D0, 0.19D0, 0.01D0, 0.1D0,
     $                            0.31D0, 0.04D0, 0.01D0/
      data (HECDL(I,7),I=1,LHEL) /0.33D0, 0.01D0, 0.22D0, 0.11D0,
     $                            0.29D0, 0.03D0, 0.01D0/
      data (HECRD(I,1),I=1,LHEL) /LHEL*1.84D-5/
      data (HECRD(I,2),I=1,LHEL) /LHEL*2.78D-6/
      data (HECRD(I,3),I=1,LHEL) /LHEL*7.57D-7/
      data (HECRD(I,4),I=1,LHEL) /LHEL*2.76D-7/
      data (HECRD(I,5),I=1,LHEL) /LHEL*1.21D-7/
      data (HECRD(I,6),I=1,LHEL) /LHEL*3.14D-4/
      data (HECRD(I,7),I=1,LHEL) /LHEL*2.14D-4/
      data (HECVW(I,1),I=1,LHEL) /LHEL*6.81D-7/
      data (HECVW(I,2),I=1,LHEL) /LHEL*1.78D-6/
      data (HECVW(I,3),I=1,LHEL) /LHEL*4.01D-6/
      data (HECVW(I,4),I=1,LHEL) /LHEL*7.82D-6/
      data (HECVW(I,5),I=1,LHEL) /LHEL*1.37D-5/
      data (HECVW(I,6),I=1,LHEL) /LHEL*1.00D-4/
      data (HECVW(I,7),I=1,LHEL) /LHEL*2.62D-4/
      data (HECSK(I,1),I=1,LHEL) /LHEL*2.51D-8/
      data (HECSK(I,2),I=1,LHEL) /LHEL*1.03D-6/
      data (HECSK(I,3),I=1,LHEL) /LHEL*1.65D-5/
      data (HECSK(I,4),I=1,LHEL) /LHEL*1.47D-4/
      data (HECSK(I,5),I=1,LHEL) /LHEL*8.87D-4/
      data (HECSK(I,6),I=1,LHEL) /LHEL*4.11D-4/
      data (HECSK(I,7),I=1,LHEL) /LHEL*1.70D-2/
C
C---------------------------- for FURGO
      data OXMAS /15.999D0/
      data OXSKE /1.0D0/
      data OXNUU /2*2.211515D0, 3*2.3022555D0, 3*2.8847544D0,
     $            3*2.9226423D0/
      data OXNUL /0.D0, .00474467D0,
     $            0.D0, .00474467D0, .0068046D0,
     $            0.D0, .00474467D0, .0068046D0,
     $            0.D0, .00474467D0, .0068046D0/
      data OXPU  /2*5.D0, 3*3.D0, 3*3.D0, 3*15.D0/
      data OXPL  /5.D0, 3.D0, 5.D0, 3.D0, 1.D0, 5.D0, 3.D0, 1.D0,
     $            5.D0, 3.D0, 1.D0/
      data OXAUL /4.2D3, 1.36D3, 3.41D8, 2.03D8, 6.76D7,
     $            9.43D7, 5.64D7, 1.88D7, 4.63D7, 2.54D7, 8.44D6/
      data OXCRD /2*2.7D-10, 3*2.75D-5, 3*4.9D-6, 3*3.7D-6/
      data OXCVW /1.52D-5, 1.53D-5, 3*1.5D-5, 3*1.9D-5, 3*1.7D-5/
      data OXCSK /2*1.27D-7, 1.81D-7, 2*1.82D-7,3*1.D-8, 3*1.7D-8/
      data IUOX  /2*6, 3*7, 3*11, 3*13/
      data ILOX  /1, 2, 1, 2, 3, 1, 2, 3, 1, 2, 3/
C
C---------------------------- for WURGO
      data X2MAS /15.999D0/
      data X2SKE /1.0D0/
      data X2NUU /3.592643D0/
      data X2NUL /0.D0/
      data X2PU  /12.D0/
      data X2PL  /4.D0/
      data X2AUL /5.17D9/
      data IUX2  /4/
      data ILX2  /1/
      data LDLX2 /3/
      data (X2DDL(I,1),I=1,LX2L) /-1.71D0, -1.14D0, 0.D0/
      data (X2CDL(I,1),I=1,LX2L) / 0.17D0, 0.33D0, 0.5D0/
      data (X2CRD(I,1),I=1,LX2L) /LX2L*9.6D-5/
      data (X2CVW(I,1),I=1,LX2L) /LX2L*2.72D-6/
      data (X2CSK(I,1),I=1,LX2L) /LX2L*2.58D-8/
C
C---------------------------- for WARGO
      data X3MAS /15.999D0/
      data X3SKE /1.0D0/
      data X3NUU /3.5991D0, 9.88062D0/
      data X3NUL /2*0.0D0/
      data X3PU  /15.D0, 9.D0/
      data X3PL  /2*9.D0/
      data X3AUL /8.45D8, 1.D10/
      data IUX3  /4, 8/
      data ILX3  /2*1/
      data LDLX3 /6, 6/
      data (X3DDL(I,1),I=1,LX3L) /0.D0, 0.79D0, 0.83D0,
     $                            2.13D0, 2.16D0, 2.36D0/
      data (X3DDL(I,2),I=1,LX3L) /0.D0, 0.049D0, 0.104D0,
     $                            0.21D0, 0.282D0, 0.388D0/
      data (X3CDL(I,1),I=1,LX3L) /0.112D0, 0.083D0, 0.251D0,
     $                            0.006D0, 0.083D0, 0.465D0/
      data (X3CDL(I,2),I=1,LX3L) /0.33D0, 0.11D0, 0.08D0,
     $                            0.14D0, 0.08D0, 0.26D0/
      data (X3CRD(I,1),I=1,LX3L) /LX3L*1.6D-5/
      data (X3CRD(I,2),I=1,LX3L) /LX3L*2.4D-6/
      data (X3CVW(I,1),I=1,LX3L) /LX3L*2.41D-6/
      data (X3CVW(I,2),I=1,LX3L) /LX3L*1.7D-6/
      data (X3CSK(I,1),I=1,LX3L) /LX3L*4.91D-8/
      data (X3CSK(I,2),I=1,LX3L) /LX3L*1.22D-6/
C
C---------------------------- for LIFFEY
      data FLNRML /15*1.D0/
C
C---------------------------- for SWABORT
      data SWDELA,SWPEND /.false., .false./
C
C---------------------------- for CARGOT
      data ESCARGO /.false./
C
C---------------------------- for STORPO
      data WRLDHO,WRLDPR,WRLDTY /.false., .false., .false./
C
C---------------------------- for KONOUT
      data KONLUN,KONLUR,KONLUD,KONHED /4*0/
C
C---------------------------- for HILYLI
      data LYLINO /0/
C
C---------------------------- for MISHED
      data MSHCLR,MSHCNO /MSHLNG*' ', 0/
C
C---------------------------- for TANGELO
      data NCISW,KCISW,KCISWD /LCISW, LCISW*0, LCISW*0/
      data NCESW,KCESW,KCESWD /LCESW, LCESW*0, LCESW*0/
C
C---------------------------- for OPACITY
      data NOPAC /NABS/
C
      data LOPAC /
     $  5, 20, 21, 10,  7, 12,  8, 17, 18, 19,
     $ 38, 42, 43,  6,  1, 40, 41, 13,  2,  9,
     $ 15, 26, 27, 22, 24, 31, 11, 34, 37, 35,
     $ 33, 39, 44, 45,  3,  4, 28, 29, 14, 30,
     $ 16, 36, 23, 25, 32/
C
      data KOPAC /NABS*1/
C
      data SYMID /NABS*' '/
C
      data CNAME( 1) /'           H- bound-free'/
      data CNAME( 2) /'            H- free-free'/
      data CNAME( 3) /'           Electron Sct.'/
      data CNAME( 4) /'         H Rayleigh Sct.'/
      data CNAME( 5) /'            H bound-free'/
      data CNAME( 6) /'            S bound-free'/
      data CNAME( 7) /'           Si bound-free'/
      data CNAME( 8) /'           Mg bound-free'/
      data CNAME( 9) /'                     H2+'/
      data CNAME(10) /'            C bound-free'/
      data CNAME(11) /'         H Ly alpha Abs.'/
      data CNAME(12) /'           Al bound-free'/
      data CNAME(13) /'             H free-free'/
      data CNAME(14) /'               Dust Sct.'/
      data CNAME(15) /'                    Dust'/
      data CNAME(16) /'         H Ly alpha Sct.'/
      data CNAME(17) /'           Fe bound-free'/
      data CNAME(18) /'           Na bound-free'/
      data CNAME(19) /'           Ca bound-free'/
      data CNAME(20) /'           He bound-free'/
      data CNAME(21) /'        He II bound-free'/
      data CNAME(22) /'   Statistical Line Abs.'/
      data CNAME(23) /'   Statistical Line Sct.'/
      data CNAME(24) /'     Composite Line Abs.'/
      data CNAME(25) /'     Composite Line Sct.'/
      data CNAME(26) /'                   X-ray'/
      data CNAME(27) /'    Carbon Monoxide Abs.'/
      data CNAME(28) /'        H2 Rayleigh Sct.'/
      data CNAME(29) /'        He Rayleigh Sct.'/
      data CNAME(30) /'    Carbon Monoxide Sct.'/
      data CNAME(31) /'      Averaged Line Abs.'/
      data CNAME(32) /'      Averaged Line Sct.'/
      data CNAME(33) /'             He-II Lines'/
      data CNAME(34) /'          H Ly 3-15 Abs.'/
      data CNAME(35) /'              He-I Lines'/
      data CNAME(36) /'          H Ly 3-15 Sct.'/
      data CNAME(37) /'            H Ly>15 Abs.'/
      data CNAME(38) /'            O bound-free'/
      data CNAME(39) /'               O-I Lines'/
      data CNAME(40) /'           OH bound-free'/
      data CNAME(41) /'           CH bound-free'/
      data CNAME(42) /'         O II bound-free'/
      data CNAME(43) /'        O III bound-free'/
      data CNAME(44) /'              O-II Lines'/
      data CNAME(45) /'             O-III Lines'/
C
      data SHNAM /
     $ '  H-bf', '  H-ff', '  Esct', '  HRay', '   Hbf', '     S',
     $ '    Si', '    Mg', '   H2+', '     C', 'LyAabs', '    Al',
     $ '   Hff', '  Dsct', '  Dabs', 'LyAsct', '    Fe', '    Na',
     $ '    Ca', '    He', '   He2', '  Sabs', '  Ssct', '  Cabs',
     $ '  Csct', '   Xry', ' COabs', ' H2Ray', ' HeRay', ' COsct',
     $ '  Aabs', '  Asct', 'He2lns', 'LyBabs', ' Helns', 'LyBsct',
     $ 'LyCabs', '     O', ' O1lns', '    OH', '    CH', '    O2',
     $ '    O3', ' O2lns', ' O3lns'/
C
C---------------------------- for FORGO
      data NLINCC /MLINCC/
      data LLINCC /11, 16, 22, 23, 24, 25, 27, 30, 31, 32, 33, 34,
     $             35, 36, 37, 39, 44, 45/
C
C---------------------------- for XRAYLIM
      data XRAYLO,XRAYHI /1.25D0, 1.24D2/
C
C---------------------------- for LYMALIM
      data XKWAVU,XKWAVL /0.D0, 0.D0/
C
C---------------------------- for SENNA
      data NCSBA /LCSBA/
      data CSBA /LCSBA*0.D0/
      data CSBO /LCSBA*0.D0/
      data LABSBA /
     $ 'TE'         , 'V'          , 'XNE'        , 'HND'        ,
     $ 'BDHM'       , 'TDUST'      , 'H2N'        , 'CON'        ,
     $ 'H(nlev)'    , 'H(bd)'      , 'H(nk)'      ,
     $ 'He-I(nlev)' , 'He-I(bd)'   , 'He-I(nk)'   ,
     $ 'He-II(nlev)', 'He-II(bd)'  , 'He-II(nk)'  ,
     $ 'C-I(nlev)'  , 'C-I(bd)'    , 'C-I(nk)'    ,
     $ 'Si-I(nlev)' , 'Si-I(bd)'   , 'Si-I(nk)'   ,
     $ 'Al-I(nlev)' , 'Al-I(bd)'   , 'Al-I(nk)'   ,
     $ 'Mg-I(nlev)' , 'Mg-I(bd)'   , 'Mg-I(nk)'   ,
     $ 'Fe-I(nlev)' , 'Fe-I(bd)'   , 'Fe-I(nk)'   ,
     $ 'Na-I(nlev)' , 'Na-I(bd)'   , 'Na-I(nk)'   ,
     $ 'Ca-I(nlev)' , 'Ca-I(bd)'   , 'Ca-I(nk)'   , 'VM'         ,
     $ 'O-I(nlev)'  , 'O-I(bd)'    , 'O-I(nk)'    , 'H1'         ,
     $ 'S-I(nlev)'  , 'S-I(bd)'    , 'S-I(nk)'    ,
     $ 'CHN'        , 'OHN'        ,
     $ 'O-II(nlev)' , 'O-II(bd)'   , 'O-II(nk)'   ,
     $ 'O-III(nlev)', 'O-III(bd)'  , 'O-III(nk)'  /
C
C---------------------------- for ELEMENT
      data NMTMAX /NELX/
      data NMT /40/
      data ELSYM /
     $ 'H  ',      'HE ',      'HE2',      'LI ',      'BE ',
     $ 'B  ',      'C  ',      'N  ',      'O  ',      'O2 ',
     $ 'O3 ',      'F  ',      'NE ',      'NA ',      'MG ',
     $ 'AL ',      'SI ',      'P  ',      'S  ',      'CL ',
     $ 'AR ',      'K  ',      'CA ',      'SC ',      'TI ',
     $ 'V  ',      'CR ',      'MN ',      'FE ',      'CO ',
     $ 'NI ',      'CU ',      'ZN ',      'GA ',      'GE ',
     $ 'RB ',      'SR ',      'Y  ',      'ZR ',      'BA ',
     $ 10*' '/
      data LATEM /
     $     3*.false., 6*.true., 2*.false.,
     $     29*.true., 10*.false. /
      data LATNO /
     $  1,          2,          2,          3,          4,
     $  5,          6,          7,          8,          8,
     $  8,          9,         10,         11,         12,
     $ 13,         14,         15,         16,         17,
     $ 18,         19,         20,         21,         22,
     $ 23,         24,         25,         26,         27,
     $ 28,         29,         30,         31,         32,
     $ 37,         38,         39,         40,         56,
     $ 10*0/
      data ELABD /
     $ 50*0.D0/
      data ELCHI /
     $ 13.595D0,   24.58D0,    54.403D0,   5.39D0,     9.32D0,
     $ 8.296D0,    11.256D0,   14.529D0,   13.614D0,   35.117D0,
     $ 54.936D0,   17.418D0,   21.558D0,   5.138D0,    7.644D0,
     $ 5.984D0,    8.149D0,    10.474D0,   10.357D0,   13.014D0,
     $ 15.755D0,   4.339D0,    6.111D0,    6.538D0,    6.818D0,
     $ 6.738D0,    6.763D0,    7.432D0,    7.896D0,    7.863D0,
     $ 7.633D0,    7.724D0,    9.391D0,    5.997D0,    7.899D0,
     $ 4.177D0,    5.693D0,    6.377D0,    6.835D0,    5.210D0,
     $ 10*0.D0/
      data ELLU1 /
     $ 2.D0,       1.D0,       2.D0,       2.09D0,     1.02D0,
     $ 6.03D0,     9.28D0,     4.07D0,     8.7D0,      4.D0,
     $ 9.D0,       5.62D0,     1.D0,       2.02D0,     1.01D0,
     $ 5.83D0,     9.26D0,     4.46D0,     8.12D0,     5.25D0,
     $ 1.D0,       2.18D0,     1.03D0,     12.D0,      30.2D0,
     $ 41.7D0,     10.3D0,     6.45D0,     24.5D0,     31.6D0,
     $ 28.8D0,     2.29D0,     1.D0,       5.37D0,     8.13D0,
     $ 2.29D0,     1.26D0,     12.D0,      33.9D0,     2.29D0,
     $ 10*0.D0/
      data ELLU2 /
     $ 1.D0,       2.D0,       1.D0,       1.D0,       2.D0,
     $ 1.D0,       5.94D0,     8.91D0,     3.98D0,     9.D0,
     $ 6.D0,       8.32D0,     5.37D0,     1.D0,       2.01D0,
     $ 1.03D0,     5.82D0,     8.12D0,     4.16D0,     7.76D0,
     $ 4.89D0,     1.D0,       2.29D0,     22.9D0,     56.2D0,
     $ 43.7D0,     7.24D0,     7.76D0,     39.2D0,     27.5D0,
     $ 10.D0,      1.02D0,     2.D0,       1.D0,       4.37D0,
     $ 1.D0,       2.19D0,     15.1D0,     45.7D0,     4.17D0,
     $ 10*0.D0/
      data ELABL /
     $ 50*0.D0/
      data ELDEF /
     $ 1.2D1,      1.1D1,      1.1D1,      1.05D0,     1.38D0,
     $ 2.70D0,     8.39D0,     7.85D0,     8.66D0,     8.66D0,
     $ 8.66D0,     4.56D0,     7.84D0,     6.17D0,     7.53D0,
     $ 6.37D0,     7.51D0,     5.36D0,     7.14D0,     5.5D0,
     $ 6.18D0,     5.08D0,     6.31D0,     3.05D0,     4.90D0,
     $ 4.D0,       5.64D0,     5.39D0,     7.45D0,     4.92D0,
     $ 6.23D0,     4.21D0,     4.6D0,      2.88D0,     3.58D0,
     $ 2.6D0,      2.92D0,     2.21D0,     2.59D0,     2.17D0,
     $ 10*0.D0/
      data LDEFR /
     $ 0,          0,          0,          3,          3,
     $ 1,          4,          5,          2,          2,
     $ 2,          1,          3,          3,          3,
     $ 3,          3,          3,          3,          1,
     $ 3,          3,          3,          3,          3,
     $ 1,          3,          1,          3,          1,
     $ 3,          1,          1,          1,          3,
     $ 1,          3,          3,          3,          3,
     $ 10*0/
C
C---------------------------- for ELEGANT
      data NELE /92/
      data ENAMES /
     $ 'HYDROGEN', 'HELIUM', 'LITHIUM', 'BERYLLIUM', 'BORON', 'CARBON',
     $ 'NITROGEN', 'OXYGEN', 'FLUORINE', 'NEON', 'SODIUM', 'MAGNESIUM',
     $ 'ALUMINUM', 'SILICON', 'PHOSPHORUS', 'SULFUR', 'CHLORINE',
     $ 'ARGON', 'POTASSIUM', 'CALCIUM', 'SCANDIUM', 'TITANIUM',
     $ 'VANADIUM', 'CHROMIUM', 'MANGANESE', 'IRON', 'COBALT', 'NICKEL',
     $ 'COPPER', 'ZINC', 'GALLIUM', 'GERMANIUM', 'ARSENIC', 'SELENIUM',
     $ 'BROMINE', 'KRYPTON', 'RUBIDIUM', 'STRONTIUM', 'YTTRIUM',
     $ 'ZIRCONIUM', 'NIOBIUM', 'MOLYBDENUM', 'TECHNETIUM', 'RUTHENIUM',
     $ 'RHODIUM', 'PALLADIUM', 'SILVER', 'CADMIUM', 'INDIUM', 'TIN',
     $ 'ANTIMONY', 'TELLURIUM', 'IODINE', 'XENON', 'CESIUM', 'BARIUM',
     $ 'LANTHANUM', 'CERIUM', 'PRASEODYMIUM', 'NEODYMIUM', 'PROMETHIUM',
     $ 'SAMARIUM', 'EUROPIUM', 'GADOLINIUM', 'TERBIUM', 'DYSPROSIUM',
     $ 'HOLMIUM', 'ERBIUM', 'THULIUM', 'YTTERBIUM', 'LUTETIUM',
     $ 'HAFNIUM', 'TANTALUM', 'TUNGSTEN', 'RHENIUM', 'OSMIUM',
     $ 'IRIDIUM', 'PLATINUM', 'GOLD', 'MERCURY', 'THALLIUM', 'LEAD',
     $ 'BISMUTH', 'POLONIUM', 'ASTATINE', 'RADON', 'FRANCIUM', 'RADIUM',
     $ 'ACTINIUM', 'THORIUM', 'PROTACTINIUM', 'URANIUM'/
      data LNAM /
     $  8,  6,  7,  9,  5,  6,  8,  6,  8,  4,  6,  9,  8,  7, 10,  6,
     $  8,  5,  9,  7,  8,  8,  8,  8,  9,  4,  6,  6,  6,  4,  7,  9,
     $  7,  8,  7,  7,  8,  9,  7,  9,  7, 10, 10,  9,  7,  9,  6,  7,
     $  6,  3,  8,  9,  6,  5,  6,  6,  9,  6, 12,  9, 10,  8,  8, 10,
     $  7, 10,  7,  6,  7,  9,  8,  7,  8,  8,  7,  6,  7,  8,  4,  7,
     $  8,  4,  7,  8,  8,  5,  8,  6,  8,  7, 12,  7/
      data EABBRS /
     $ 'H', 'HE', 'LI', 'BE', 'B', 'C', 'N', 'O', 'F', 'NE', 'NA', 'MG',
     $ 'AL', 'SI', 'P', 'S', 'CL', 'AR', 'K', 'CA', 'SC', 'TI', 'V',
     $ 'CR', 'MN', 'FE', 'CO', 'NI', 'CU', 'ZN', 'GA', 'GE', 'AS', 'SE',
     $ 'BR', 'KR', 'RB', 'SR', 'Y', 'ZR', 'NB', 'MO', 'TC', 'RU', 'RH',
     $ 'PD', 'AG', 'CD', 'IN', 'SN', 'SB', 'TE', 'I', 'XE', 'CS', 'BA',
     $ 'LA', 'CE', 'PR', 'ND', 'PM', 'SM', 'EU', 'GD', 'TB', 'DY', 'HO',
     $ 'ER', 'TM', 'YB', 'LU', 'HF', 'TA', 'W', 'RE', 'OS', 'IR', 'PT',
     $ 'AU', 'HG', 'TL', 'PB', 'BI', 'PO', 'AT', 'RN', 'FR', 'RA', 'AC',
     $ 'TH', 'PA', 'U'/
      data LABR /
     $ 1, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 2, 2, 1, 2,
     $ 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2,
     $ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2,
     $ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2,
     $ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1/
C
C---------------------------- for IONDATA
      data      LEND /ILENTP/
C
C     Hydrogen
C
      data LIMDAT(1) /ILEVH/
      data LLPRNT(1) /.true./
      data (LLABEL(I,1),I=1,ILEVH) /
     $     ' 1S 2S          ', ' 2S 2S          ', ' 3S 2S          ',
     $     ' 4S 2S          ', ' 5S 2S          ', ' 6S 2S          ',
     $     ' 7S 2S          ', ' 8S 2S          ', ' 9S 2S          ',
     $     '10S 2S          ', '11S 2S          ', '12S 2S          ',
     $     '13S 2S          ', '14S 2S          ', '15S 2S          '/
      data (PILEVL(I,1),I=1,ILEVH) /ILEVH*0.D0/
      data (XLMTHR(I,1),I=1,ILEVH) /ILEVH*0.D0/
      data (CCPLEV(I,1),I=1,ILEVH) /ILEVH*0.D0/
      data (SCPLEV(I,1),I=1,ILEVH) /ILEVH*0.D0/
      data (NPTABL(I,1),I=1,ILEVH) /ILEVH*0/
C     (Note: Hydrogen data are set up in subroutine AMOEBA)
C
C     Carbon I
C
      data LIMDAT(2) /ILEVC/
      data LLPRNT(2) /.true./
      data (LLABEL(I,2),I=1,ILEVC) /
     $     '2P2 3P          ', '2P2 1D          ', '2P2 1S          ',
     $     '2P3 5S0         ', '3S 3P0          ', '3S 1P0          ',
     $     '2P3 3D0         ', '3P 3D           '/
      data (PILEVL(I,2),I=1,ILEVC) /9.D0, 5.D0, 1.D0, 5.D0, 9.D0,
     $     3.D0, 15.D0, 27.D0/
      data (XLMTHR(I,2),I=1,ILEVC) /1100.4D0, 1239.4D0, 1443.2D0,
     $     1748.1D0, 3272.6D0, 3453.4D0, 3726.5D0, 5126.3D0/
      data (CCPLEV(I,2),I=1,ILEVC) /17.2D0, 10.5D0, 14.3D0, 13.1D0,
     $     .146D0, 1.07D0, 12.D0, 2.44D0/
      data (SCPLEV(I,2),I=1,ILEVC) /ILEVC*0.D0/
      data (NPTABL(I,2),I=1,ILEVC) /24, 19, 15, 17, 16, 15, 14, 16/
      data (XLMTAB(I,1,2),I=1,24) /200.D0, 300.D0, 400.D0, 500.D0,
     $      600.D0, 650.D0, 700.D0, 740.D0, 760.D0, 780.D0, 800.D0,
     $      820.D0, 840.D0, 860.D0, 880.D0, 913.D0, 940.D0, 970.D0,
     $      1000.D0, 1020.D0, 1040.D0, 1060.D0, 1080.D0, 1100.4D0/
      data (RCPTAB(I,1,2),I=1,24) /.08D0, .17D0, .28D0, .4D0, .53D0,
     $      .59D0, .65D0, .7D0, .72D0, .74D0, .76D0, .79D0, .8D0,
     $      .83D0, .84D0, .87D0, .9D0, .92D0, .94D0, .96D0, .97D0,
     $      .98D0, .99D0, 1.D0/
      data (XLMTAB(I,2,2),I=1,19) /351.D0, 506.D0, 570.D0, 651.D0,
     $     701.D0, 759.D0, 828.D0, 893.D0, 895.D0, 911.D0, 930.D0,
     $     1000.D0, 1044.D0, 1070.D0, 1139.D0, 1171.D0, 1210.D0,
     $     1220.D0, 1239.4D0/
      data (RCPTAB(I,2,2),I=1,19) /.37D0, .61D0, .73D0, .92D0, 1.02D0,
     $     1.04D0, 1.48D0, 3.14D0, 6.58D0, 4.93D0, 2.15D0, 3.33D0,
     $     4.29D0, 2.38D0, 1.03D0, .92D0, .97D0, .97D0, 1.D0/
      data (XLMTAB(I,3,2),I=1,15) /351.D0, 456.D0, 570.D0, 701.D0,
     $     828.D0, 911.D0, 959.D0, 997.D0, 1013.D0, 1040.D0, 1139.D0,
     $     1210.D0, 1220.D0, 1302.D0, 1443.2D0/
      data (RCPTAB(I,3,2),I=1,15) /.25D0, .41D0, .53D0, .71D0, 1.D0,
     $     1.51D0, 2.76D0, 9.75D0, 2.11D0, .25D0, .59D0, .73D0, .73D0,
     $     .89D0, 1.D0/
      data (XLMTAB(I,4,2),I=1,17) /259.D0, 274.D0, 292.D0, 335.D0,
     $     362.D0, 393.D0, 474.D0, 530.D0, 641.D0, 690.D0, 813.D0,
     $     892.D0, 1110.D0, 1264.D0, 1468.D0, 1717.D0, 1748.1 /
      data (RCPTAB(I,4,2),I=1,17) /.21D0, .24D0, .26D0, .34D0, .39D0,
     $     .43D0, .57D0, .65D0, .8D0, .85D0, .96D0, 1.02D0, 1.1D0,
     $     1.11D0, 1.09D0, 1.02D0, 1.D0/
      data (XLMTAB(I,5,2),I=1,16) /278.D0, 316.D0, 367.D0, 438.D0,
     $     543.D0, 713.D0, 773.D0, 845.D0, 932.D0, 1171.D0, 1577.D0,
     $     1907.D0, 2412.D0, 2780.D0, 3166.D0, 3272.6D0/
      data (RCPTAB(I,5,2),I=1,16) /.38D0, .43D0, .47D0, .53D0, .57D0,
     $     .64D0, .68D0, .71D0, .76D0, .88D0, 1.02D0, 1.04D0, .95D0,
     $     .88D0, .93D0, 1.D0/
      data (XLMTAB(I,6,2),I=1,15) /369.D0, 402.D0, 441.D0, 489.D0,
     $     622.D0, 783.D0, 856.D0, 1055.D0, 1193.D0, 1373.D0, 1966.D0,
     $     2507.D0, 2907.D0, 3333.D0, 3453.4D0/
      data (RCPTAB(I,6,2),I=1,15) /.077D0, .087D0, .096D0, .1D0,
     $     .14D0, .18D0, .2D0, .24D0, .27D0, .28D0, .32D0, .39D0,
     $     .55D0, .86D0, 1.D0/
      data (XLMTAB(I,7,2),I=1,14) /281.D0, 320.D0, 345.D0, 406.D0,
     $     494.D0, 631.D0, 797.D0, 965.D0, 1080.D0, 1415.D0, 1675.D0,
     $     2649.D0, 3100.D0, 3726.5D0/
      data (RCPTAB(I,7,2),I=1,14) /.07D0, .09D0, .1D0, .14D0, .2D0,
     $     .28D0, .38D0, .48D0, .53D0, .67D0, .75D0, .93D0, .97D0,
     $     1.D0/
      data (XLMTAB(I,8,2),I=1,16) /400.D0, 600.D0, 1000.D0, 1500.D0,
     $     2000.D0, 2375.D0, 2500.D0, 3000.D0, 3500.D0, 4000.D0,
     $     4500.D0, 4733.D0, 4734.D0, 4965.D0, 4966.D0, 5126.3D0/
      data (RCPTAB(I,8,2),I=1,16) /.09D0, .23D0, .56D0, .95D0, 1.7D0,
     $     1.46D0, 2.16D0, 1.96D0, 2.51D0, 2.48D0, 2.78D0, 2.56D0,
     $     2.02D0, 1.98D0, 1.016D0, 1.D0/
C
C     Silicon I
C
      data LIMDAT(3) /ILEVSI/
      data LLPRNT(3) /.true./
      data (LLABEL(I,3),I=1,ILEVSI) /
     $     '3P2 3P          ', '3P2 1D          ', '3P2 1S          ',
     $     '3P3 5S0         ', '4S 3P0          ', '4S 1P0          ',
     $     '3D 3D0          ', '4P 3SPD         '/
      data (PILEVL(I,3),I=1,ILEVSI) /9.D0, 5.D0, 1.D0, 5.D0, 9.D0,
     $     3.D0, 15.D0, 15.D0/
      data (XLMTHR(I,3),I=1,ILEVSI) /1519.9D0, 1680.8D0, 1984.D0,
     $     3080.D0, 3847.D0, 4032.D0, 4878.9D0, 5660.9D0/
      data (CCPLEV(I,3),I=1,ILEVSI) /70.1D0, 34.6D0, 26.3D0,
     $     54.D0, 0.56D0, 3.386D0, 16.6D0, 22.18D0/
      data (SCPLEV(I,3),I=1,ILEVSI) /ILEVSI*0.D0/
      data (NPTABL(I,3),I=1,ILEVSI) /103, 114, 43, 8, 81, 61, 48, 66/
      data (XLMTAB(I,1,3),I=1,80) /
     $ 1.39400D+02, 1.54100D+02, 1.80800D+02, 2.03900D+02, 2.29900D+02,
     $ 2.59200D+02, 2.92200D+02, 3.29500D+02, 3.71500D+02, 4.18800D+02,
     $ 4.72200D+02, 5.32400D+02, 6.00300D+02, 6.24800D+02, 6.50300D+02,
     $ 7.30300D+02, 7.37900D+02, 7.39000D+02, 7.47300D+02, 7.48000D+02,
     $ 7.48800D+02, 7.51800D+02, 7.52600D+02, 7.54900D+02, 7.57400D+02,
     $ 7.60000D+02, 7.81500D+02, 7.85000D+02, 7.90500D+02, 7.97900D+02,
     $ 8.01300D+02, 8.09900D+02, 8.32700D+02, 8.74900D+02, 8.91300D+02,
     $ 9.19300D+02, 9.25300D+02, 9.28400D+02, 9.46700D+02, 9.75000D+02,
     $ 9.78000D+02, 9.80400D+02, 9.82900D+02, 9.83300D+02, 9.84100D+02,
     $ 9.85300D+02, 9.86200D+02, 9.86600D+02, 9.87000D+02, 9.87500D+02,
     $ 9.87900D+02, 9.88400D+02, 9.88800D+02, 9.89300D+02, 9.89800D+02,
     $ 9.90300D+02, 9.91200D+02, 9.93300D+02, 9.99000D+02, 1.00500D+03,
     $ 1.00800D+03, 1.01000D+03, 1.01100D+03, 1.01200D+03, 1.01300D+03,
     $ 1.01400D+03, 1.01500D+03, 1.01600D+03, 1.01700D+03, 1.01800D+03,
     $ 1.01900D+03, 1.02000D+03, 1.02200D+03, 1.02300D+03, 1.02400D+03,
     $ 1.02600D+03, 1.02800D+03, 1.03300D+03, 1.06200D+03, 1.07000D+03/
      data (XLMTAB(I,1,3),I=81,103) /
     $ 1.07800D+03, 1.08100D+03, 1.08300D+03, 1.08500D+03, 1.08800D+03,
     $ 1.09000D+03, 1.09300D+03, 1.09600D+03, 1.09800D+03, 1.10100D+03,
     $ 1.10400D+03, 1.10700D+03, 1.11000D+03, 1.11300D+03, 1.11700D+03,
     $ 1.12400D+03, 1.13100D+03, 1.14700D+03, 1.19200D+03, 1.24000D+03,
     $ 1.34600D+03, 1.47300D+03, 1.52000D+03/
      data (RCPTAB(I,1,3),I=1,80) /
     $ 2.10000D-04, 2.90000D-04, 4.70000D-04, 6.70000D-04, 9.60000D-04,
     $ 1.40000D-03, 2.00000D-03, 2.80000D-03, 4.00000D-03, 5.80000D-03,
     $ 8.30000D-03, 1.20000D-02, 1.70000D-02, 1.90000D-02, 2.20000D-02,
     $ 5.50000D-02, 5.50000D-02, 5.40000D-02, 5.40000D-02, 4.60000D-02,
     $ 3.90000D-02, 1.10000D-01, 1.10000D-01, 1.00000D-01, 9.20000D-02,
     $ 8.80000D-02, 8.10000D-02, 7.00000D-02, 3.70000D-02, 8.60000D-02,
     $ 9.40000D-02, 1.00000D-01, 1.20000D-01, 1.50000D-01, 1.70000D-01,
     $ 1.90000D-01, 1.90000D-01, 2.30000D-01, 2.30000D-01, 2.40000D-01,
     $ 2.80000D-01, 3.00000D-01, 3.70000D-01, 3.70000D-01, 2.60000D-01,
     $ 2.30000D-01, 2.20000D-01, 1.80000D-01, 1.20000D-01, 6.00000D-02,
     $ 5.40000D-02, 8.70000D-02, 1.20000D-01, 1.50000D-01, 1.80000D-01,
     $ 1.90000D-01, 2.20000D-01, 2.50000D-01, 2.90000D-01, 3.40000D-01,
     $ 3.80000D-01, 4.40000D-01, 4.50000D-01, 4.10000D-01, 3.30000D-01,
     $ 2.90000D-01, 2.70000D-01, 2.30000D-01, 1.50000D-01, 7.60000D-02,
     $ 4.80000D-02, 6.70000D-02, 1.30000D-01, 1.60000D-01, 1.80000D-01,
     $ 2.20000D-01, 2.40000D-01, 2.70000D-01, 3.90000D-01, 4.40000D-01/
      data (RCPTAB(I,1,3),I=81,103) /
     $ 5.40000D-01, 5.20000D-01, 4.40000D-01, 3.60000D-01, 3.30000D-01,
     $ 3.20000D-01, 2.80000D-01, 2.00000D-01, 1.10000D-01, 5.90000D-02,
     $ 7.00000D-02, 1.10000D-01, 1.50000D-01, 1.90000D-01, 2.20000D-01,
     $ 2.70000D-01, 3.10000D-01, 3.60000D-01, 5.15000D-01, 5.15000D-01,
     $ 8.00000D-01, 9.70000D-01, 1.00000D+00/
      data (XLMTAB(I,2,3),I=1,80) /
     $ 6.38900D+02, 6.78400D+02, 6.95700D+02, 7.43200D+02, 7.43600D+02,
     $ 7.48200D+02, 7.48400D+02, 7.48600D+02, 7.54900D+02, 7.55800D+02,
     $ 7.56100D+02, 7.56400D+02, 7.58900D+02, 7.59600D+02, 7.60700D+02,
     $ 7.63400D+02, 7.64600D+02, 7.65100D+02, 7.73700D+02, 7.76100D+02,
     $ 7.78000D+02, 7.80100D+02, 7.88800D+02, 8.16800D+02, 8.20600D+02,
     $ 8.22400D+02, 8.23000D+02, 8.23300D+02, 8.23600D+02, 8.24000D+02,
     $ 8.24300D+02, 8.24600D+02, 8.25300D+02, 8.26000D+02, 8.26700D+02,
     $ 8.27100D+02, 8.27800D+02, 8.28200D+02, 8.31100D+02, 8.35200D+02,
     $ 8.38900D+02, 8.41200D+02, 8.43100D+02, 8.43700D+02, 8.44400D+02,
     $ 8.45000D+02, 8.45700D+02, 8.46400D+02, 8.47800D+02, 8.50000D+02,
     $ 8.54000D+02, 8.54800D+02, 8.55700D+02, 8.56500D+02, 8.79400D+02,
     $ 9.04800D+02, 9.15500D+02, 9.38900D+02, 9.39700D+02, 9.40600D+02,
     $ 9.41400D+02, 9.42300D+02, 9.43200D+02, 9.44000D+02, 9.45900D+02,
     $ 9.47800D+02, 9.51900D+02, 9.76500D+02, 9.92500D+02, 1.00100D+03,
     $ 1.00300D+03, 1.00500D+03, 1.00700D+03, 1.01000D+03, 1.01200D+03,
     $ 1.01300D+03, 1.01500D+03, 1.01600D+03, 1.01800D+03, 1.01900D+03/
      data (XLMTAB(I,2,3),I=81,114) /
     $ 1.02100D+03, 1.02120D+03, 1.02200D+03, 1.02300D+03, 1.02500D+03,
     $ 1.02600D+03, 1.02700D+03, 1.02800D+03, 1.02900D+03, 1.03000D+03,
     $ 1.03100D+03, 1.03500D+03, 1.04100D+03, 1.04600D+03, 1.05500D+03,
     $ 1.09900D+03, 1.13300D+03, 1.16000D+03, 1.18400D+03, 1.20600D+03,
     $ 1.23200D+03, 1.25300D+03, 1.27000D+03, 1.28900D+03, 1.35300D+03,
     $ 1.40600D+03, 1.45800D+03, 1.52200D+03, 1.54400D+03, 1.54600D+03,
     $ 1.55400D+03, 1.55600D+03, 1.60200D+03, 1.68090D+03/
      data (RCPTAB(I,2,3),I=1,80) /
     $ 5.60000D-02, 6.70000D-02, 7.90000D-02, 1.10000D-01, 1.10000D-01,
     $ 1.00000D-01, 1.10000D-01, 1.40000D-01, 1.80000D-01, 1.30000D-01,
     $ 1.20000D-01, 1.20000D-01, 1.30000D-01, 1.40000D-01, 1.60000D-01,
     $ 2.40000D-01, 1.60000D-01, 1.60000D-01, 1.40000D-01, 1.20000D-01,
     $ 1.10000D-01, 1.10000D-01, 1.40000D-01, 2.60000D-01, 2.10000D-01,
     $ 2.00000D-01, 1.80000D-01, 1.80000D-01, 2.10000D-01, 3.00000D-01,
     $ 3.80000D-01, 3.90000D-01, 3.60000D-01, 3.30000D-01, 3.20000D-01,
     $ 3.30000D-01, 3.00000D-01, 3.00000D-01, 2.90000D-01, 3.00000D-01,
     $ 3.30000D-01, 3.50000D-01, 3.30000D-01, 4.00000D-01, 4.20000D-01,
     $ 3.70000D-01, 3.40000D-01, 3.10000D-01, 2.80000D-01, 2.60000D-01,
     $ 2.60000D-01, 2.50000D-01, 2.40000D-01, 2.50000D-01, 2.90000D-01,
     $ 3.30000D-01, 2.90000D-01, 4.20000D-01, 4.40000D-01, 6.00000D-01,
     $ 4.70000D+00, 2.40000D-01, 2.80000D-01, 2.90000D-01, 3.30000D-01,
     $ 3.50000D-01, 3.80000D-01, 4.70000D-01, 5.50000D-01, 6.20000D-01,
     $ 6.60000D-01, 5.90000D+00, 9.50000D-01, 9.60000D-01, 1.10000D+00,
     $ 1.20000D+00, 1.50000D+00, 1.80000D+00, 2.60000D+00, 2.50000D+00/
      data (RCPTAB(I,2,3),I=81,114) /
     $ 1.30000D+00, 9.90000D-01, 6.40000D-01, 3.30000D-01, 1.40000D-01,
     $ 8.40000D-02, 5.80000D-02, 4.70000D-02, 5.10000D-02, 5.80000D-02,
     $ 9.90000D-02, 2.30000D-01, 3.70000D-01, 4.40000D-01, 5.20000D-01,
     $ 7.50000D-01, 9.50000D-01, 1.20000D+00, 1.40000D+00, 1.70000D+00,
     $ 2.30000D+00, 2.60000D+00, 2.70000D+00, 2.60000D+00, 2.00000D+00,
     $ 1.60000D+00, 1.40000D+00, 1.20000D+00, 1.12000D+00, 1.12000D+00,
     $ 1.14000D+00, 1.14000D+00, 1.10000D+00, 1.00000D+00/
      data (XLMTAB(I,3,3),I=1,43) /
     $ 6.98000D+02, 8.55600D+02, 9.41400D+02, 1.05800D+03, 1.09700D+03,
     $ 1.10700D+03, 1.10900D+03, 1.11200D+03, 1.11500D+03, 1.11700D+03,
     $ 1.12000D+03, 1.12100D+03, 1.12600D+03, 1.14400D+03, 1.19400D+03,
     $ 1.23800D+03, 1.27400D+03, 1.30500D+03, 1.33500D+03, 1.38300D+03,
     $ 1.40100D+03, 1.41400D+03, 1.42000D+03, 1.42700D+03, 1.43500D+03,
     $ 1.45800D+03, 1.46600D+03, 1.47500D+03, 1.48400D+03, 1.49400D+03,
     $ 1.50300D+03, 1.51400D+03, 1.52400D+03, 1.53500D+03, 1.54700D+03,
     $ 1.55900D+03, 1.57200D+03, 1.58500D+03, 1.59900D+03, 1.61400D+03,
     $ 1.62900D+03, 1.74100D+03, 1.98420D+03/
      data (RCPTAB(I,3,3),I=1,43) /
     $ 1.10000D-01, 3.30000D-01, 5.20000D-01, 9.40000D-01, 1.10000D+00,
     $ 1.10000D+00, 1.20000D+00, 1.60000D+00, 3.40000D+01, 1.70000D+00,
     $ 1.30000D+00, 1.30000D+00, 1.20000D+00, 1.30000D+00, 1.50000D+00,
     $ 1.80000D+00, 2.20000D+00, 2.70000D+00, 3.30000D+00, 4.60000D+00,
     $ 4.80000D+00, 4.70000D+00, 4.50000D+00, 4.20000D+00, 3.80000D+00,
     $ 2.30000D+00, 1.80000D+00, 1.30000D+00, 9.70000D-01, 6.90000D-01,
     $ 4.70000D-01, 3.10000D-01, 1.90000D-01, 1.20000D-01, 7.90000D-02,
     $ 6.00000D-02, 5.80000D-02, 7.10000D-02, 9.30000D-02, 1.20000D-01,
     $ 1.60000D-01, 4.40000D-01, 1.00000D+00/
      data (XLMTAB(I,4,3),I=1,8) /
     $ 8.00500D+02, 8.16600D+02, 8.84400D+02, 1.04100D+03, 1.11100D+03,
     $ 1.32600D+03, 1.32650D+03, 3.08010D+03/
      data (RCPTAB(I,4,3),I=1,8) /
     $ 1.50000D-01, 1.60000D-01, 2.30000D-01, 4.50000D-01, 5.70000D-01,
     $ 1.00000D+00, 0.00000D+00, 0.00000D+00/
      data (XLMTAB(I,5,3),I=1,81) /
     $ 8.78300D+02, 9.20500D+02, 9.57500D+02, 9.57700D+02, 9.86100D+02,
     $ 9.87300D+02, 9.89200D+02, 9.91100D+02, 9.91600D+02, 9.92400D+02,
     $ 9.92600D+02, 9.92900D+02, 9.93100D+02, 9.93400D+02, 9.93700D+02,
     $ 9.94000D+02, 9.99600D+02, 1.00000D+03, 1.00100D+03, 1.00120D+03,
     $ 1.00300D+03, 1.01200D+03, 1.03100D+03, 1.03400D+03, 1.05200D+03,
     $ 1.05800D+03, 1.06800D+03, 1.08600D+03, 1.40300D+03, 1.40800D+03,
     $ 1.41300D+03, 1.41800D+03, 1.42700D+03, 1.52900D+03, 1.58500D+03,
     $ 1.69300D+03, 1.76500D+03, 1.76900D+03, 1.77300D+03, 1.77700D+03,
     $ 1.78100D+03, 1.78500D+03, 1.78900D+03, 1.79400D+03, 1.79800D+03,
     $ 1.80300D+03, 1.80700D+03, 1.82200D+03, 1.82700D+03, 1.83200D+03,
     $ 1.86700D+03, 1.87300D+03, 1.88000D+03, 1.89300D+03, 1.91500D+03,
     $ 1.94700D+03, 1.97400D+03, 1.98400D+03, 1.99400D+03, 2.00400D+03,
     $ 2.01500D+03, 2.03700D+03, 2.08700D+03, 2.16100D+03, 2.19500D+03,
     $ 2.21300D+03, 2.23200D+03, 2.25200D+03, 2.29400D+03, 2.31600D+03,
     $ 2.36500D+03, 2.41900D+03, 2.47900D+03, 2.54500D+03, 2.66100D+03,
     $ 2.91000D+03, 3.10900D+03, 3.27100D+03, 3.46400D+03, 3.69900D+03,
     $ 3.84710D+03/
      data (RCPTAB(I,5,3),I=1,81) /
     $ 8.90000D+00, 1.20000D+01, 1.40000D+01, 1.50000D+01, 1.60000D+01,
     $ 1.50000D+01, 1.30000D+01, 1.00000D+01, 9.00000D+00, 6.20000D+00,
     $ 5.60000D+00, 5.60000D+00, 6.90000D+00, 9.60000D+00, 1.30000D+01,
     $ 1.60000D+01, 1.40000D+01, 1.60000D+02, 5.90000D+01, 3.40000D+01,
     $ 2.50000D+01, 2.30000D+01, 2.10000D+01, 2.80000D+01, 6.40000D+00,
     $ 3.70000D+01, 3.70000D+01, 3.80000D+01, 4.10000D+01, 3.40000D+01,
     $ 3.00000D+01, 2.60000D+01, 2.40000D+01, 5.00000D+00, 3.30000D+00,
     $ 2.10000D+00, 1.50000D+00, 1.60000D+00, 1.70000D+00, 2.00000D+00,
     $ 3.00000D+00, 4.90000D+00, 4.50000D+00, 3.20000D+00, 2.40000D+00,
     $ 2.10000D+00, 1.80000D+00, 1.50000D+00, 1.70000D+00, 1.30000D+00,
     $ 9.20000D-01, 6.70000D-01, 1.70000D+00, 1.40000D+00, 1.20000D+00,
     $ 1.00000D+00, 9.70000D-01, 1.00000D+00, 3.60000D+00, 1.20000D+00,
     $ 9.30000D-01, 8.60000D-01, 8.10000D-01, 8.70000D-01, 1.10000D+00,
     $ 1.30000D+00, 1.70000D+00, 2.50000D+00, 5.60000D+00, 4.60000D+00,
     $ 1.90000D+00, 1.10000D+00, 8.10000D-01, 6.60000D-01, 5.10000D-01,
     $ 3.50000D-01, 3.00000D-01, 3.20000D-01, 4.10000D-01, 6.00000D-01,
     $ 1.00000D+00/
      data (XLMTAB(I,6,3),I=1,61) /
     $ 8.88600D+02, 1.06500D+03, 1.06900D+03, 1.10100D+03, 1.13300D+03,
     $ 1.13900D+03, 1.14400D+03, 1.15400D+03, 1.15500D+03, 1.15700D+03,
     $ 1.17500D+03, 1.17800D+03, 1.18300D+03, 1.18600D+03, 1.18900D+03,
     $ 1.19300D+03, 1.19500D+03, 1.21300D+03, 1.21400D+03, 1.21500D+03,
     $ 1.21520D+03, 1.21540D+03, 1.21560D+03, 1.21580D+03, 1.21600D+03,
     $ 1.21620D+03, 1.21640D+03, 1.21660D+03, 1.21680D+03, 1.21700D+03,
     $ 1.21800D+03, 1.21900D+03, 1.22700D+03, 1.24800D+03, 1.58700D+03,
     $ 1.63600D+03, 1.68100D+03, 1.72500D+03, 1.73200D+03, 1.74700D+03,
     $ 1.75500D+03, 1.75700D+03, 1.76000D+03, 1.76600D+03, 1.78200D+03,
     $ 1.81100D+03, 1.81500D+03, 1.90800D+03, 1.94900D+03, 2.00800D+03,
     $ 2.05900D+03, 2.13200D+03, 2.22400D+03, 2.36500D+03, 2.56200D+03,
     $ 2.85400D+03, 3.16500D+03, 3.41900D+03, 3.63100D+03, 3.88900D+03,
     $ 4.03210D+03/
      data (RCPTAB(I,6,3),I=1,61) /
     $ 1.20000D+00, 4.30000D+00, 6.10000D+00, 1.00000D+01, 1.10000D+01,
     $ 5.70000D+00, 6.90000D+00, 1.00000D+01, 1.20000D+01, 2.90000D+01,
     $ 5.30000D+01, 7.40000D+01, 1.10000D+02, 1.30000D+02, 1.10000D+02,
     $ 7.90000D+01, 6.00000D+01, 4.00000D+00, 4.00000D+00, 4.00000D+00,
     $ 4.00000D+00, 4.00000D+00, 4.00000D+00, 4.00000D+00, 4.00000D+00,
     $ 4.00000D+00, 4.00000D+00, 4.00000D+00, 4.00000D+00, 4.00000D+00,
     $ 3.30000D+01, 2.60000D+01, 1.30000D+01, 5.20000D+00, 1.00000D+00,
     $ 1.20000D+00, 2.10000D+00, 5.60000D+00, 8.20000D+00, 2.60000D+01,
     $ 4.90000D+01, 5.40000D+01, 5.10000D+01, 3.30000D+01, 8.50000D+00,
     $ 2.20000D+00, 2.00000D+00, 7.20000D-01, 5.30000D-01, 4.10000D-01,
     $ 3.60000D-01, 3.30000D-01, 3.10000D-01, 3.20000D-01, 3.50000D-01,
     $ 4.10000D-01, 4.90000D-01, 5.70000D-01, 6.60000D-01, 7.90000D-01,
     $ 1.00000D+00/
      data (XLMTAB(I,7,3),I=1,48) /
     $ 9.13000D+02, 9.44600D+02, 1.08100D+03, 1.08300D+03, 1.12800D+03,
     $ 1.14500D+03, 1.20100D+03, 1.31750D+03, 1.46900D+03, 1.48800D+03,
     $ 1.52400D+03, 1.62900D+03, 1.63600D+03, 1.63700D+03, 1.72800D+03,
     $ 1.76700D+03, 1.77500D+03, 1.95000D+03, 2.01700D+03, 2.02400D+03,
     $ 2.03100D+03, 2.03800D+03, 2.04600D+03, 2.05400D+03, 2.06200D+03,
     $ 2.07100D+03, 2.07900D+03, 2.08800D+03, 2.10700D+03, 2.12700D+03,
     $ 2.17000D+03, 2.23400D+03, 2.31100D+03, 2.36400D+03, 2.42500D+03,
     $ 2.47100D+03, 2.54700D+03, 2.57500D+03, 2.60400D+03, 2.66700D+03,
     $ 2.70200D+03, 2.73800D+03, 2.77600D+03, 2.90600D+03, 3.12200D+03,
     $ 3.49700D+03, 3.93600D+03, 4.87896D+03/
      data (RCPTAB(I,7,3),I=1,48) /
     $ 7.10000D-01, 8.20000D-01, 1.60000D+00, 2.40000D+00, 3.60000D+00,
     $ 1.80000D+00, 5.30000D+00, 3.00000D+00, 2.80000D+00, 4.30000D-01,
     $ 1.30000D+00, 3.30000D+00, 8.00000D+02, 3.30000D-01, 4.00000D-01,
     $ 5.10000D-01, 1.40000D+00, 3.10000D-01, 4.20000D-01, 6.20000D-01,
     $ 1.20000D+00, 4.00000D+00, 2.30000D+01, 2.70000D+00, 7.70000D-01,
     $ 1.20000D+00, 1.20000D+01, 1.40000D+00, 5.70000D-01, 4.10000D-01,
     $ 3.30000D-01, 3.10000D-01, 3.20000D-01, 3.50000D-01, 4.20000D-01,
     $ 5.80000D-01, 1.70000D+00, 1.50000D+00, 1.10000D+00, 6.70000D-01,
     $ 6.00000D-01, 5.60000D-01, 5.40000D-01, 5.20000D-01, 5.40000D-01,
     $ 6.30000D-01, 7.50000D-01, 1.00000D+00/
      data (XLMTAB(I,8,3),I=1,66) /
     $ 5.00000D+02, 5.50000D+02, 6.00000D+02, 6.50000D+02, 7.00000D+02,
     $ 7.50000D+02, 8.00000D+02, 8.50000D+02, 9.00000D+02, 9.50000D+02,
     $ 1.00000D+03, 1.05000D+03, 1.10000D+03, 1.15000D+03, 1.20000D+03,
     $ 1.25000D+03, 1.30000D+03, 1.35000D+03, 1.40000D+03, 1.45000D+03,
     $ 1.50000D+03, 1.55000D+03, 1.60000D+03, 1.65000D+03, 1.70000D+03,
     $ 1.75000D+03, 1.80000D+03, 1.85000D+03, 1.90000D+03, 1.95000D+03,
     $ 2.00000D+03, 2.05000D+03, 2.10000D+03, 2.15000D+03, 2.20000D+03,
     $ 2.25000D+03, 2.30000D+03, 2.35000D+03, 2.40000D+03, 2.45000D+03,
     $ 2.65000D+03, 2.70000D+03, 2.80000D+03, 2.85000D+03, 2.90000D+03,
     $ 3.05000D+03, 3.10000D+03, 3.15000D+03, 3.25000D+03, 3.30000D+03,
     $ 3.35000D+03, 3.55000D+03, 3.60000D+03, 3.65000D+03, 3.90000D+03,
     $ 3.95000D+03, 4.00000D+03, 4.50000D+03, 4.55000D+03, 4.60000D+03,
     $ 5.10000D+03, 5.15000D+03, 5.20000D+03, 5.60000D+03, 5.65000D+03,
     $ 5.66095D+03/
      data (RCPTAB(I,8,3),I=1,66) /
     $ 7.50000D-02, 9.90000D-02, 1.30000D-01, 1.60000D-01, 2.00000D-01,
     $ 2.40000D-01, 2.90000D-01, 3.50000D-01, 4.10000D-01, 5.30000D-01,
     $ 9.50000D-01, 9.80000D-01, 2.00000D+00, 1.00000D+01, 1.40000D+00,
     $ 6.10000D+00, 8.10000D+00, 8.40000D-01, 1.20000D-01, 4.20000D-02,
     $ 3.40000D-02, 3.20000D-02, 4.40000D-02, 2.30000D+00, 2.40000D-01,
     $ 1.70000D-01, 1.60000D-01, 8.00000D-02, 5.50000D-02, 8.00000D-02,
     $ 6.60000D-02, 7.40000D-02, 8.20000D-02, 8.20000D-02, 9.10000D-02,
     $ 1.00000D-01, 1.00000D-01, 7.70000D-02, 8.40000D-02, 1.10000D-01,
     $ 1.10000D-01, 1.50000D-01, 1.50000D-01, 1.60000D-01, 1.90000D-01,
     $ 1.90000D-01, 2.40000D-01, 2.50000D-01, 2.50000D-01, 2.90000D-01,
     $ 3.10000D-01, 3.10000D-01, 4.00000D-01, 4.10000D-01, 4.10000D-01,
     $ 4.30000D-01, 5.50000D-01, 5.50000D-01, 7.10000D-01, 7.20000D-01,
     $ 7.20000D-01, 8.40000D-01, 9.00000D-01, 9.00000D-01, 1.40000D+00,
     $ 1.00000D+00/
C
C     Helium I
C
      data LIMDAT(4) /ILEVHE1/
      data LLPRNT(4) /.true./
      data (LLABEL(I,4),I=1,ILEVHE1) /
     $     '1S 1S2          ', '3S 2S           ', '1S 2S           ',
     $     '3P0 2P          ', '1P0 2P          ', '3S 3S           ',
     $     '1S 3S           ', '3P0 3P          ', '3D 3D           ',
     $     '1D 3D           ', '1P0 3P          ', '3P0 4P          ',
     $     '1P0 4P          '/
      data (PILEVL(I,4),I=1,ILEVHE1) /1.D0, 3.D0, 1.D0, 9.D0, 3.D0,
     $     3.D0, 1.D0, 9.D0, 15.D0, 5.D0, 3.D0, 64.D0, 100.D0/
      data (XLMTHR(I,4),I=1,ILEVHE1) /504.D0, 2594.D0, 3112.D0,
     $     3410.D0, 3665.D0, 6587.D0, 7378.D0, 7779.D0, 8118.D0,
     $     8120.D0, 8190.D0, 14380.D0, 22340.D0/
      data (CCPLEV(I,4),I=1,ILEVHE1) /7.394D0, 5.48D0, 9.253D0,
     $     15.98D0, 13.48D0, 8.025D0, 14.49D0, 28.52D0, 18.48D0,
     $     18.13D0, 26.99D0, 41.59D0, 41.75D0/
      data (SCPLEV(I,4),I=1,ILEVHE1) /0.D0, 0.D0, 0.D0, 3.D0, 3.6D0,
     $     1.54D0, 1.86D0, 2.6D0, 3.D0, 3.D0, 3.D0, 3.D0, 3.D0/
      data (NPTABL(I,4),I=1,ILEVHE1) /21, 14, 16, 0, 0, 0, 0, 0, 0,
     $     0, 0, 0, 0/
      data (XLMTAB(I,1,4),I=1,21) /100.D0, 130.D0, 145.D0, 160.D0,
     $     175.D0, 190.D0, 200.D0, 215.D0, 225.D0, 239.D0, 256.D0,
     $     276.D0, 298.D0, 325.D0, 348.D0, 395.D0, 443.D0, 454.D0,
     $     480.D0, 493.D0, 504.D0/
      data (RCPTAB(I,1,4),I=1,21) /.036D0, .067D0, .086D0, .107D0,
     $     .13D0, .155D0, .176D0, .207D0, .228D0, .258D0, .295D0,
     $     .342D0, .399D0, .471D0, .537D0, .675D0, .819D0, .852D0,
     $     .93D0, .968D0, 1.D0/
      data (XLMTAB(I,2,4),I=1,14) /357.D0, 424.D0, 467.D0, 588.D0,
     $     675.D0, 792.D0, 959.D0, 1214.D0, 1401.D0, 1518.D0, 1821.D0,
     $     2142.D0, 2427.D0, 2594.D0/
      data (RCPTAB(I,2,4),I=1,14) /.041D0, .05D0, .06D0, .093D0,
     $     .122D0, .166D0, .237D0, .358D0, .452D0, .512D0, .665D0,
     $     .817D0, .936D0, 1.D0/
      data (XLMTAB(I,3,4),I=1,16) /398.D0, 436.D0, 482.D0, 539.D0,
     $     610.D0, 705.D0, 835.D0, 1022.D0, 1317.D0, 1540.D0, 1682.D0,
     $     1852.D0, 2325.D0, 2665.D0, 2875.D0, 3112.D0/
      data (RCPTAB(I,3,4),I=1,16) /.012D0, .017D0, .023D0, .031D0,
     $     .042D0, .059D0, .086D0, .132D0, .222D0, .3D0, .354D0,
     $     .423D0, .628D0, .785D0, .885D0, 1.D0/
C
C     Helium II
C
      data LIMDAT(5) /ILEVHE2/
      data LLPRNT(5) /.true./
      data (LLABEL(I,5),I=1,ILEVHE2) /
     $     '1               ', '2               ', '3               ',
     $     '4               ', '5               ', '6               ',
     $     '7               ', '8               '/
      data (PILEVL(I,5),I=1,ILEVHE2) /2.D0, 8.D0, 18.D0, 32.D0, 50.D0,
     $     72.D0, 98.D0, 128.D0/
      data (XLMTHR(I,5),I=1,ILEVHE2) /227.9D0, 911.6D0, 2051.1D0,
     $     3646.4D0, 5697.5D0, 8204.4D0, 11167.1D0, 14585.6D0/
      data (CCPLEV(I,5),I=1,ILEVHE2) /1.581D0, 3.518D0, 5.395D0,
     $     7.270D0, 9.190D0, 11.8D0, 13.8D0, 15.8D0/
      data (SCPLEV(I,5),I=1,ILEVHE2) /ILEVHE2*3.D0/
      data (NPTABL(I,5),I=1,ILEVHE2) /ILEVHE2*0/
C
C     Aluminum I
C
      data LIMDAT(6) /ILEVAL/
      data LLPRNT(6) /.true./
      data (LLABEL(I,6),I=1,ILEVAL) /
     $     '3p-2P           ', '4s-2S           ', '3p2-4P          ',
     $     '3d-2D           ', '4p-2P*          ', '5s-2S           ',
     $     '4d-2D           ', '5p-2P*          '/
      data (PILEVL(I,6),I=1,ILEVAL) /6.D0, 2.D0, 12.D0, 10.D0, 6.D0,
     $     2.D0, 10.D0, 6.D0/
      data (XLMTHR(I,6),I=1,ILEVAL) /2075.D0, 4356.D0, 5206.D0, 6301.D0,
     $     6517.D0, 9417.D0, 10670.D0, 12460.D0/
      data (CCPLEV(I,6),I=1,ILEVAL) /60.9D0, .105D0, 36.6D0, 30.3D0,
     $     17.5D0, 1.17D0, 10.1D0, 18.4D0/
      data (SCPLEV(I,6),I=1,ILEVAL) /ILEVAL*0.D0/
      data (NPTABL(I,6),I=1,ILEVAL) /19, 101, 19, 138, 98, 71, 79, 74/
      data (XLMTAB(I,1,6),I=1,19) /
     $ 8.71000D+02, 9.02800D+02, 1.04550D+03, 1.18510D+03, 1.34600D+03,
     $ 1.41300D+03, 1.41510D+03, 1.42160D+03, 1.42390D+03, 1.42620D+03,
     $ 1.42850D+03, 1.43080D+03, 1.72840D+03, 1.87850D+03, 1.90370D+03,
     $ 1.91680D+03, 1.93040D+03, 1.94440D+03, 2.06990D+03/
      data (RCPTAB(I,1,6),I=1,19) /
     $ 1.40000D-02, 1.90000D-02, 4.50000D-02, 1.30000D-01, 1.70000D-01,
     $ 4.20000D-01, 3.70000D-01, 1.30000D-01, 7.40000D-02, 3.70000D-02,
     $ 1.80000D-02, 1.10000D-02, 4.50000D-01, 7.10000D-01, 8.00000D-01,
     $ 2.10000D+00, 7.60000D-01, 7.80000D-01, 1.00000D+00/
      data (XLMTAB(I,2,6),I=1,80) /
     $ 8.45700D+02, 8.48000D+02, 8.49700D+02, 8.79900D+02, 1.01550D+03,
     $ 1.01920D+03, 1.02320D+03, 1.02670D+03, 1.02960D+03, 1.03110D+03,
     $ 1.03190D+03, 1.03590D+03, 1.03760D+03, 1.03930D+03, 1.04010D+03,
     $ 1.04100D+03, 1.04190D+03, 1.04280D+03, 1.04370D+03, 1.04460D+03,
     $ 1.04560D+03, 1.04650D+03, 1.04740D+03, 1.04840D+03, 1.04940D+03,
     $ 1.05040D+03, 1.05140D+03, 1.05240D+03, 1.05340D+03, 1.05440D+03,
     $ 1.05550D+03, 1.05660D+03, 1.05770D+03, 1.05870D+03, 1.05980D+03,
     $ 1.06100D+03, 1.06210D+03, 1.06320D+03, 1.06560D+03, 1.06800D+03,
     $ 1.07040D+03, 1.07830D+03, 1.10090D+03, 1.10260D+03, 1.10610D+03,
     $ 1.11150D+03, 1.12520D+03, 1.14760D+03, 1.39360D+03, 1.43960D+03,
     $ 1.48560D+03, 1.50420D+03, 1.52180D+03, 1.53780D+03, 1.55160D+03,
     $ 1.55400D+03, 1.55600D+03, 1.56260D+03, 1.57410D+03, 1.58220D+03,
     $ 1.59050D+03, 1.59910D+03, 1.60360D+03, 1.61270D+03, 1.63200D+03,
     $ 1.64220D+03, 1.65290D+03, 1.65840D+03, 1.66330D+03, 1.67330D+03,
     $ 1.68910D+03, 1.72240D+03, 1.77920D+03, 1.98190D+03, 2.12080D+03,
     $ 2.15090D+03, 2.22160D+03, 2.38030D+03, 2.45360D+03, 2.47380D+03/
      data (XLMTAB(I,2,6),I=81,101) /
     $ 2.50590D+03, 2.56420D+03, 2.62960D+03, 2.64360D+03, 2.67270D+03,
     $ 2.71920D+03, 2.78700D+03, 2.80100D+03, 2.88280D+03, 3.04270D+03,
     $ 3.27500D+03, 3.54080D+03, 3.67950D+03, 3.72980D+03, 3.83740D+03,
     $ 3.95540D+03, 4.01870D+03, 4.08530D+03, 4.22910D+03, 4.30680D+03,
     $ 4.35530D+03/
      data (RCPTAB(I,2,6),I=1,80) /
     $ 1.30000D+00, 1.60000D+00, 1.90000D+00, 2.40000D+00, 8.40000D-01,
     $ 1.10000D+00, 1.70000D+00, 2.40000D+00, 3.20000D+00, 3.90000D+00,
     $ 4.50000D+00, 9.80000D+00, 1.20000D+01, 1.70000D+01, 2.00000D+01,
     $ 2.40000D+01, 3.00000D+01, 3.70000D+01, 4.90000D+01, 6.60000D+01,
     $ 9.10000D+01, 1.30000D+02, 1.90000D+02, 2.50000D+02, 2.90000D+02,
     $ 2.40000D+02, 1.70000D+02, 1.20000D+02, 8.30000D+01, 5.90000D+01,
     $ 4.40000D+01, 3.40000D+01, 2.70000D+01, 2.00000D+01, 1.70000D+01,
     $ 1.40000D+01, 1.20000D+01, 1.00000D+01, 7.80000D+00, 6.10000D+00,
     $ 4.90000D+00, 3.00000D+00, 1.00000D+00, 9.40000D-01, 7.40000D-01,
     $ 5.60000D-01, 3.70000D-01, 3.70000D-01, 3.90000D+00, 2.00000D+01,
     $ 1.90000D+01, 2.00000D+01, 2.50000D+01, 3.40000D+01, 4.50000D+01,
     $ 5.10000D+01, 5.10000D+01, 5.90000D+01, 8.10000D+01, 1.00000D+02,
     $ 1.40000D+02, 1.90000D+02, 2.40000D+02, 3.50000D+02, 1.20000D+03,
     $ 3.20000D+03, 1.50000D+04, 2.00000D+04, 1.10000D+04, 3.00000D+03,
     $ 5.40000D+02, 1.30000D+02, 8.40000D+01, 9.40000D-02, 1.70000D+01,
     $ 1.20000D+01, 6.60000D+00, 1.90000D+00, 3.70000D-01, 9.40000D-02/
      data (RCPTAB(I,2,6),I=81,101) /
     $ 9.40000D-02, 6.60000D+00, 8.60000D+01, 7.20000D+01, 4.00000D+01,
     $ 2.00000D+01, 1.10000D+01, 1.10000D+01, 6.60000D+00, 3.50000D+00,
     $ 1.60000D+00, 4.70000D-01, 1.90000D-01, 9.40000D-02, 1.70000D-03,
     $ 1.70000D-03, 9.40000D-02, 1.90000D-01, 4.70000D-01, 6.60000D-01,
     $ 1.00000D+00/
      data (XLMTAB(I,3,6),I=1,19) /
     $ 6.92800D+02, 8.47500D+02, 8.68300D+02, 8.76000D+02, 9.44800D+02,
     $ 9.45500D+02, 9.79200D+02, 1.02740D+03, 1.02780D+03, 1.02830D+03,
     $ 1.02930D+03, 1.03510D+03, 1.12100D+03, 1.16470D+03, 1.27200D+03,
     $ 1.37510D+03, 1.48190D+03, 1.48100D+03, 5.20560D+03/
      data (RCPTAB(I,3,6),I=1,19) /
     $ 1.70000D-02, 3.50000D-02, 3.90000D-02, 4.10000D-02, 5.70000D-02,
     $ 6.50000D-02, 7.40000D-02, 8.50000D-02, 1.10000D-01, 1.20000D-01,
     $ 1.20000D-01, 1.40000D-01, 1.90000D-01, 2.30000D-01, 3.00000D-01,
     $ 4.00000D-01, 5.40000D-01, 0.00000D+00, 0.00000D+00/
      data (XLMTAB(I,4,6),I=1,80) /
     $ 8.20500D+02, 8.97600D+02, 9.01300D+02, 9.11600D+02, 9.85200D+02,
     $ 9.88100D+02, 9.99400D+02, 1.04790D+03, 1.05150D+03, 1.05540D+03,
     $ 1.05720D+03, 1.05790D+03, 1.05830D+03, 1.05860D+03, 1.05900D+03,
     $ 1.05940D+03, 1.05980D+03, 1.06010D+03, 1.06050D+03, 1.06090D+03,
     $ 1.06130D+03, 1.06210D+03, 1.06330D+03, 1.06370D+03, 1.06410D+03,
     $ 1.06450D+03, 1.06490D+03, 1.06530D+03, 1.06580D+03, 1.06710D+03,
     $ 1.06890D+03, 1.07360D+03, 1.07610D+03, 1.07770D+03, 1.07930D+03,
     $ 1.07990D+03, 1.10620D+03, 1.11040D+03, 1.11400D+03, 1.11490D+03,
     $ 1.11670D+03, 1.11860D+03, 1.12160D+03, 1.12880D+03, 1.16660D+03,
     $ 1.17640D+03, 1.17820D+03, 1.17990D+03, 1.18170D+03, 1.18350D+03,
     $ 1.18530D+03, 1.18720D+03, 1.20090D+03, 1.20300D+03, 1.20520D+03,
     $ 1.20730D+03, 1.20950D+03, 1.22340D+03, 1.34000D+03, 1.40340D+03,
     $ 1.40580D+03, 1.40750D+03, 1.41430D+03, 1.41760D+03, 1.42250D+03,
     $ 1.43450D+03, 1.43840D+03, 1.45090D+03, 1.45580D+03, 1.46090D+03,
     $ 1.47320D+03, 1.48040D+03, 1.49550D+03, 1.51050D+03, 1.51690D+03,
     $ 1.52220D+03, 1.52770D+03, 1.53500D+03, 1.54920D+03, 1.56310D+03/
      data (XLMTAB(I,4,6),I=81,138) /
     $ 1.60450D+03, 1.61670D+03, 1.64360D+03, 1.66180D+03, 1.70730D+03,
     $ 1.72740D+03, 1.77340D+03, 1.79960D+03, 1.82250D+03, 1.84720D+03,
     $ 1.86030D+03, 1.86710D+03, 1.87400D+03, 1.88110D+03, 1.88740D+03,
     $ 1.89380D+03, 1.90030D+03, 1.91380D+03, 1.92070D+03, 2.18210D+03,
     $ 2.18390D+03, 2.18760D+03, 2.19710D+03, 2.21970D+03, 2.26800D+03,
     $ 2.28750D+03, 2.29630D+03, 2.30240D+03, 2.30550D+03, 2.30860D+03,
     $ 2.31170D+03, 2.31490D+03, 2.31820D+03, 2.32140D+03, 2.32480D+03,
     $ 2.32810D+03, 2.33150D+03, 2.33490D+03, 2.33840D+03, 2.34190D+03,
     $ 2.34550D+03, 2.34910D+03, 2.35640D+03, 2.37170D+03, 2.40040D+03,
     $ 3.03560D+03, 3.08530D+03, 3.13870D+03, 3.17660D+03, 3.23720D+03,
     $ 3.30290D+03, 3.39920D+03, 3.59780D+03, 3.92910D+03, 4.39240D+03,
     $ 4.99030D+03, 5.76720D+03, 6.30100D+03/
      data (RCPTAB(I,4,6),I=1,80) /
     $ 2.00000D-02, 3.10000D-02, 3.60000D-02, 3.80000D-02, 5.30000D-02,
     $ 5.10000D-02, 4.70000D-02, 5.80000D-02, 7.80000D-02, 1.10000D-01,
     $ 1.40000D-01, 1.60000D-01, 1.80000D-01, 2.10000D-01, 2.50000D-01,
     $ 2.50000D-01, 1.70000D-01, 1.40000D-01, 1.40000D-01, 1.30000D-01,
     $ 1.20000D-01, 1.20000D-01, 1.10000D-01, 1.00000D-01, 8.70000D-02,
     $ 9.20000D-02, 1.30000D-01, 1.60000D-01, 1.70000D-01, 2.00000D-01,
     $ 2.20000D-01, 2.00000D-01, 1.70000D-01, 1.50000D-01, 9.60000D-02,
     $ 8.80000D-02, 1.10000D-01, 1.20000D-01, 1.50000D-01, 1.50000D-01,
     $ 1.30000D-01, 1.10000D-01, 1.00000D-01, 1.00000D-01, 1.10000D-01,
     $ 1.20000D-01, 1.30000D-01, 1.70000D-01, 3.60000D-01, 1.80000D-01,
     $ 1.40000D-01, 1.30000D-01, 1.10000D-01, 1.60000D-01, 1.20000D-01,
     $ 9.10000D-02, 8.90000D-02, 9.90000D-02, 1.50000D-01, 1.70000D-01,
     $ 2.30000D-01, 2.50000D-01, 2.00000D-01, 2.00000D-01, 2.20000D-01,
     $ 1.70000D-01, 1.80000D-01, 3.20000D-01, 3.90000D-01, 3.90000D-01,
     $ 3.10000D-01, 2.60000D-01, 2.60000D-01, 2.60000D-01, 2.90000D-01,
     $ 3.50000D-01, 4.60000D-01, 6.50000D-01, 1.20000D+00, 2.00000D+00/
      data (RCPTAB(I,4,6),I=81,138) /
     $ 5.00000D+00, 5.30000D+00, 4.50000D+00, 4.40000D+00, 5.30000D+00,
     $ 5.00000D+00, 2.90000D+00, 2.10000D+00, 1.60000D+00, 1.30000D+00,
     $ 1.30000D+00, 1.40000D+00, 1.70000D+00, 1.70000D+00, 1.30000D+00,
     $ 1.00000D+00, 9.40000D-01, 8.30000D-01, 1.00000D-01, 7.30000D-02,
     $ 6.80000D-02, 6.10000D-02, 5.40000D-02, 5.20000D-02, 5.60000D-02,
     $ 6.30000D-02, 7.20000D-02, 8.60000D-02, 9.90000D-02, 1.20000D-01,
     $ 1.60000D-01, 2.30000D-01, 3.90000D-01, 6.40000D-01, 6.70000D-01,
     $ 4.40000D-01, 2.80000D-01, 2.00000D-01, 1.50000D-01, 1.30000D-01,
     $ 1.10000D-01, 1.00000D-01, 8.70000D-02, 7.60000D-02, 7.00000D-02,
     $ 1.60000D-01, 1.90000D-01, 2.70000D-01, 4.30000D-01, 7.90000D-01,
     $ 3.90000D-01, 2.50000D-01, 2.30000D-01, 2.80000D-01, 3.70000D-01,
     $ 5.10000D-01, 7.20000D-01, 1.00000D+00/
      data (XLMTAB(I,5,6),I=1,80) /
     $ 8.15600D+02, 8.77200D+02, 8.78000D+02, 8.83600D+02, 8.84000D+02,
     $ 8.84500D+02, 8.84900D+02, 8.85400D+02, 8.85800D+02, 8.86300D+02,
     $ 8.86800D+02, 8.87200D+02, 8.87700D+02, 8.88600D+02, 8.89100D+02,
     $ 8.99300D+02, 9.14700D+02, 9.64600D+02, 9.73700D+02, 9.86900D+02,
     $ 1.08420D+03, 1.18870D+03, 1.20840D+03, 1.26180D+03, 1.28790D+03,
     $ 1.32250D+03, 1.43000D+03, 1.54080D+03, 1.55810D+03, 1.57220D+03,
     $ 1.58370D+03, 1.59390D+03, 1.60260D+03, 1.60940D+03, 1.61410D+03,
     $ 1.61890D+03, 1.62140D+03, 1.62390D+03, 1.62640D+03, 1.62890D+03,
     $ 1.63160D+03, 1.63420D+03, 1.63680D+03, 1.63960D+03, 1.64230D+03,
     $ 1.64510D+03, 1.64790D+03, 1.65080D+03, 1.65370D+03, 1.65660D+03,
     $ 1.65960D+03, 1.66270D+03, 1.66570D+03, 1.66890D+03, 1.67210D+03,
     $ 1.67530D+03, 1.67850D+03, 1.68190D+03, 1.68520D+03, 1.68870D+03,
     $ 1.69220D+03, 1.69570D+03, 1.69930D+03, 1.70290D+03, 1.71040D+03,
     $ 1.71810D+03, 1.72610D+03, 1.73860D+03, 1.75170D+03, 1.76550D+03,
     $ 1.78010D+03, 1.80080D+03, 1.82310D+03, 1.85340D+03, 1.88710D+03,
     $ 1.92740D+03, 1.93440D+03, 2.53410D+03, 2.57160D+03, 2.59170D+03/
      data (XLMTAB(I,5,6),I=81,98) /
     $ 2.60570D+03, 2.62010D+03, 2.62740D+03, 2.63500D+03, 2.64260D+03,
     $ 2.65040D+03, 2.68280D+03, 2.70860D+03, 2.75490D+03, 3.12090D+03,
     $ 3.49640D+03, 3.98700D+03, 4.99260D+03, 5.08410D+03, 5.18070D+03,
     $ 5.39060D+03, 6.19670D+03, 6.51610D+03/
      data (RCPTAB(I,5,6),I=1,80) /
     $ 5.70000D-03, 9.70000D-03, 1.10000D-02, 2.30000D-02, 2.50000D-02,
     $ 2.90000D-02, 3.50000D-02, 4.50000D-02, 6.00000D-02, 8.10000D-02,
     $ 1.10000D-01, 1.20000D-01, 1.10000D-01, 6.80000D-02, 5.40000D-02,
     $ 1.60000D-02, 1.40000D-02, 2.00000D-02, 2.60000D-02, 1.80000D-01,
     $ 2.80000D-02, 2.20000D-02, 2.60000D-02, 4.00000D-02, 5.10000D-02,
     $ 5.80000D-02, 1.20000D-01, 4.10000D-01, 5.60000D-01, 7.60000D-01,
     $ 1.00000D+00, 1.40000D+00, 1.80000D+00, 2.40000D+00, 3.00000D+00,
     $ 4.00000D+00, 4.80000D+00, 6.00000D+00, 8.30000D+00, 1.40000D+01,
     $ 3.40000D+01, 3.60000D+01, 1.90000D+01, 1.70000D+01, 1.90000D+01,
     $ 2.60000D+01, 3.60000D+01, 5.30000D+01, 7.30000D+01, 8.80000D+01,
     $ 9.30000D+01, 7.90000D+01, 5.40000D+01, 3.40000D+01, 2.20000D+01,
     $ 1.50000D+01, 1.10000D+01, 8.20000D+00, 6.30000D+00, 5.00000D+00,
     $ 4.00000D+00, 3.30000D+00, 2.70000D+00, 2.30000D+00, 1.70000D+00,
     $ 1.30000D+00, 9.90000D-01, 7.00000D-01, 5.20000D-01, 3.90000D-01,
     $ 3.00000D-01, 2.20000D-01, 1.70000D-01, 1.30000D-01, 1.00000D-01,
     $ 8.30000D-02, 1.70000D-02, 1.20000D-01, 1.20000D-01, 1.00000D-01/
      data (RCPTAB(I,5,6),I=81,98) /
     $ 8.10000D-02, 4.60000D-02, 3.00000D-02, 2.20000D-02, 2.30000D-02,
     $ 3.10000D-02, 7.60000D-02, 1.00000D-01, 1.30000D-01, 2.50000D-01,
     $ 3.70000D-01, 5.60000D-01, 9.20000D-01, 8.50000D-01, 9.40000D-01,
     $ 9.90000D-01, 1.10000D+00, 1.00000D+00/
      data (XLMTAB(I,6,6),I=1,71) /
     $ 8.48500D+02, 9.18700D+02, 9.23300D+02, 9.27800D+02, 9.31100D+02,
     $ 9.32800D+02, 9.33300D+02, 9.38800D+02, 9.43500D+02, 9.53000D+02,
     $ 9.90700D+02, 1.31670D+03, 1.34220D+03, 1.43880D+03, 1.56890D+03,
     $ 1.57730D+03, 1.62210D+03, 1.64460D+03, 1.65740D+03, 1.66420D+03,
     $ 1.66950D+03, 1.67500D+03, 1.67690D+03, 1.67880D+03, 1.68270D+03,
     $ 1.68870D+03, 1.69480D+03, 1.70130D+03, 1.71250D+03, 1.73730D+03,
     $ 1.80890D+03, 1.87550D+03, 2.06450D+03, 2.08160D+03, 2.12170D+03,
     $ 3.09710D+03, 3.28130D+03, 3.48900D+03, 3.57220D+03, 3.61710D+03,
     $ 3.68880D+03, 3.73980D+03, 3.79370D+03, 3.85080D+03, 3.88050D+03,
     $ 3.94270D+03, 4.00870D+03, 4.15360D+03, 4.27540D+03, 4.56060D+03,
     $ 4.91880D+03, 5.13440D+03, 5.29520D+03, 5.38150D+03, 5.66710D+03,
     $ 5.88360D+03, 6.25690D+03, 6.39650D+03, 6.54510D+03, 6.70330D+03,
     $ 6.87220D+03, 7.05280D+03, 7.24650D+03, 7.45470D+03, 7.67900D+03,
     $ 7.92150D+03, 8.18430D+03, 8.47030D+03, 8.78240D+03, 9.12450D+03,
     $ 9.41640D+03/
      data (RCPTAB(I,6,6),I=1,71) /
     $ 3.40000D-02, 2.20000D-01, 2.50000D-01, 3.10000D-01, 3.80000D-01,
     $ 4.70000D-01, 5.20000D-01, 1.50000D+00, 5.80000D+00, 9.30000D-01,
     $ 7.70000D-02, 6.90000D-02, 9.30000D-02, 3.20000D-01, 2.60000D+00,
     $ 3.90000D+00, 1.40000D+01, 3.40000D+01, 7.60000D+01, 1.50000D+02,
     $ 3.50000D+02, 1.70000D+03, 3.80000D+03, 5.00000D+03, 1.00000D+03,
     $ 2.20000D+02, 8.60000D+01, 4.70000D+01, 2.50000D+01, 1.10000D+01,
     $ 2.00000D+00, 9.30000D-01, 4.20000D-02, 1.70000D+00, 9.20000D-01,
     $ 6.00000D-02, 1.70000D-02, 0.00000D+00, 3.40000D-02, 7.70000D-02,
     $ 2.60000D-01, 6.10000D-01, 1.60000D+00, 4.10000D+00, 4.40000D+00,
     $ 2.50000D+00, 1.20000D+00, 4.70000D-01, 2.60000D-01, 1.00000D-01,
     $ 3.40000D-02, 1.70000D-02, 8.50000D-03, 0.00000D+00, 0.00000D+00,
     $ 8.50000D-03, 2.60000D-02, 3.40000D-02, 5.10000D-02, 6.90000D-02,
     $ 9.30000D-02, 1.20000D-01, 1.60000D-01, 2.00000D-01, 2.60000D-01,
     $ 3.20000D-01, 4.10000D-01, 5.10000D-01, 6.40000D-01, 8.00000D-01,
     $ 1.00000D+00/
      data (XLMTAB(I,7,6),I=1,79) /
     $ 7.33900D+02, 7.70600D+02, 9.02500D+02, 9.32900D+02, 9.41500D+02,
     $ 9.42000D+02, 9.43800D+02, 9.44500D+02, 9.45100D+02, 9.46300D+02,
     $ 9.48200D+02, 9.51600D+02, 9.58900D+02, 1.06550D+03, 1.27060D+03,
     $ 1.27270D+03, 1.27470D+03, 1.27680D+03, 1.27890D+03, 1.28110D+03,
     $ 1.29240D+03, 1.29720D+03, 1.29960D+03, 1.30210D+03, 1.30460D+03,
     $ 1.32060D+03, 1.41820D+03, 1.46100D+03, 1.53630D+03, 1.54330D+03,
     $ 1.55220D+03, 1.57660D+03, 1.57870D+03, 1.58590D+03, 1.58740D+03,
     $ 1.62940D+03, 1.64770D+03, 1.66900D+03, 1.68180D+03, 1.70520D+03,
     $ 1.70910D+03, 1.71720D+03, 1.74650D+03, 1.77530D+03, 1.78990D+03,
     $ 1.81210D+03, 1.83670D+03, 1.84800D+03, 1.89900D+03, 1.93910D+03,
     $ 1.96720D+03, 2.07700D+03, 2.10190D+03, 2.11050D+03, 2.11930D+03,
     $ 2.12840D+03, 2.13650D+03, 2.14470D+03, 2.15310D+03, 2.17040D+03,
     $ 2.17930D+03, 2.23930D+03, 3.04150D+03, 3.18980D+03, 3.56080D+03,
     $ 3.81180D+03, 3.89360D+03, 3.98260D+03, 4.01410D+03, 4.04640D+03,
     $ 4.11400D+03, 4.18600D+03, 4.22360D+03, 4.30270D+03, 4.38720D+03,
     $ 4.62590D+03, 5.26500D+03, 9.30270D+03, 1.06640D+04/
      data (RCPTAB(I,7,6),I=1,79) /
     $ 2.20000D-02, 2.90000D-02, 5.50000D-02, 1.20000D-01, 3.60000D-01,
     $ 3.40000D-01, 2.00000D-01, 1.70000D-01, 1.50000D-01, 1.20000D-01,
     $ 1.10000D-01, 1.00000D-01, 8.70000D-02, 1.40000D-01, 2.90000D-01,
     $ 3.40000D-01, 7.10000D-01, 4.30000D-01, 3.50000D-01, 3.30000D-01,
     $ 2.80000D-01, 2.80000D-01, 4.10000D-01, 2.90000D-01, 2.40000D-01,
     $ 2.70000D-01, 3.80000D-01, 4.60000D-01, 5.70000D-01, 1.80000D+00,
     $ 1.90000D+00, 4.00000D-01, 4.10000D-01, 1.00000D+00, 1.30000D+00,
     $ 2.20000D+01, 2.10000D+01, 2.50000D+01, 2.50000D+01, 2.00000D+01,
     $ 2.60000D+01, 1.80000D+01, 1.30000D+01, 1.20000D+01, 1.10000D+01,
     $ 6.40000D+00, 2.90000D+00, 2.10000D+00, 1.20000D+00, 1.60000D+00,
     $ 1.60000D+00, 1.30000D+00, 1.30000D+00, 1.40000D+00, 1.70000D+00,
     $ 1.90000D+00, 1.50000D+00, 1.30000D+00, 1.20000D+00, 1.10000D+00,
     $ 4.10000D-01, 3.10000D-01, 3.60000D-01, 3.40000D-01, 3.80000D-01,
     $ 5.00000D-01, 7.00000D-01, 1.60000D+00, 2.20000D+00, 2.30000D+00,
     $ 1.50000D+00, 1.00000D+00, 8.90000D-01, 7.50000D-01, 6.90000D-01,
     $ 6.40000D-01, 6.80000D-01, 1.00000D+00, 1.00000D+00/
      data (XLMTAB(I,8,6),I=1,74) /
     $ 8.67200D+02, 9.07600D+02, 9.11500D+02, 9.52000D+02, 9.59200D+02,
     $ 9.74400D+02, 1.03710D+03, 1.04830D+03, 1.05210D+03, 1.07750D+03,
     $ 1.08900D+03, 1.15580D+03, 1.16320D+03, 1.18150D+03, 1.19440D+03,
     $ 1.24250D+03, 1.29730D+03, 1.32280D+03, 1.37220D+03, 1.40510D+03,
     $ 1.59170D+03, 1.60210D+03, 1.60910D+03, 1.61820D+03, 1.62680D+03,
     $ 1.63350D+03, 1.65030D+03, 1.65340D+03, 1.65540D+03, 1.65760D+03,
     $ 1.66890D+03, 1.67120D+03, 1.67250D+03, 1.67370D+03, 1.67490D+03,
     $ 1.67870D+03, 1.68130D+03, 1.68390D+03, 1.68660D+03, 1.68930D+03,
     $ 1.69350D+03, 1.69930D+03, 1.70690D+03, 1.71660D+03, 1.72340D+03,
     $ 1.73050D+03, 1.73990D+03, 1.74980D+03, 1.76240D+03, 1.77610D+03,
     $ 1.79330D+03, 1.81210D+03, 1.82970D+03, 1.89640D+03, 1.90870D+03,
     $ 1.93070D+03, 1.96970D+03, 2.03300D+03, 2.11870D+03, 2.24310D+03,
     $ 2.66900D+03, 3.17610D+03, 3.26260D+03, 3.43360D+03, 3.98460D+03,
     $ 4.55240D+03, 5.23000D+03, 6.23750D+03, 7.85640D+03, 8.08550D+03,
     $ 8.33250D+03, 8.59960D+03, 1.13172D+04, 1.24520D+04/
      data (RCPTAB(I,8,6),I=1,74) /
     $ 3.30000D-03, 2.70000D-03, 6.00000D-02, 6.00000D-02, 1.50000D-02,
     $ 5.40000D-03, 4.90000D-03, 1.00000D-02, 3.50000D-02, 7.10000D-03,
     $ 7.60000D-03, 8.70000D-03, 3.80000D-02, 1.10000D-02, 1.10000D-02,
     $ 1.60000D-02, 1.30000D-02, 1.60000D-02, 2.40000D-02, 3.20000D-02,
     $ 3.90000D-01, 5.20000D-01, 5.70000D-01, 7.10000D-01, 9.50000D-01,
     $ 1.30000D+00, 3.50000D+00, 4.70000D+00, 6.00000D+00, 8.10000D+00,
     $ 5.20000D+01, 1.10000D+02, 1.90000D+02, 2.40000D+02, 2.20000D+02,
     $ 7.20000D+01, 3.40000D+01, 1.90000D+01, 1.20000D+01, 7.80000D+00,
     $ 4.80000D+00, 2.80000D+00, 1.60000D+00, 9.60000D-01, 7.10000D-01,
     $ 5.30000D-01, 3.90000D-01, 2.90000D-01, 2.10000D-01, 1.60000D-01,
     $ 1.20000D-01, 9.00000D-02, 7.70000D-02, 3.90000D-02, 3.30000D-02,
     $ 2.80000D-02, 2.30000D-02, 2.00000D-02, 1.90000D-02, 2.10000D-02,
     $ 2.90000D-02, 6.50000D-02, 1.30000D-02, 8.30000D-02, 1.60000D-01,
     $ 2.30000D-01, 3.30000D-01, 5.00000D-01, 7.50000D-01, 7.10000D-01,
     $ 7.90000D-01, 8.20000D-01, 1.00000D+00, 1.00000D+00/
C
C     Magnesium I
C
      data LIMDAT(7) /ILEVMG/
      data LLPRNT(7) /.true./
      data (LLABEL(I,7),I=1,ILEVMG) /
     $     '3S2 1S          ', '3P 3P0          ', '3P 1P0          ',
     $     '4S 3S           ', '4S 1S           ', '3D 1D           ',
     $     '4P 3P0          ', '3D 3D           '/
      data (PILEVL(I,7),I=1,ILEVMG) /1.D0, 9.D0, 3.D0, 3.D0, 1.D0, 5.D0,
     $     9.D0, 15.D0/
      data (XLMTHR(I,7),I=1,ILEVMG) /1621.51D0, 2515.1D0, 3756.61D0,
     $     4884.33D0, 5504.28D0, 6549.66D0, 7235.95D0, 7291.82D0/
      data (CCPLEV(I,7),I=1,ILEVMG) /2.55D0, 20.2D0, 213.3D0, 2.2D0,
     $     34.D0, 40.1D0, 26.3D0, 34.2D0/
      data (SCPLEV(I,7),I=1,ILEVMG) /0.D0, 0.D0, 0.D0, 2.2D0, 0.D0,
     $     3.D0, 3.D0, 3.D0/
      data (NPTABL(I,7),I=1,ILEVMG) /10, 6, 10, 0, 8, 0, 0, 0/
      data (XLMTAB(I,1,7),I=1,10) /1200.D0, 1214.5D0, 1217.D0, 1265.D0,
     $     1300.D0, 1341.D0, 1400.D0, 1500.D0, 1550.D0, 1621.5D0/
      data (RCPTAB(I,1,7),I=1,10) /.096D0, .135D0, .135D0, .36D0,
     $     .13D0, 0.D0, .12D0, .36D0, .56D0, 1.D0/
      data (XLMTAB(I,2,7),I=1,6) /1195.D0, 1217.D0, 1316.D0, 1620.D0,
     $     1970.D0, 2513.8D0/
      data (RCPTAB(I,2,7),I=1,6) /.04D0, .045D0, .069D0, .109D0, .45D0,
     $     1.D0/
      data (XLMTAB(I,3,7),I=1,10) /2059.D0, 2660.D0, 2977.D0, 2987.D0,
     $     3009.D0, 3032.D0, 3042.D0, 3200.D0, 3647.D0, 3756.6D0/
      data (RCPTAB(I,3,7),I=1,10) /.063D0, .603D0, .88D0, 1.075D0,
     $     3.482D0, 1.078D0, .88D0, .907D0, .981D0, 1.D0/
      data (XLMTAB(I,5,7),I=1,8) /2000.D0, 2500.D0, 3000.D0, 3500.D0,
     $     4000.D0, 4500.D0, 5000.D0, 5504.3D0/
      data (RCPTAB(I,5,7),I=1,8) /1.31D0, .992D0, .625D0, .264D0,
     $     .0429D0, .0457D0, .354D0, 1.D0/
C
C     Iron I
C
      data LIMDAT(8) /ILEVFE/
      data LLPRNT(8) /.true./
      data (LLABEL(I,8),I=1,ILEVFE) /
     $     'A 5D            ', 'A 5F            ', 'A 3F            ',
     $     'A 5P            ', 'A 3P            ', 'Z 7D0           ',
     $     'A 3H            ', 'B 3F            '/
      data (PILEVL(I,8),I=1,ILEVFE) /25.D0, 35.D0, 21.D0, 15.D0, 9.D0,
     $     35.D0, 33.D0, 21.D0/
      data (XLMTHR(I,8),I=1,ILEVFE) /1576.D0, 1769.D0, 1942.D0, 2178.D0,
     $     2218.D0, 2267.D0, 2269.D0, 2335.D0/
      data (CCPLEV(I,8),I=1,ILEVFE) /1.D0, 5.D0, 3.D0, 3.D0, 0.44D0,
     $     1.7D0, 1.61D0, 1.02D0/
      data (SCPLEV(I,8),I=1,ILEVFE) /ILEVFE*0.D0/
      data (NPTABL(I,8),I=1,ILEVFE) /28, 9, 5, 5, 5, 9, 5, 5/
      data (XLMTAB(I,1,8),I=1,28) /1150.D0, 1200.D0, 1250.D0, 1280.D0,
     $     1305.D0, 1325.D0, 1334.D0, 1350.D0, 1355.D0, 1373.D0,
     $     1400.D0, 1430.D0, 1510.D0, 1530.D0, 1535.D0, 1540.D0,
     $     1545.D0, 1552.D0, 1560.D0, 1575.D0, 1576.D0, 1585.D0,
     $     1586.D0, 1593.D0, 1594.D0, 1597.D0, 1598.D0, 1600.D0/
      data (RCPTAB(I,1,8),I=1,28) /.4D0, .6D0, 2.3D0, 6.7D0, 2.8D0,
     $     10.8D0, 6.3D0, 9.2D0, 5.4D0, 11.3D0, 2.5D0, 1.2D0, .9D0,
     $     3.8D0, 5.3D0, 5.D0, 4.4D0, 7.7D0, 2.9D0, 1.D0, .64D0, .64D0,
     $     .36D0, .36D0, .16D0, .16D0, .03D0, .03D0/
      data (XLMTAB(I,2,8),I=1,9) /1768.D0, 1769.D0, 1782.D0, 1783.D0,
     $     1793.D0, 1794.D0, 1802.D0, 1803.D0, 1808.D0/
      data (RCPTAB(I,2,8),I=1,9) /1.D0, .69D0, .69D0, .43D0, .43D0,
     $     .23D0, .23D0, .09D0, .09D0/
      data (XLMTAB(I,3,8),I=1,5) /1941.D0, 1942.D0, 1963.D0, 1964.D0,
     $     1980.D0/
      data (RCPTAB(I,3,8),I=1,5) /1.D0, .57D0, .57D0, .24D0, .24D0/
      data (XLMTAB(I,4,8),I=1,5) /2177.D0, 2178.D0, 2185.D0, 2186.D0,
     $     2195.D0/
      data (RCPTAB(I,4,8),I=1,5) /1.D0, .53D0, .53D0, .2D0, .2D0/
      data (XLMTAB(I,5,8),I=1,5) /2217.D0, 2218.D0, 2276.D0, 2277.D0,
     $     2302.D0/
      data (RCPTAB(I,5,8),I=1,5) /1.D0, .45D0, .45D0, .11D0, .11D0/
      data (XLMTAB(I,6,8),I=1,9) /2266.D0, 2267.D0, 2277.D0, 2278.D0,
     $     2287.D0, 2288.D0, 2295.D0, 2296.D0, 2301.D0/
      data (RCPTAB(I,6,8),I=1,9) /1.D0, .69D0, .69D0, .43D0, .43D0,
     $     .23D0, .23D0, .09D0, .09D0/
      data (XLMTAB(I,7,8),I=1,5) /2268.D0, 2269.D0, 2280.D0, 2281.D0,
     $     2289.D0/
      data (RCPTAB(I,7,8),I=1,5) /1.D0, .61D0, .61D0, .27D0, .27D0/
      data (XLMTAB(I,8,8),I=1,5) /2334.D0, 2335.D0, 2347.D0, 2348.D0,
     $     2356.D0/
      data (RCPTAB(I,8,8),I=1,5) /1.D0, .57D0, .57D0, .24D0, .24D0/
C
C     Sodium I
C
      data LIMDAT(9) /ILEVNA/
      data LLPRNT(9) /.true./
      data (LLABEL(I,9),I=1,ILEVNA) /
     $     '3S 2S           ', '3P 2P1/2        ', '3P 2P3/2        ',
     $     '4S 2S           ', '3D 2D5/2        ', '3D 2D3/2        ',
     $     '4P 2P1/2        ', '4P 2P3/2        '/
      data (PILEVL(I,9),I=1,ILEVNA) /2.D0, 2.D0, 4.D0, 2.D0, 6.D0, 4.D0,
     $     2.D0, 4.D0/
      data (XLMTHR(I,9),I=1,ILEVNA) /2412.6D0, 4082.7D0, 4085.5D0,
     $     6365.6D0, 8147.4D0, 8147.6D0, 8942.6D0, 8947.4D0/
      data (CCPLEV(I,9),I=1,ILEVNA) /0.13D0, 7.93D0, 7.93D0, 0.9D0,
     $     3.61D0, 3.61D0, 18.7D0, 18.7D0/
      data (SCPLEV(I,9),I=1,ILEVNA) /0.D0, 4.14D0, 0.D0, 0.D0, 3.D0,
     $     3.D0, 3.D0, 3.D0/
      data (NPTABL(I,9),I=1,ILEVNA) /33, 0, 4, 5, 0, 0, 0, 0/
      data (XLMTAB(I,1,9),I=1,33) /575.D0, 660.D0, 775.D0, 900.D0,
     $     1000.D0, 1100.D0, 1191.7D0, 1209.7D0, 1215.7D0, 1219.7D0,
     $     1221.7D0, 1239.7D0, 1300.D0, 1350.D0, 1400.D0, 1450.D0,
     $     1500.D0, 1550.D0, 1600.D0, 1650.D0, 1700.D0, 1750.D0,
     $     1800.D0, 1850.D0, 1900.D0, 2000.D0, 2050.D0, 2100.D0,
     $     2150.D0, 2200.D0, 2250.D0, 2350.D0, 2412.6D0/
      data (RCPTAB(I,1,9),I=1,33) /1.196D0, 1.326D0, 1.435D0, 1.478D0,
     $     1.478D0, 1.402D0, 1.3D0, 1.27D0, 1.26D0, 1.25D0, 1.23D0,
     $     1.21D0, 1.12D0, 1.033D0, .949D0, .848D0, .75D0, .652D0,
     $     .554D0, .435D0, .293D0, .141D0, .043D0, .011D0, 0.D0, 0.D0,
     $     .011D0, .065D0, .174D0, .348D0, .543D0, .848D0, 1.D0/
      data (XLMTAB(I,3,9),I=1,4) /1400.D0, 2830.D0, 3666.D0, 4082.7D0/
      data (RCPTAB(I,3,9),I=1,4) /.1D0, .228D0, .657D0, 1.D0/
      data (XLMTAB(I,4,9),I=1,5) /4200.D0, 5000.D0, 5440.D0, 6140.D0,
     $     6365.6D0/
      data (RCPTAB(I,4,9),I=1,5) /0.D0, .17D0, .28D0, .64D0, 1.D0/
C
C     Calcium I
C
      data LIMDAT(10) /ILEVCA/
      data LLPRNT(10) /.true./
      data (LLABEL(I,10),I=1,ILEVCA) /
     $     '4S2 1S          ', '4S3/2 3P0       ', '4P5/2 3P0       ',
     $     '3D 3D           ', '3D 1D           ', '4P 1P0          ',
     $     '5S 3S           ', '4P 1P0          '/
      data (PILEVL(I,10),I=1,ILEVCA) /1.D0, 3.D0, 6.D0, 15.D0, 5.D0,
     $     3.D0, 3.D0, 3.D0/
      data (XLMTHR(I,10),I=1,ILEVCA) /2028.2D0, 2932.9D0, 2939.8D0,
     $     3454.3D0, 3642.2D0, 3898.1D0, 5627.8D0, 7952.9D0/
      data (CCPLEV(I,10),I=1,ILEVCA) /1.D0, 6.2D0, 12.2D0, 5.D0,
     $     10.3D0, 7.49D0, 2.86D0, 51.8D0/
      data (SCPLEV(I,10),I=1,ILEVCA) /0.D0, 20.D0, 20.D0, 14.D0, 3.D0,
     $     2.4D0, 3.D0, 3.D0/
      data (NPTABL(I,10),I=1,ILEVCA) /30, 0, 0, 0, 0, 0, 0, 0/
      data (XLMTAB(I,1,10),I=1,30) /1342.D0, 1415.D0, 1540.D0, 1587.D0,
     $     1690.D0, 1709.D0, 1722.D0, 1739.D0, 1749.D0, 1750.4D0,
     $     1751.3D0, 1752.1D0, 1754.D0, 1764.D0, 1773.D0, 1799.D0,
     $     1817.D0, 1835.D0, 1844.D0, 1863.D0, 1868.D0, 1873.D0,
     $     1892.D0, 1912.D0, 1922.D0, 1943.D0, 1974.D0, 2001.D0,
     $     2012.D0, 2028.2D0/
      data (RCPTAB(I,1,10),I=1,30) /0.9D0, 1.3D0, 2.22D0, 2.08D0,
     $     1.06D0, 0.61D0, 0.14D0, 1.18D0, 19.95D0, 100.71D0,
     $     130.21D0, 52.64D0, 10.69D0, 0.07D0, 0.02D0, 0.69D0, 1.76D0,
     $     4.57D0, 8.04D0, 32.28D0, 38.4D0, 33.7D0, 5.45D0, 0.88D0,
     $     0.29D0, 0.D0, 0.5D0, 1.48D0, 2.05D0, 3.5D0/
C
C     Oxygen I
C
      data LIMDAT(11) /ILEVO/
      data LLPRNT(11) /.true./
      data (LLABEL(I,11),I=1,ILEVO) /
     $      '2p4 3P          ', '2p4 3P          ', '2p4 3P          ',
     $      '2p4 1D          ', '2p4 1S          ', '3s 5S0          ',
     $      '3s 3S0          ', '3p 5P           ', '3p 3P           ',
     $      '4s 5S0          ', '4s 3S0          ', '3d 5D0          ',
     $      '3d 3D0          ', '   1D0          '/
      data (PILEVL(I,11),I=1,ILEVO) /5.D0, 3.D0, 1.D0, 5.D0, 1.D0,
     $      5.D0, 3.D0, 1.5D1, 9.D0, 5.D0, 3.D0, 2.5D1, 1.5D1, 5.D0/
      data (XLMTHR(I,11),I=1,ILEVO) /910.441D0, 911.754D0, 912.326D0,
     $      1064.18D0, 1315.03D0, 2772.49D0, 3026.46D0, 4308.88D0,
     $      4715.65D0, 6963.67D0, 7346.50D0, 8053.97D0, 8098.4D0,
     $      13950.D0/
      data (CCPLEV(I,11),I=1,ILEVO) /1.23D0, 1.23D0, 1.23D0, 4.64D0,
     $      7.65D0, 0.005D0, 0.069D0, 4.65D0, 2.43D0, 3.D0, 3.D0,
     $      3.D0, 19.8D0, 3.D0/
      data (SCPLEV(I,11),I=1,ILEVO) /9*0.D0, 3*3.D0, 0.D0, 3.D0/
      data (NPTABL(I,11),I=1,ILEVO) /23, 23, 23, 7, 8, 8, 4, 6, 7,
     $      0, 0, 0, 10, 0/
      data (XLMTAB(I,1,11),I=1,23) /125.D0, 200.D0, 300.D0, 400.D0,
     $      450.D0, 455.D0, 465.D0, 480.D0, 510.D0, 600.D0, 625.D0,
     $      650.D0, 665.D0, 665.1D0, 665.9D0, 666.D0, 700.D0, 732.D0,
     $      732.1D0, 732.9D0, 733.D0, 800.D0, 910.441D0/
      data (RCPTAB(I,1,11),I=1,23) /.385D0, .974D0, 1.887D0, 2.641D0,
     $      2.897D0, 3.128D0, 2.872D0, 3.256D0, 3.077D0, 3.359D0,
     $      3.41D0, 3.231D0, 2.974D0, 2.897D0, 2.359D0, 2.282D0,
     $      2.128D0, 1.974D0, 1.897D0, 1.308D0, 1.231D0, 1.051D0,
     $      1.0D0/
      data (XLMTAB(I,2,11),I=1,23) /125.D0, 200.D0, 300.D0, 400.D0,
     $      450.D0, 455.D0, 465.D0, 480.D0, 510.D0, 600.D0, 625.D0,
     $      650.D0, 665.D0, 665.1D0, 665.9D0, 666.D0, 700.D0, 732.D0,
     $      732.1D0, 732.9D0, 733.D0, 800.D0, 911.754D0/
      data (RCPTAB(I,2,11),I=1,23) /.385D0, .974D0, 1.887D0, 2.641D0,
     $      2.897D0, 3.128D0, 2.872D0, 3.256D0, 3.077D0, 3.359D0,
     $      3.41D0, 3.231D0, 2.974D0, 2.897D0, 2.359D0, 2.282D0,
     $      2.128D0, 1.974D0, 1.897D0, 1.308D0, 1.231D0, 1.051D0,
     $      1.0D0/
      data (XLMTAB(I,3,11),I=1,23) /125.D0, 200.D0, 300.D0, 400.D0,
     $      450.D0, 455.D0, 465.D0, 480.D0, 510.D0, 600.D0, 625.D0,
     $      650.D0, 665.D0, 665.1D0, 665.9D0, 666.D0, 700.D0, 732.D0,
     $      732.1D0, 732.9D0, 733.D0, 800.D0, 912.326D0/
      data (RCPTAB(I,3,11),I=1,23) /.385D0, .974D0, 1.887D0, 2.641D0,
     $      2.897D0, 3.128D0, 2.872D0, 3.256D0, 3.077D0, 3.359D0,
     $      3.41D0, 3.231D0, 2.974D0, 2.897D0, 2.359D0, 2.282D0,
     $      2.128D0, 1.974D0, 1.897D0, 1.308D0, 1.231D0, 1.051D0,
     $      1.0D0/
      data (XLMTAB(I,4,11),I=1,7) /503.D0, 505.D0, 743.839D0, 743.85D0,
     $      827.9D0, 827.91D0, 1064.18D0/
      data (RCPTAB(I,4,11),I=1,7) /2.078D0, 2.081D0, 1.776D0, 1.359D0,
     $      1.0D0, 0.0D0, 0.0D0/
      data (XLMTAB(I,5,11),I=1,8) /503.D0, 505.D0, 800.D0, 911.D0,
     $      913.D0, 972.17D0, 972.18D0, 1315.03D0/
      data (RCPTAB(I,5,11),I=1,8) /1.113D0, 1.116D0, 1.292D0, 1.143D0,
     $      1.139D0, 1.0D0, 0.0D0, 0.0D0/
      data (XLMTAB(I,6,11),I=1,8) /1059.D0, 1225.D0, 1250.D0, 1500.D0,
     $      1700.D0, 2000.D0, 2500.D0, 2772.49D0/
      data (RCPTAB(I,6,11),I=1,8) /30.D0, 30.D0, 31.D0, 35.D0, 33.D0,
     $      24.D0, 6.8D0, 1.0D0/
      data (XLMTAB(I,7,11),I=1,4) /2000.D0, 2500.D0, 3000.D0,
     $      3026.46D0/
      data (RCPTAB(I,7,11),I=1,4) /.0007D0, .1059D0, .9112D0, 1.0D0/
      data (XLMTAB(I,8,11),I=1,6) /2000.D0, 2500.D0, 3000.D0, 3500.D0,
     $      4000.D0, 4308.88D0/
      data (RCPTAB(I,8,11),I=1,6) /.06D0, .0842D0, .1912D0, .3852D0,
     $      .71D0, 1.0D0/
      data (XLMTAB(I,9,11),I=1,7) /2000.D0, 2500.D0, 3000.D0, 3500.D0,
     $      4000.D0, 4500.D0, 4715.65D0/
      data (RCPTAB(I,9,11),I=1,7) /.25D0, .26D0, .295D0, .35D0, .6D0,
     $      .9D0, 1.0D0/
      data (XLMTAB(I,13,11),I=1,10) /2500.D0, 3000.D0, 3500.D0, 4000.D0,
     $      4500.D0, 5000.D0, 5500.D0, 6000.D0, 7000.D0, 8098.4D0/
      data (RCPTAB(I,13,11),I=1,10) /.0169D0, .0323D0, .0557D0, .0891D0,
     $      .1346D0, .1942D0, .2699D0, .3638D0, .6143D0, 1.0D0/
C
C     Sulfur I
C
      data LIMDAT(12) /ILEVS/
      data LLPRNT(12) /.true./
      data (LLABEL(I,12),I=1,ILEVS) /
     $      '3p4_3P          ', '3p4_1D           ', '3p4_1S          ',
     $      '4s_3S0          ', '4s''_3D0         ', '3d_3D0          ',
     $      '3p5_3P0         ', '5p_5P            '/
      data (PILEVL(I,12),I=1,ILEVS) /9.D0, 5.D0, 1.D0, 3.D0, 15.D0,
     $      15.D0, 5.D0, 15.D0/
      data (XLMTHR(I,12),I=1,ILEVS) /1205.03D0, 1345.53D0, 1629.22D0,
     $      3542.55D0, 6355.69D0, 7466.62D0, 8668.79D0, 11027.4D0/
      data (CCPLEV(I,12),I=1,ILEVS) /1.6D0, 20.99D0, 22.56D0, 1.D0,
     $      20.D0, 30.D0, 30.D0, 40.D0/
      data (SCPLEV(I,12),I=1,ILEVS) /4*0.D0, 3.D0, 0.D0, 2*3.D0/
      data (NPTABL(I,12),I=1,ILEVS) /80, 19, 24, 11, 0, 9, 0, 0/
      data (XLMTAB(I,1,12),I=1,80) /
     $ 1.08000D+03, 1.09000D+03, 1.09300D+03, 1.09410D+03, 1.09430D+03,
     $ 1.09440D+03, 1.09480D+03, 1.09510D+03, 1.09560D+03, 1.09610D+03,
     $ 1.09630D+03, 1.09660D+03, 1.09690D+03, 1.09710D+03, 1.09740D+03,
     $ 1.09780D+03, 1.09820D+03, 1.09880D+03, 1.09910D+03, 1.09920D+03,
     $ 1.09930D+03, 1.09960D+03, 1.09980D+03, 1.10020D+03, 1.10090D+03,
     $ 1.10130D+03, 1.10160D+03, 1.10170D+03, 1.10240D+03, 1.10260D+03,
     $ 1.10280D+03, 1.10350D+03, 1.10380D+03, 1.10450D+03, 1.11100D+03,
     $ 1.11500D+03, 1.11900D+03, 1.12100D+03, 1.12700D+03, 1.14600D+03,
     $ 1.16200D+03, 1.16500D+03, 1.16630D+03, 1.16710D+03, 1.16750D+03,
     $ 1.16820D+03, 1.16860D+03, 1.16900D+03, 1.16960D+03, 1.17050D+03,
     $ 1.17130D+03, 1.17150D+03, 1.17170D+03, 1.17250D+03, 1.17310D+03,
     $ 1.17350D+03, 1.17370D+03, 1.17390D+03, 1.17500D+03, 1.17520D+03,
     $ 1.17530D+03, 1.17550D+03, 1.17600D+03, 1.17650D+03, 1.17710D+03,
     $ 1.17730D+03, 1.17750D+03, 1.17840D+03, 1.17880D+03, 1.17950D+03,
     $ 1.18000D+03, 1.18400D+03, 1.18600D+03, 1.19000D+03, 1.19400D+03,
     $ 1.19700D+03, 1.19900D+03, 1.20100D+03, 1.20300D+03, 1.20503D+03/
      data (RCPTAB(I,1,12),I=1,80) /
     $ 6.34414D+00, 1.50025D+01, 2.35810D+01, 7.19701D+01, 1.11970D+02,
     $ 9.55112D+01, 2.95212D+01, 2.52319D+01, 2.88479D+01, 4.69327D+01,
     $ 7.06733D+01, 1.31471D+02, 5.97007D+01, 3.76758D+01, 2.80549D+01,
     $ 2.59501D+01, 2.77057D+01, 4.58554D+01, 7.44140D+01, 5.94015D+01,
     $ 4.36509D+01, 2.83292D+01, 2.70025D+01, 2.85686D+01, 4.70474D+01,
     $ 1.00648D+02, 6.19451D+01, 4.77207D+01, 2.70673D+01, 2.69327D+01,
     $ 2.75411D+01, 4.62244D+01, 3.36309D+01, 2.42843D+01, 1.17556D+01,
     $ 3.85736D+00, 7.00249D-01, 5.24190D-01, 2.11222D+00, 1.03142D+01,
     $ 2.31721D+01, 3.50274D+01, 6.99751D+01, 2.12319D+02, 9.55611D+01,
     $ 3.63691D+01, 3.41097D+01, 3.67631D+01, 5.10224D+01, 1.96209D+02,
     $ 5.59102D+01, 5.26683D+01, 5.51122D+01, 1.30623D+02, 4.74214D+01,
     $ 3.43042D+01, 3.31222D+01, 3.35910D+01, 7.03242D+01, 6.46384D+01,
     $ 6.25436D+01, 6.80798D+01, 1.25237D+02, 4.88229D+01, 3.07631D+01,
     $ 3.00898D+01, 3.05287D+01, 5.87032D+01, 3.89476D+01, 2.72170D+01,
     $ 2.65237D+01, 1.95000D+01, 1.66000D+01, 1.24000D+01, 9.20000D+00,
     $ 6.80000D+00, 5.20000D+00, 4.00000D+00, 2.50000D+00, 1.00000D+00/
      data (XLMTAB(I,2,12),I=1,19) /100.D0, 200.D0, 300.D0, 400.D0,
     $      500.D0, 600.D0, 700.D0, 800.D0, 850.D0, 900.D0, 950.D0,
     $      1000.D0, 1050.D0, 1080.D0, 1100.D0, 1110.D0, 1121.1D0,
     $      1121.3D0, 1345.53D0/
      data (RCPTAB(I,2,12),I=1,19) /.03D0, .1D0, .23D0, .39D0, .58D0,
     $      .77D0, .95D0, 1.1D0, 1.15D0, 1.19D0, 1.2D0, 1.18D0,
     $      1.13D0, 1.09D0, 1.05D0, 1.03D0, 1.D0, 0.D0, 0.D0/
      data (XLMTAB(I,3,12),I=1,24) /150.D0, 200.D0, 250.D0, 350.D0,
     $      400.D0, 450.D0, 500.D0, 550.D0, 600.D0, 650.D0, 700.D0,
     $      750.D0, 800.D0, 850.D0, 900.D0, 950.D0, 1000.D0, 1050.D0,
     $      1100.D0, 1125.D0, 1150.D0, 1164.D0, 1164.1D0, 1629.22D0/
      data (RCPTAB(I,3,12),I=1,24) /.01D0, .04D0, .11D0, .3D0, .41D0,
     $      .53D0, .65D0, .77D0, .89D0, 1.01D0, 1.11D0, 1.2D0, 1.27D0,
     $      1.33D0, 1.36D0, 1.36D0, 1.33D0, 1.27D0, 1.17D0, 1.11D0,
     $      1.04D0, 1.0D0, 0.D0, 0.D0/
      data (XLMTAB(I,4,12),I=1,11) /724.D0, 951.D0, 1203.D0, 1635.D0,
     $      2237.D0, 2550.D0, 2872.D0, 3065.D0, 3327.D0, 2540.D0,
     $      3542.55D0/
      data (RCPTAB(I,4,12),I=1,11) /6.5D0, 5.4D0, 6.9D0, 8.05D0,
     $      5.5D0, 3.3D0, 1.11D0, .29D0, .017D0, 1.D0, 1.D0/
      data (XLMTAB(I,6,12),I=1,9) /891.D0, 1108.D0, 1262.D0, 1465.D0,
     $      2829.D0, 4104.D0, 5624.D0, 7460.D0, 7466.62D0/
      data (RCPTAB(I,6,12),I=1,9) /.00587D0, .00893D0, .0117D0,
     $      .0158D0, .0757D0, .2D0, .46D0, 1.D0, 1.D0/
C
C     Oxygen-II
C
      data LIMDAT(13) /ILEVO2/
      data LLPRNT(13) /.true./
      data (LLABEL(I,13),I=1,ILEVO2) /
     $      '1               ', '2               ', '3               ',
     $      '4               ', '5               ', '6               ',
     $      '7               ', '8               '/
      data (PILEVL(I,13),I=1,ILEVO2) / 4.D0, 10.D0, 6.D0, 12.D0, 10.D0,
     $      12.D0, 6.D0, 10.D0/
      data (XLMTHR(I,13),I=1,ILEVO2) / 353.0189D0, 389.9238D0,
     $      411.8567D0, 611.8707D0, 852.6425D0, 1020.037D0,
     $      1061.544D0, 1310.629D0/
      data (CCPLEV(I,13),I=1,ILEVO2) / 11.13D0, 5.D0, 1.5D0, 6.48D0,
     $      7.65D0, 8.37D0, 8.54D0, 9.48D0/
      data (SCPLEV(I,13),I=1,ILEVO2) / ILEVO2*3.D0/
      data (NPTABL(I,13),I=1,ILEVO2) / ILEVO2*0/
C
C     Oxygen-III
C
      data LIMDAT(14) /ILEVO3/
      data LLPRNT(14) /.true./
      data (LLABEL(I,14),I=1,ILEVO3) /
     $      '1               ', '2               ', '3               ',
     $      '4               ', '5               ', '6               ',
     $      '7               ', '8               '/
      data (PILEVL(I,14),I=1,ILEVO3) / 9.D0, 5.D0, 1.D0, 15.D0, 9.D0,
     $      5.D0, 3.D0, 9.D0/
      data (XLMTHR(I,14),I=1,ILEVO3) / 225.6904D0, 236.5108D0,
     $      250.062D0, 309.5671D0, 332.5608D0, 390.5706D0,
     $      406.5078D0, 881.0363D0/
      data (CCPLEV(I,14),I=1,ILEVO3) / 4.86D0, 4.86D0, 6*1.D0/
      data (SCPLEV(I,14),I=1,ILEVO3) / ILEVO3*3.D0/
      data (NPTABL(I,14),I=1,ILEVO3) / ILEVO3*0/
C     !EJECT
      real*8 dummy
C
      call HI ('BOX')
C     !BEG
C---- Initialize universal constants (in common block shaman.inc)
      call KOSMOS
C---- Initialize derived constants (stored in RIGEL itself)
      call RIGEL  (0, dummy)
C---- Set up logs of extrema
      ZLNLARG = log(ZZLARGE)
      ZLNSMAL = log(ZZSMALL)
      ZL10LAR = log10(ZZLARGE)
      ZL10SMA = log10(ZZSMALL)
C     !END
      call BYE ('BOX')
C
      return
      end
