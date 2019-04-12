      subroutine VANILLA
     $(X,IX,W,LZA,ZAUX,XKPCR,INPAIR,KSW,WMNO,WMXO,RKWO,Y,CRD,CVW,CSK,
     $ CRS,COP,RHW,RHWTO,CDL,DDL,DWN,WSM,DRO,XC,XP,XR,GMA,PGL,XIB,
     $ XIR,XIS,DPM,IFS,ILS,NED,ISB1,ISB2,IST,KST)
C     Rudolf Loeser, 1968 Apr 18
C---- Controls reading the second batch of input statements -
C     'General Input'.
C     !DASH
      save
C     !DASH
      real*8 CDL, COP, CRD, CRS, CSK, CVW, DDL, DPM, DRO, DWN, GMA, PGL,
     $       RHW, RHWTO, RKWO, W, WMNO, WMXO, WSM, X, XC, XIB, XIR, XIS,
     $       XKPCR, XP, XR, Y, ZAUX, dummy
      integer IFS, ILS, INPAIR, IPNT, ISB1, ISB2, IST, IX, KERR, KIND,
     $        KST, KSW, LOOK, LUEO, LZA, MODE, NBAM, NDICM, NED, NFAR1,
     $        NFAR2, NFVEC, NIR2, NIV1, NIV2, NIV3, NIVEC, NMISC, NPAR,
     $        NTOT, jummy
      character GO*8, LBAM*8, LDICM*8, LFAR1*8, LFAR2*8, LFVEC*8,
     $          LIR2*8, LIV1*8, LIV2*8, LIV3*8, LIVEC*8, LMISC*8,
     $          QALL*8, QNAME*8, qummy*8
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
C
C---- BAMBI       as of 1998 Apr 22
      integer     IPDPAR
      real*8      APDPAR
      dimension   APDPAR(10)
      common      /BAMBI1/ IPDPAR
      common      /BAMBI2/ APDPAR
C     Parameters for "original" d coefficients
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- ZINDEX      as of 1984 Apr 24
      integer     MAUXZI
      common      /ZINDEX/ MAUXZI
C     Auxiliary Z-scale index, for input processing.
C     .
C
C---- CARGOT      as of 1996 Dec 10
      logical     ESCARGO
      common      /CARGOT/ ESCARGO
C     Input reading signal from subroutine CARMEN.
C     .
C     !DASH
C     !EJECT
      external KIWI, LOOKUC, MUSTARD, SAFFRON, CICELY, PAPRIKA, GINGER,
     $         UNMIX, ETONOS, CHLOE, ABORT, CARMEN, MATHENA, MOCHA,
     $         PUDU, HI, BYE
C
      dimension X(*), IX(*), W(*)
C
C               ZAUX(LZM,NZM), KSW(Nopac), INPAIR(2,NT), CRD(LDLMX,NT),
      dimension ZAUX(*),       KSW(*),     INPAIR(*),    CRD(*),
C
C               CVW(LDLMX,NT), CSK(LDLMX,NT), CDL(LDLMX,NT), RHW(N,NT),
     $          CVW(*),        CSK(*),        CDL(*),        RHW(*),
C
C               DDL(LDLMX,NT), DWN(LDLMX,NT), CRS(NT), WSM(NT), XC(NT),
     $          DDL(*),        DWN(*),        CRS(*),  WSM(*),  XC(*),
C
C               XIB(KBTMX,NT), XIR(KRTMX,NT), ISB2(NT), IFS(NT), Y(NT),
     $          XIB(*),        XIR(*),        ISB2(*),  IFS(*),  Y(*),
C
C               DRO(NT), GMA(NT), ISB1(NT), XIS(KSTMX,NT), RHWTO(N,NT),
     $          DRO(*),  GMA(*),  ISB1(*),  XIS(*),        RHWTO(*),
C
C               NED(NT), IST(NT), KST(NT), PGL(NT), COP(N,NT), DPM(NT),
     $          NED(*),  IST(*),  KST(*),  PGL(*),  COP(*),    DPM(*),
C
C               XKPCR(NT), LZA(50), XP(NT), ILS(NT), RKWO(N), XR(NT)
     $          XKPCR(*),  LZA(*),  XP(*),  ILS(*),  RKWO(*), XR(*)
C
C
C     NOTE: there is room in LIV1, LIV2, LFVEC, LFAR1, LFAR2, LMISC
C
      parameter (NIV1=177,    NIV2=226,   NIV3=4,     NFVEC=103 )
      parameter (NFAR1=9,     NFAR2=44,   NMISC=36,   NBAM=7    )
      parameter (NDICM=10,    NIR2=14,    NIVEC=4     )
C
      parameter (NPAR=NIV1+NIV2+NIV3+NFVEC+NIR2+NFAR1+NFAR2+NMISC)
      parameter (NTOT=NPAR+NBAM+NDICM+NIVEC)
C
      dimension IPNT(NTOT), QALL(NTOT)
      dimension LIV1(NIV1), LIV2(NIV2), LIV3(NIV3), LFVEC(NFVEC),
     $          LFAR1(NFAR1), LIR2(NIR2), LFAR2(NFAR2), LMISC(NMISC),
     $          LDICM(NDICM), LBAM(NBAM), LIVEC(NIVEC)
C
C     !EJECT
      data (LIV1(I),I=1,127) /
     $ 'ADS', 'WZ', '0   ', 'MASS', 'PART', 'ABD', 'PW', 'Y',
C       1      2     3       4       5       6      7     8
C
     $ 'NUK', 'CWR', 'CHOP', 'EXLYM', 'TGLYM', 'HSEC', 'YH', 'CGR',
C       9      10     11      12       13       14      15    16
C
     $ 'DLU', 'TX', 'YL', 'YPRE', 'TSM', 'DRLIM', 'R1N', 'TBAR',
C       17     18    19    20      21     22       23     24
C
     $ 'WPOP', 'LMT', 'LME', 'KDUST', 'TDUST', 'YFLUX', 'HEL', 'LMZ',
C       25      26     27     28       29       30       31     32
C
     $ 'LMF', 'WSM', 'LMA', 'LMB', 'LMR', 'TLIMG', 'BLIMG', 'WEP',
C       33     34     35     36     37     38       39       40
C
     $ 'OPF', 'RFAC', 'WRMN', 'TMS', 'WBD', 'RCCFE', 'PARTLIM', 'KURMI',
C       41     42      43      44     45     46       47         48
C
     $ 'KURMA', 'WTD', 'TLTR', 'XCOMX', 'DDT', 'INCH', 'WRMX', 'SMP',
C       49       50     51      52       53     54      55      56
C
     $ 'EIDIF', 'CUTFE', 'WZM', 'RFMAS', 'FABD', 'EPCBR', 'TSMALL',
C       57       58       59     60       61      62       63
C
     $ 'TLARGE', '0   ', 'PRDCV', '0   ', '0   ', 'CSFCRIT',
C       64        65      66       67      68      69
C
     $ 'CLNH', 'HTAU', 'PZERO', 'TML', 'DELTB', 'CEQMX', '0   ',
C       70      71      72       73     74       75       76
C
     $ 'SMATC', 'ELLED', 'EMXED', 'VMNFE', 'XQMAX', 'DQMIN',
C       77       78       79       80       81       82
C
     $ 'DQMAX', 'XJFE', 'VSMLL', 'CPRESS', 'WPRESS', 'FZLIM',
C       83       84      85       86        87        88
C
     $ 'CLOGG', 'DZMSS', 'REFLM', 'SOBFEQ', 'SOBDMX', 'SOBDMN',
C       89       90       91       92        93        94
C
     $ 'ADMAS', 'CHLIM', 'YCOL', 'LCOD', 'CCHX', 'CHEFLOW', 'CQM',
C       95       96       97      98      99      100        101
C
     $ 'XCL', 'TAUCL', 'CVXS', 'MCOA', 'VOITC', 'ZNDW', 'WNJUNK',
C       102    103      104     105     106      107     108
C
     $ 'WBDIR', '0   ', 'ASMCR', 'ZXMIN', 'CFH', 'CVSB', 'HEABL',
C       109      110     111      112      113    114     115
C
     $ 'RFHEAB', 'TRFLI', 'ESCTAU', 'WNUK', 'HSBM', 'HSBDMN',
C       116       117      118       119     120     121
C
     $ 'HSBDMX', 'HSBFEQ', 'PMSK', 'CEDMN', 'CEDMX', 'CEFEQ'/
C       122       123       124     125      126      127
C     !EJECT
      data (LIV1(I),I=128,NIV1) /
     $ 'FZION', 'FROSCE', 'FSTKM', 'FRCDL', 'FMCDL', 'CSDW', 'CLEVELS',
C       128      129       130      131      132      133     134
C
     $ 'WAVEMN', 'WAVEMX', 'CVZ', 'CDZ', 'COMU', 'BMWAC', 'SRCO',
C       135       136       137    138    139     140      141
C
     $ '0   ', '0   ', 'HNAJL', '0   ', '0   ', 'FMVLIM', 'RCOMIN',
C       142     143     144      145     146     147       148
C
     $ 'CWJ', 'PNH', 'CLM', 'CFHE', 'SN1CC', 'LMDL3', 'WSN1D', 'AOWXP',
C       149    150    151    152     153      154      155      156
C
     $ 'CTCO', 'CTMX', 'SHCOP', 'ZRCO', 'SHCOC', 'LMXC', 'LMXP',
C       157     158     159      160     161      162     163
C
     $ 'LMDL2', 'LMCR', 'WRATMN', 'WRATMX', 'YRATS', 'LMH', '0   ',
C       164      165     166       167       168      169    170
C
     $ '0   ', 'SCTA', 'SCTS', '0   ', '0   ', 'CN1S', 'DELLIM'/
C       171     172     173     174     175     176     177
C
C     !EJECT
      data (LIV2(I),I=1,125) /
     $ 'NDW', 'MS', 'NS', 'ISUB', 'IDFSW', 'IRLS1', 'JSTIN', 'IOMX',
C       1      2     3     4       5        6        7        8
C
     $ 'IRLSN', '0   ', 'IXSTA', '0   ', 'ILI', 'NIL', 'MFONT',
C       9        10      11       12      13     14     15
C
     $ 'LN', 'TOPE', 'IPEX', 'LYMITER', 'HSLITER', 'NGRL', 'NGRR',
C       16    17      18      19         20         21      22
C
     $ 'INFSM', 'INLSM', 'METEP', 'KONFORM', 'KURIN', 'KINMAX',
C       23       24       25       26         27       28
C
     $ 'KININT', 'MDTR1', 'MDTR2', 'KKPR', '0   ', 'JNUNC', 'JSTCN',
C       29        30       31       32      33      34       35
C
     $ 'IZOPT', 'JZOPT', 'IVOIT', 'NVOIT', 'JBDNC', 'IMUCD', 'M304',
C       36       37       38       39       40       41       42
C
     $ 'LSTMP', 'NCOSW', '0   ', 'JBFSW', 'IHEDF', 'LDINT', 'LDTYP',
C       43       44       45      46       47       48       49
C
     $ 'KUDNT', 'JH1', 'JH2', 'ISRCD', 'IDRCD', 'NHTSW', 'IONSTAGE',
C       50       51     52     53       54       55       56
C
     $ 'IPR01', 'IPR02', 'IPR03', 'IPR04', 'MSKIP', 'IPERFA', 'IRPUN',
C       57       58       59       60       61       62        63
C
     $ 'IDEX', '0    ', 'MCON', 'NIASM', '0    ', '0    ',
C       64      65       66      67       68       69
C
     $ '0    ', 'KODNT', 'JHEAS', 'MAMAS', 'ISNUD', 'IDWIN', 'ITRFI',
C       70       71       72       73       74       75       76
C
     $ 'NSPED', 'IFXDS', 'NVDFE', 'NNDFE', 'NZDFE', 'IPZER', 'IHEAB',
C       77       78       79       80       81       82       83
C
     $ 'IHDMP', 'IRTIS', 'KOOLSUM', 'MH2N', '0    ', 'LYODS', 'KDIAG',
C       84       85       86         87      88       89       90
C
     $ 'N1MET', 'ISMBD', 'IRUNT', '0    ', 'NERM', '0    ', 'ISOD',
C       91       92       93       94       95      96       97
C
     $ 'IPRDD', 'IPRDF', 'IPPOD', 'MN1', 'IDRDP', 'KDRDP', 'KAPDB',
C       98       99       100      101    102      103      104
C
     $ 'MTHEI', 'MDFV', 'NCOPT', 'LSFGC', 'NODCG', 'MNG1', 'IDFDM',
C       105      106     107      108      109      110     111
C
     $ 'IDFDI', 'ISNDD', 'IDEDP', 'IBRDP', 'ICXDP', 'NSPRD', 'ICHDP',
C       112      113      114      115,     116      117      118
C
     $ 'ISMSW', 'ICDIT', 'IRATE', 'ICHSW', 'IHSSW', 'IHSDP', 'IHSDD'/
C       119      120      121      122      123      124      125
C     !EJECT
      data (LIV2(I),I=126,NIV2) /
     $ 'IHSKM', 'IHSSM', 'NDWM', 'LHHSE', 'LX2DS', 'LX3DS',
C       126      127      128     129      130      131
C
     $ 'KMMAX', 'ISTARK', 'NBS', 'NANAL1', 'IHSSP', 'NANAL2',
C       132      133       134    135       136      137
C
     $ 'LOGAS', 'NECLIP', 'NARB', '0   ', '0   ', '0   ',
C       138      139       140     141     142     143
C
     $ 'IXNCS', 'IRFNC', 'JDMCI', 'JDMCE', 'IDNRT', 'JHBFD',
C       144      145      146      147      148      149
C
     $ 'MOPRNT', 'IWSMD', 'LWNT', 'KDIFD1', 'KDIFGS', 'KDIFGA',
C       150       151      152     153       154       155
C
     $ 'KDIFGB', 'ITN1R', 'LODCG', 'I4DFM', 'I4DEQ', 'I4DIO',
C       156       157      158      159      160      161
C
     $ 'MSSPR', 'NEFDF', 'IPDIJ', 'IPDEE', 'IBETSW', 'KB1WA',
C       162      163      164      165      166       167
C
     $ 'KB1WB', 'KDAMP', 'KBNDS', 'N1NUP', 'MDFG', 'INDRN', 'MKURU',
C       168      169      170      171      172     173      174
C
     $ 'KHFFS', 'MTREF', 'KANTNU', 'ISCOMP', 'KB1WS', 'KOELS',
C       175      176      177       178       179      180
C
     $ 'JZATOM', 'JZATMO', 'IGMSW', 'NLY', 'KCOAA', 'IWEIT',
C       181       182       183      184    185      186
C
     $ 'IFALL', 'NQLYM', 'JXNCS', 'JHLSK', 'NMLR', 'JSSV',
C       187      188      189      190      191     192
C
     $ 'JNEDP', 'JEDIT', 'MBREC', 'JATAW', 'IBNVIEW', 'NDSN1',
C       193      194      195      196      197        198
C
     $ 'IXASM', 'MXPPI', 'MXTAP', 'ITKZA', 'IDFDS', 'LDFD1', 'NGNV',
C       199      200      201      202      203      204      205
C
     $ 'IPIJG', 'ISMVE', 'JSFEX', 'LOXDS', 'IORIC', 'LHEDS', '0   ',
C       206      207      208      209      210      211      212
C
     $ 'IGII', 'ITPRD', 'LEEDS', 'KXLYM', '0   ', '0   ', 'KLDIN',
C       213     214      215      216      217     218     219
C
     $ 'KLFIN', 'METSEDG', 'METSEDW', '0   ', '0   ', 'INCEI',
C       220      221        222        223     224     225
C
     $ 'ICIH1'/
C       226
C     !EJECT
      data LDICM /
     $ 'NAPWRA', 'NAPWRB', 'NAPKNT', 'APARAD', 'APETA', 'APCDP',
C       1         2         3         4         5        6
C
     $ 'APWRA', 'APWRB', 'APCI', 'APEI'/
C       7        8        9       10
C
C
C
      data LIV3 /
     $ 'NAME', 'ELSYM', 'MODLAB', 'ATOLAB'/
C       1       2        3         4
C
C
C
      data LBAM /
     $ 'APDTEXP', 'APDXICA', 'APDXICB', 'APDXICC', 'APDDIFC',
C       1          2          3          4          5
C
     $ 'APDDTFC', 'APDXICD'/
C       6          7
C     !EJECT
      data LFVEC /
     $ 'TS', 'XI', 'NU', 'P', 'CP', 'TER', 'TE', 'TR', 'NE', 'NH',
C       1     2     3     4    5     6      7     8     9     10
C
     $ 'V', 'FRR', 'Z', 'ADT', 'XISYM', 'XIRED', 'HE304', 'XK', 'XIBLU',
C       11   12     13   14     15       16       17       18    19
C
     $ 'ZALBK', 'EP1', 'BXI', 'RZM', 'VT', 'RABD', 'WAVES', 'TDST',
C       20       21     22     23     24    25      26       27
C
     $ 'YK', 'DGM', 'YWAVE', 'AEL', 'YLDT', 'LHM', 'AHM', 'BDHM',
C       28    29     30       31     32      33     34     35
C
     $ 'YHM', 'LDT', 'LMM', 'MLC', 'DWAVE', 'LMDUST', 'DFDUST',
C       36     37     38     39     40       41        42
C
     $ 'ALBDUST', 'EPDUST', 'LMXX', 'LMDR', 'ALBK', 'VSB', 'ALBDT',
C       43         44        45      46      47      48     49
C
     $ 'XINK', 'FINK', 'YLYM', 'VBMB', 'PALBET', 'GK', 'LCR',
C       50      51      52      53      54        55    56
C
     $ 'ICR', 'YCR', 'NK', 'QTAIL', 'EP2', 'WNUC', 'ZGM', 'DGMZ',
C       57     58     59    60       61     62      63     64
C
     $ 'XCOL', 'AL', 'ZMASS', 'XDR', 'DDR', 'FKUR', 'TEX', 'TAUKIN',
C       65      66    67       68     69     70      71     72
C
     $ 'QIN', '0   ', 'VXS', 'XMU', 'BANDY', 'DELWAVE', 'VR',
C       73     74      75     76     77       78         79
C
     $ 'PBETAL', 'PBETGM', 'VM', 'VNH', 'HNDV', 'RHEAB', 'WNU',
C       80        81        82    83     84      85       86
C
     $ 'LCOA', 'LCOB', 'PGMBET', 'ZME', 'NCOI', 'CQT', 'CQA', 'NC',
C       87      88      89        90     91      92     93     94
C
     $ 'RABDL', 'MCI', 'ACI', 'SCOW', 'RKMULT', 'FNRMLA', 'FNRMLB',
C       95       96     97     98      99        100       101
C
     $ 'NUC', 'BHORIZ'/
C       102    103
C
C
C
      data LIVEC /
     $ 'IRLCOMP', 'IRKCOMP', 'QNL', 'LCH'/
C       1          2          3      4
C
C
C
      data LFAR1 /
     $ 'RK', 'RL', 'TRN', '0   ', 'CKADD', 'CI', 'ND', 'BD',
C       1     2     3      4       5        6     7     8
C
     $ 'RKWT'/
C       9
C     !EJECT
C
      data LIR2 /
     $ 'A', 'CE', 'RHO', 'YCONT', 'OML', 'JBAR', 'OLL', 'CIJADD',
C       1    2     3      4        5      6       7      8
C
     $ 'MCE', 'ACE', 'CHI', 'AW', 'FCE', 'PCE'/
C       9      10     11     12    13     14
C
C
C
      data LFAR2 /
     $ 'KPCR', 'ECLI', 'METSE', 'PROF', 'SCH', 'CRD', 'CVW',
C       1       2       3        4       5      6      7
C
     $ 'CSK', 'CRS', 'YLINE', '0   ', 'KPC', 'RHWT', 'BLCSW',
C       8      9      10       11      12     13      14
C
     $ 'LSFTYP', 'SMOOTH', 'DRHO', 'NED', 'XP', 'XC', 'GMMA',
C       15        16        17      18     19    20    21
C
     $ '0   ', 'INRHO', 'LFLUX', 'LDL', 'DDL', 'CDL', 'LSFPRINT',
C       22      23       24       25     26     27     28
C
     $ 'LSFFDB', 'SOBOLEV', 'PROGLI', 'SGRAF', 'DWN', 'KBT', 'KRT',
C       29        30         31        32       33     34     35
C
     $ 'KST', 'XIBLUT', 'XIREDT', 'XISYMT', 'STARKI', 'CSTARK',
C       36     37        38        39        40        41
C
     $ 'LSFBOC', 'DPMULT', 'XR'/
C       42        43        44
C
C
C
      data LMISC /
     $ 'FILE', 'DOSFPRNT', 'WEIGHT', 'KTRANS', 'USE', 'BDOPT',
C       1       2           3         4         5      6
C
     $ 'RHOPT', 'CIMETHOD', 'ELEMENT', 'RQCP', 'LEVDES', 'WRAT',
C       7        8           9          10      11        12
C
     $ 'RRCP', 'YRATE', 'NABS', 'POPUP', 'NEWELE', 'ZAUX', 'MAUX',
C       13      14       15      16       17        18      19
C
     $ 'RKC', 'TKR', 'YKR', 'MATRIX', 'DOPROF', 'DOFLUX', 'DOFDB',
C       20     21     22     23        24        25        26
C
     $ 'COLINES', 'KRATE', 'RCHX', 'XRKH', 'XRLH', 'RKW', 'RHOWT',
C       27         28       29      30      31      32     33
C
     $ 'WMN', 'WMX', 'CEMETHOD'/
C       34     35     36
C
C
C
      data GO /'GO'/
C     !EJECT
C
      call HI ('VANILLA')
C     !BEG
      KERR   = 0
      MAUXZI = 0
C---- Read next input field
  100 continue
        call KIWI      (MODE, dummy, jummy, QNAME, jummy)
        if(MODE.ne.2) goto 201
        call UNMIX     (QNAME)
C----   Check for simple parameter
        call LOOKUC    (LIV1, NIV1, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call MUSTARD (QNAME, RZQ, jummy, qummy, KIND, 5)
          if(ESCARGO) goto 199
          goto 100
        end if
        call LOOKUC    (LIV2, NIV2, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call MUSTARD (QNAME, dummy, KZQ, qummy, KIND, 3)
          if(ESCARGO) goto 199
          goto 100
        end if
        call LOOKUC    (LIV3, NIV3, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call MUSTARD (QNAME, dummy, jummy, QZQ, KIND, 2)
          if(ESCARGO) goto 199
          goto 100
        end if
        call LOOKUC    (LBAM, NBAM, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call MUSTARD (QNAME, APDPAR, jummy, qummy, KIND, 5)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for floating vector
        call LOOKUC    (LFVEC, NFVEC, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call MOCHA   (QNAME, KIND, X, LZA, ZAUX, W)
          if(ESCARGO) goto 199
          goto 100
        end if
C     !EJECT
C----   Check for integer vector
        call LOOKUC    (LIVEC, NIVEC, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call MATHENA (QNAME, KIND, IX)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for floating array with one index
        call LOOKUC    (LFAR1, NFAR1, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call CICELY  (QNAME, KIND, X, LZA, ZAUX, W)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for array with two indices
        call LOOKUC    (LIR2, NIR2, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call PAPRIKA (QNAME, KIND, X, LZA, ZAUX, W)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for Recombination data
        call LOOKUC    (LDICM, NDICM, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call PUDU    (QNAME, KIND)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for Line Intensity Data Blocks data
        call LOOKUC    (LFAR2, NFAR2, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call GINGER  (QNAME, KIND, X, LZA, ZAUX, W, XKPCR, Y, CRD,
     $                  CVW, CSK, CRS, COP, RHW, CDL, DDL, DWN, WSM,
     $                  DRO, XC, XP, XR, GMA, PGL, XIB, XIR, XIS,
     $                  DPM, IFS, ILS, NED, ISB1, ISB2, IST, KST)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for miscellaneous arrays
        call LOOKUC    (LMISC, NMISC, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call SAFFRON (X, IX, QNAME, KIND, LZA, ZAUX, INPAIR, KSW,
     $                  WMNO, WMXO, RHWTO, RKWO, W)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for end-of-input
        if(QNAME.ne.GO) goto 202
C---- That's all.
      goto 199
C     !EJECT
C---- Error messages
  202 KERR = KERR+1
  201 KERR = KERR+1
      call ETONOS (LIV1, NIV1, LIV2, NIV2, LIV3, NIV3, LFVEC, NFVEC,
     $             LFAR1, NFAR1, LFAR2, NFAR2, LMISC, NMISC, LIR2,
     $             NIR2, LDICM, NDICM, LBAM, NBAM, LIVEC, NIVEC, QALL,
     $             IPNT, LUEO)
      call CHLOE  (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
      if(ESCARGO) goto 199
      goto 100
C---- Go home
  199 continue
C     !END
      call BYE ('VANILLA')
C
      return
      end
