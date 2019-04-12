      subroutine FENNEL
     $(MR,INPAIR,NLPAIR,LZA,LR,BANDL,BANDU,IBNDE,KODOUT)
C
C     Rudolf Loeser, 1981 Apr 28
C---- Controls the reading of the 1. batch of input statements:
C     counters and options.
C     (This is version 2 of FENNEL.)
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, dummy
      integer IBNDE, INPAIR, IPEX, IPNT, ISCRS, KALOR, KARB, KBTMX,
     $        KERR, KIND, KMMAX, KODOUT, KOLEV, KRTMX, KSTMX, LDLMX, LI,
     $        LL, LOOK, LR, LUCA, LUEO, LUIN, LZA, MODE, MR, NARB, NL,
     $        NLPAIR, NOION, NSL, NTAN, NTOT, jummy
      logical STOPOPT
      character GO*8, LIST*8, LISTP*8, LNLIST*8, QALHD*8, QALL*8,
     $          QNAME*8, qummy*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
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
      equivalence (QZQ(  5),QALHD)
      equivalence (KZQ( 45),NTAN )
      equivalence (KZQ( 65),KALOR)
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ( 96),KARB )
      equivalence (KZQ(140),NARB )
      equivalence (KZQ( 88),ISCRS)
      equivalence (KZQ(132),KMMAX)
      equivalence (KZQ( 33),KOLEV)
      equivalence (KZQ( 18),IPEX )
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
      equivalence (LEST(33),LDLMX)
      equivalence (LEST(60),KBTMX)
      equivalence (LEST(61),KRTMX)
      equivalence (LEST(62),KSTMX)
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 9),LUCA )
      equivalence (LUNITS( 1),LUIN )
C
C---- DATAFIL     as of 1984 Apr 19
      integer     KIWILFN
      common      /DATAFIL/ KIWILFN
C     Number of unit from which to read input statements.
C     .
C---- CARGOT      as of 1996 Dec 10
      logical     ESCARGO
      common      /CARGOT/ ESCARGO
C     Input reading signal from subroutine CARMEN.
C     .
C     !EJECT
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
C---- CARDFIL     as of 1984 Apr 19
      integer     KIWIOUT
      common      /CARDFIL/ KIWIOUT
C     Number of unit to which to copy input statements.
C     .
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
C     !DASH
C     !EJECT
      external  CARMEN, LOOKUC, MUSTARD, TARZAN, SHRUG, ABORT, UNMIX,
     $          CHLOE, KIWI, HI, BYE
      intrinsic max
C
C               MR(NSL+1), LR(NL), BANDL(NAB), BANDU(NAB), IBNDE(NAB),
      dimension MR(*),     LR(*),  BANDL(*),   BANDU(*),   IBNDE(*),
C
C               LZA(*), NLPAIR(2*NL), INPAIR(2*NT),
     $          LZA(*), NLPAIR(*),    INPAIR(*)
C
      parameter (LL=58, LI=30)
      parameter (NTOT=LL+LI+NPI)
C
      dimension IPNT(NTOT), QALL(NTOT)
      dimension LNLIST(LL), LIST(LI), LISTP(NPI)
C
      data LNLIST /
     $ 'N'  , 'NL' , 'M'  , '0'  , 'NT' , 'NSW', 'L'  , '0'  , 'NFB',
     $ 'KK' , 'KBX', '0'  , '0'  , '0'  , 'MRR', '0'  , 'NWV', '0'  ,
     $ 'LF' , 'NTE', 'NDT', 'MHM', 'NWS', 'JM' , '0'  , 'LDU', 'LLY',
     $ '0'  , '0'  , '0'  , 'NFH', 'NCR', '0'  , 'LG' , 'INK', 'KS' ,
     $ 'KR' , 'KB' , 'MQT', 'NSL', 'NDR', 'NVX', 'NDV', '0'  , 'NAB',
     $ '0'  , 'NVF', 'NXF', '0'  , 'NKA', 'NCL', '0',   'NCQ', 'NVH',
     $ 'NCB', 'NZE', '0'  , 'NGM'/
C
      data LIST /
     $ 'INPAIR',  'MR',      'LZA',     'LR',      'NLPAIR',  'OMIT',
     $ 'DO',      'USE',     'NMT',     'NTAN',    'KALHD',   'KALOR',
     $ 'NOION',   'BANDL',   'BANDU',   'BANDE',   'KOLEV',   'WORLDLY',
     $ 'KARB',    'OUTPUT',  'ISCRS',   'FILE',    'RUNTOPOP','LDLMAX',
     $ 'KBTMAX',  'KRTMAX',  'KSTMAX',  'KMMAX',   'NARB',    'IPEX'/
C
      data LISTP /
     $ 'NLH',  'NLC',  'NLS',  'NLZ',  'NZ2',  'NAL',  'NMG',  'NFE',
     $ 'NNA',  'NCA',  'NLO',  'NLU',  'NO2',  'NO3'/
C
      data GO /'GO'/
C
      call HI ('FENNEL')
C     !BEG
      KODOUT  = 0
      KERR    = 0
      LUOUT   = 0
      LUINP   = 0
      KART    = 0
      KIWIOUT = LUCA
      KIWILFN = LUIN
      STOPOPT = .false.
C     !EJECT
C---- Read next input field
  100 continue
        call KIWI      (MODE, dummy, jummy, QNAME, jummy)
C
        if(MODE.ne.2) goto 201
        call UNMIX     (QNAME)
C----   Check for array counter
        call LOOKUC    (LNLIST, LL, QNAME, KIND, LOOK)
C       ("K" is a synonym for "KS")
        if(QNAME.eq.'K       ') then
          KIND = 36
          LOOK = 1
        end if
C
        if(LOOK.eq.1) then
          call MUSTARD (QNAME, dummy, JZQ, qummy, KIND, 3)
          if(KIND.eq.2) then
C           (Set up NSL default promptly)
            NSL = max(NSL,NL)
          end if
          if(ESCARGO) goto 199
          goto 100
        end if
C
C----   Check for population counter
        call LOOKUC    (LISTP, NPI, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call MUSTARD (QNAME, dummy, LENPOP, qummy, KIND, 3)
          if(ESCARGO) goto 199
          goto 100
        end if
C
C----   Check for miscellaneous stuff
        call LOOKUC    (LIST, LI, QNAME, KIND, LOOK)
        if(LOOK.eq.1) then
          call TARZAN  (KIND, QNAME, INPAIR, MR, LR, LZA, NMT, NTAN,
     $                  KALOR, BANDL, BANDU, IBNDE, NOION, KARB,
     $                  KODOUT, ISCRS, MRTP, MRTPM, LDLMX, KBTMX,
     $                  KRTMX, KSTMX, NLPAIR, KMMAX, STOPOPT, NARB,
     $                  QALHD, KOLEV, IPEX)
          if(ESCARGO) goto 199
          goto 100
        end if
C
C----   Check for end-of-input
        if(QNAME.eq.GO) then
C----     Yes - go home
          goto 199
        else
C----     No - error
          goto 202
        end if
      continue
C     !EJECT
C---- Error messages
  202 KERR = KERR+1
  201 KERR = KERR+1
      call SHRUG  (LNLIST, LL, LIST, LI, LISTP, NPI, QALL, IPNT, LUEO)
      call CHLOE  (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
      if(ESCARGO) goto 199
      goto 100
C
C---- Go home (almost)
  199 continue
      if(STOPOPT) then
        call ABORT
      end if
C     !END
      call BYE ('FENNEL')
C
      return
      end
