      subroutine RAPTURE
     $(LU,X,WVL,COP,TAU,TAUM,GTN,GTO,PHI,XND,XKL,XKT,EDINT,IMGT,EDTAU,
     $ EDTAUM,EDGTO,IMGG,LDL,DDLM,CDLM,LSTMP,EQUL,SMTH)
C
C     Rudolf Loeser, 1980 Apr 09
C---- Prints results from TAU calculation.
C     (This is version 2 of RAPTURE.)
C     !DASH
      save
C     !DASH
      real*8 CDLM, COP, DDLM, GTN, GTO, PHI, REFLM, TAU, TAUM, WVL, X,
     $       XKL, XKT, XND, dummy
      integer ICE, IL, IMGG, IMGT, IQPGA, IU, JJTKI, JJVXS, JJZ, KTKIN,
     $        LDL, LG, LP, LSTMP, LU, LUEO, MO, N, NERM, NL, NO
      logical EDGTO, EDINT, EDTAU, EDTAUM, EQUL, FORCED, FULL, SMTH
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ( 75),JJTKI)
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
      equivalence (RZQ( 91),REFLM)
      equivalence (KZQ( 95),NERM )
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
      equivalence (LEST(11),KTKIN)
C     !EJECT
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
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
      equivalence (IQQ(229),IQPGA)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 6),LUEO )
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
C     !EJECT
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
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS( 4),ICE  )
C     !DASH
C     !EJECT
      external ABJECT, CONDOR, LINER, IMGPRNT, MESHED, MASHED, HI, BYE
C
      dimension X(*)
C
C               XND(N,NL), TAUM(N), XKL(N), XKT(N), IMGG(N), IMGT(N),
      dimension XND(*),    TAUM(*), XKL(*), XKT(*), IMGG(*), IMGT(*),
C
C               TAU(N), GTO(N), GTN(N), PHI(N), COP(N)
     $          TAU(*), GTO(*), GTN(*), PHI(*), COP(*)
C
      call HI ('RAPTURE')
C     !BEG
      if(IQPGA.gt.0) then
C----   PEGTNALL on : error messages in every iteration
        LG = NO
      else
C----   PEGTNALL off: error messages in last iteration only
        LG = MO
      end if
C
      if(EDGTO.or.EDINT) then
C----   Editing happened: print regardless (based on PEGTNALL),
C       no matter what (the external switch) LU says
        LP = LUEO
      else
C----   Normal case: print according to (the external switch) LU
        LP = LU
      end if
C
C---- Initalize full printout switch
      FULL = .true.
C
C---- Determine whether this is a forced error message, and determine
C     whether a FULL version is allowed; if not, disallow printing
      FORCED = LP.ne.LU
      if(FORCED) then
        FULL = (KERMED(11).le.NERM).and.(NERM.gt.0)
      end if
      if(.not.FULL) then
        LP = 0
      end if
C     !EJECT
      if(LP.gt.0) then
        if(FORCED) then
          call MESHED    ('RAPTURE', 3)
          write (LP,100) KERMED(11),NERM
  100     format(' ','The following printout constitutes an error ',
     $               'notification',54X,'[',I7,'/',I7,']')
          KERMED(11) = KERMED(11)+1
        else
          call LINER     (2, LP)
        end if
C
        if(KTKIN.le.0) then
          call CONDOR    (LP, WVL, N, NL, IU, IL, ICE, X(JJZ)  ,
     $                    COP, XND, PHI, GTN, TAU, TAUM, X(JJVXS),
     $                    EDINT, EDTAU, EDTAUM, EDGTO, KTKIN, dummy,
     $                    GTO, XKL, XKT, LDL, DDLM, CDLM, LSTMP,
     $                    EQUL, SMTH)
        else
          call CONDOR    (LP, WVL, N, NL, IU, IL, ICE, X(JJTKI),
     $                    COP, XND, PHI, GTN, TAU, TAUM, X(JJVXS),
     $                    EDINT, EDTAU, EDTAUM, EDGTO, KTKIN, REFLM,
     $                    GTO, XKL, XKT, LDL, DDLM, CDLM, LSTMP,
     $                    EQUL, SMTH)
        end if
C
        if(FORCED) then
          call MASHED    ('RAPTURE')
          call ABJECT    (LP)
        end if
      end if
C
      if(.not.FULL) then
        if(EDGTO.or.EDINT) then
          call MESHED    ('RAPTURE', 3)
          if(EDGTO) then
            write (LUEO,101) IU,IL,'GTN'
  101       format(' ','##########  Transition (',I2,'/',I2,'): ',A,
     $                 '-editing occurred.')
            call IMGPRNT (LUEO, IMGG, N, 1)
          end if
          if(EDINT) then
            write (LUEO,101) IU,IL,'TAU-integrand'
            call IMGPRNT (LUEO, IMGT, N, 1)
          end if
          call MASHED    ('RAPTURE')
        end if
      end if
C     !END
      call BYE ('RAPTURE')
C
      return
      end
