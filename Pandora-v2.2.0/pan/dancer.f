      subroutine DANCER
     $(N,K,SS,BC,COP,SLF,SNU,SLR,SCNU,XKCNU,STRN,BCTRN,COPTRN,SSTRN,
     $ IMG,FO)
C
C     Rudolf Loeser, 1992 Aug 17
C---- Sets up the arrays: STRN, BCTRN, COPTRN [ & SSTRN ]
C     that are needed for emergent profile calculations.
C     (This is version 3 of DANCER.)
C     !DASH
      save
C     !DASH
      real*8 BC, BCTRN, COP, COPTRN, FO, SCNU, SLF, SLR, SNU, SS, SSTRN,
     $       STRN, XKCNU, ZERO
      integer ICE, IFDB, IL, IMG, IQESL, IQLTE, IU, J, K, N, NERM, NK
      logical lummy
      character LABEL*80
C     !COM
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
      equivalence (LINKDS(15),IFDB )
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (KZQ( 95),NERM )
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
      equivalence (IQQ( 33),IQLTE)
      equivalence (IQQ(145),IQESL)
C     !DASH
      external  MOVE1, FILLER, EDITH, HI, BYE
C
C               SS(N), BC(N), STRN(N,K), SLF(N,K), SNU(N,K), SCNU(N,K),
      dimension SS(*), BC(*), STRN(N,*), SLF(*),   SNU(*),   SCNU(*),
C
C               XKCNU(N,K), BCTRN(N,K), COPTRN(N,K), COP(N), SLR(N,K),
     $          XKCNU(*),   BCTRN(*),   COPTRN(*),   COP(*), SLR(*),
C
C               SSTRN(N,K), FO(N), IMG(N)
     $          SSTRN(*),   FO(*), IMG(*)
C     !EJECT
C
      call HI ('DANCER')
C     !BEG
      NK = N*K
      if(ICE.gt.0) then
C----   Use all three whole-profile frequency-dependent arrays as they
C       were produced by the PRD calculation
        if(ICE.eq.2) then
          call MOVE1  (SLR,   NK, STRN  )
        else
          call MOVE1  (SNU,   NK, STRN  )
        end if
        call MOVE1    (SCNU,  NK, BCTRN )
        call MOVE1    (XKCNU, NK, COPTRN)
      else
C----   Use the frequency-dependent Source Function array that is
C       always produced, and ...
        call MOVE1    (SLF,   NK, STRN  )
        if(IFDB.gt.0) then
C----     ... use the existing frequency-dependent BC and COP arrays
          call MOVE1  (SCNU,  NK, BCTRN )
          call MOVE1  (XKCNU, NK, COPTRN)
        else
C----     ... set up 'formally'-frequency-dependent BC and COP arrays
C         that use, at every frequency, the line-center values
          call MOVE1  (BC,    N,  BCTRN )
          call MOVE1  (COP,   N,  COPTRN)
          call FILLER (BCTRN,  N, N, K)
          call FILLER (COPTRN, N, N, K)
        end if
      end if
C
      if(IQLTE.gt.0) then
C----   Set up, in addition, a 'formally'-frequency-dependent
C       LTE Source Function array
        call MOVE1    (SS, N, SSTRN)
        call FILLER   (SSTRN, N, N, K)
      end if
C
      if(IQESL.gt.0) then
        do 101 J = 1,K
          write (LABEL,100) IU,IL,J,ICE
  100     format('Frequency-dependent S for transition (',I2,'/',I2,
     $           '),',I4,'. frequency',5X,'PRD=',I1,5X,
     $           '(Option SEDITIF)')
          call EDITH  (STRN(1,J), N, ZERO, 2, 1, 1, LABEL, IMG, FO,
     $                 KERMED(4), NERM, lummy)
  101   continue
      end if
C     !END
      call BYE ('DANCER')
C
      return
      end
