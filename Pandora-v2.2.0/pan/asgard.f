      subroutine ASGARD
     $(N,KK,XJBAR,S,IMG,FO)
C
C     Rudolf Loeser, 2004 May 07
C---- Edits Jbar for BRASS.
C     !DASH
      save
C     !DASH
      real*8 FO, S, XJBAR, ZERO
      integer IL, IMG, IQSRJ, IU, KK, KMSS, M, MO, N, NERM
      logical ALLBAD, BAD, lummy
      character LABEL*60
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
      equivalence (KZQ( 95),NERM )
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
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !EJECT
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C
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
      equivalence (IQQ(246),IQSRJ)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external EDIT1, UTTA, MOVE1, WOBLE, ZEROI, HI, BYE
C
C               XJBAR(N), IMG(N), FO(N), S(N)
      dimension XJBAR(*), IMG(*), FO(*), S(*)
C
      call HI ('ASGARD')
C     !BEG
      KMSS = 1
      if((IQSRJ.le.0).or.(MO.le.0)) then
        KMSS = 0
      end if
C
      write (LABEL,100) IU,IL,KK
  100 format('Jbar for Transition',I3,'/',I2,', from depth #',I8)
C
      call ZEROI   (IMG, 1, N)
      M = (N-KK)+1
C     call EDITH   (XJBAR(KK), M, ZERO, 2, 1, KMSS, LABEL, IMG(KK), FO,
C    $              KERMED(6), NERM, BAD)
C
      call MOVE1   (XJBAR(KK), M, FO)
      call EDIT1   (XJBAR(KK), M, ZERO, 2, 1, IMG(KK), BAD, ALLBAD)
      if(ALLBAD) then
        call MOVE1 (S(KK), M, XJBAR(KK))
      else
        call UTTA  (XJBAR(KK), M, ZERO, 2, 1, KMSS, LABEL, IMG(KK), FO,
     $              KERMED(6), NERM, .false.)
      end if
C
      if(BAD.and.(KMSS.le.0)) then
        call WOBLE (IMG, N, LABEL, 'ELBOW', lummy, 1)
      end if
C     !END
      call BYE ('ASGARD')
C
      return
      end
