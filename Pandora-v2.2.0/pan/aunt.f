      subroutine AUNT
     $(N,F,LABEL,EDINT,IMG,W)
C
C     Rudolf Loeser, 2003 Oct 21
C---- Checks integrand, and edits out negatives if needed,
C     for TUNA.
C     !DASH
      save
C     !DASH
      real*8 F, W, ZERO
      integer IFO, IMG, IN, IQPTN, IS, LLT, MOX, N, NERM
      logical EDINT, lummy
      character LAB*100, LABEL*(*)
C     !COM
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (IQQ( 79),IQPTN)
C     !DASH
      external PLUSD, EDITH, FOMAKE, WGIVE, HI, BYE
C
      dimension W(*)
C
C               F(N), IMG(N)
      dimension F(*), IMG(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IFO   )
C
      call HI ('AUNT')
C     !BEG
      EDINT = .false.
C
C---- Check for negatives and zeroes in integrand
      call PLUSD    (F, 1, N, LLT)
      if((LLT.lt.N).and.(IQPTN.gt.0)) then
C----   Edit negatives out of integrand
        EDINT = .true.
        LAB   = LABEL
        LAB(91:100) = ' Integrand'
C
        call FOMAKE (IN, IS, MOX, 'TUNA')
C
        call EDITH  (F, N, ZERO, 2, 1, 1, LAB, IMG, W(IFO), KERMED(1),
     $               NERM, lummy)
C
        call WGIVE  (W, 'TUNA')
      end if
C     !END
      call BYE ('AUNT')
C
      return
      end
