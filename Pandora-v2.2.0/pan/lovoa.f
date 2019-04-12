      subroutine LOVOA
     $(LU,XC,XP,XR,GAMMA,WAVE,VXS,MSFT,K,MPROM)
C
C     Rudolf Loeser, 1989 May 05
C---- Prints a Line Source Function results heading, for PERSEUS.
C     (This is version 5 of LOVOA.)
C     !DASH
      save
C     !DASH
      real*8 GAMMA, TAUCL, VXS, WAVE, XC, XCL, XP, XR
      integer IB, ICE, IFDB, IL, IM, IN, IQEXA, IQINC, IQLSP, IQTAF, IU,
     $        JM, K, KFDB, KIC, KNT, LDL, LINT, LSFP, LU, MPROM, MSFT,
     $        N
      logical VZERO
      character BLANK*1, LINE*12, SPACE*13, TAT*8, TET*7, TIT*5, TKT*23,
     $          TMT*6, TOT*10, TUT*8, TYT*17
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
      equivalence (LINKDS(12),LDL  )
      equivalence (LINKDS(13),LINT )
      equivalence (LINKDS(14),LSFP )
      equivalence (LINKDS(15),IFDB )
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (RZQ(102),XCL  )
      equivalence (RZQ(103),TAUCL)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
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
      equivalence (IQQ( 51),IQINC)
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ(287),IQLSP)
      equivalence (IQQ( 26),IQTAF)
C     !DASH
C     !EJECT
      external  LINER, NAUGHTD, VELCRO, HI, BYE
      intrinsic max, min
C
C               VXS(N)
      dimension VXS(*)
C
      dimension TAT(2), TET(2), TIT(2), TOT(2), TUT(2), TMT(3)
C
      data SPACE /'             '/
      data TAT   /'complete',   ' partial'/
      data TET   /' Single',    'Blended'/
      data TIT   /' Half',      'Whole'/
      data TOT   /'Stationary', 'Expaanding'/
      data TUT   /'Constant',   ' Varying'/
      data TMT   /'  Full', 'Direct', 'Escape'/
C
      call HI ('LOVOA')
C     !BEG
      if((LU.gt.0).and.((LSFP.gt.0).or.(IQLSP.gt.0))) then
        TYT = BLANK
        if(IQEXA.gt.0) then
          call NAUGHTD (VXS, 1, N, VZERO)
          if(VZERO) then
            TYT='(Note that VXS=0)'
          end if
        end if
C
        KIC  = min((max(ICE,  0)), 1)
        KFDB = min((max(IFDB, 0)), 1)
        IN = min((max(IQEXA, 0)), 1)
        IM = min((max(LINT,  0)), 1)
        IB = min((max(LDL,   1)), 2)
        JM = min((max(MSFT,  0)), 2)
C
        if((MSFT.eq.0).or.(MSFT.eq.1)) then
          write (TKT,100) K
  100     format(I5,' frequency points.')
        else
          TKT = BLANK
        end if
C
        LINE = BLANK
        KNT  = 1
        if(IQINC.gt.0) then
          LINE = '------------'
          KNT  = 13
        end if
        call LINER     (3, LU)
        write (LU,101) LINE
  101   format(' ',11('----------'),'-----',A12)
C     !EJECT
        call LINER    (1, LU)
        write (LU,102) IU,IL,WAVE,SPACE(1:KNT),TET(IB)
  102   format(' ','Line Source Function for the (',I2,'/',I2,') ',
     $             'transition, at ',1PE16.8,' Angstroms.',A,23X,A7,
     $             ' line')
        write (LU,103) TAT(KIC+1),TKT,SPACE(1:KNT),TIT(IM+1)
  103   format(' ','Solution with ',A8,' redistribution.',18X,A23,A,
     $             10X,A5,'-profile integration')
C
        if(MPROM.gt.0) then
          write (LU,104)
  104     format(' ','Convolved Stark line profile.')
        end if
C
        if(KIC.gt.0) then
          call VELCRO (LU, IU, IL, ICE, GAMMA, XC, XP, XR, XCL, TAUCL)
        end if
C
        call LINER    (1, LU)
        write (LU,105) TOT(IN+1),TYT,TMT(JM+1),SPACE(1:KNT),TUT(KFDB+1)
  105   format(' ',A10,' atmosphere ',A17,10X,A6,' solution ',
     $             'requested',16X,A,A8,' background data')
C
        if(IQTAF.le.0) then
          call LINER  (1, LU)
          write (LU,106)
  106     format(' ','(Use option TAUPRNT to print the details of the ',
     $               'TAU calculation.)')
        end if
C
        call LINER    (1, LU)
        write (LU,101) LINE
      end if
C     !END
      call BYE ('LOVOA')
C
      return
      end
