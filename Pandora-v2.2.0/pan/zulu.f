      subroutine ZULU
     $(TAU,S,RHO,KOUNT)
C
C     Rudolf Loeser, 1984 Jan 24
C---- Saves line source function results for iterative summary.
C     (This is version 3 of ZULU.)
C     !DASH
      save
C     !DASH
      real*8 RHO, S, TAU
      integer IL, IQIRH, IQISS, IQITA, IU, KOUNT, KRHO, KSSS, KTAU, N
      logical KILROY
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ(111),IQITA)
      equivalence (IQQ(110),IQIRH)
      equivalence (IQQ(109),IQISS)
C     !DASH
      external KEVYN, HI, BYE
C
C               TAU(N), RHO(N), S(N)
      dimension TAU(*), RHO(*), S(*)
C
      data KTAU,KRHO,KSSS /1, 3, 11/
C
      call HI ('ZULU')
C     !BEG
      KILROY = KOUNT.eq.1
C     (The initialization signal KILROY should be true only for
C     the first transition in a sub-iteration.)
C
      call KEVYN (IQITA, KTAU, 'Tau', IU, IL, N, TAU, KILROY)
      call KEVYN (IQIRH, KRHO, 'Rho', IU, IL, N, RHO, KILROY)
      call KEVYN (IQISS, KSSS, 'S'  , IU, IL, N, S  , KILROY)
C     !END
      call BYE ('ZULU')
C
      return
      end
