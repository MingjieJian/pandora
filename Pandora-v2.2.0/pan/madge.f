      subroutine MADGE
     $(X,XLB1,JLFLX,JPROM)
C
C     Rudolf Loeser, 1998 Jul 29
C---- Some initializations for the current transition, for MINUET.
C     !DASH
      save
C     !DASH
      real*8 X, XLB1
      integer ILFLX, JLFLX, JPROM, KALTG, MO, MWNSV
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(67),MWNSV)
      equivalence (LEST(74),KALTG)
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
      equivalence (LINKDS(11),ILFLX)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external ZEUS, OAK, GOBY, CRUTO, GEMMA, HI, BYE
C
      dimension X(*)
C
C               XLB1(Li1len)
      dimension XLB1(*)
C
      call HI ('MADGE')
C     !BEG
C---- Print header
      call CRUTO (MO)
C---- Saved-WN-matrix count
      MWNSV = 0
C---- Line flux switch
      call ZEUS  (MO, ILFLX, JLFLX)
C---- Hydrogen Stark broadening switch
      call OAK
      call GOBY  (XLB1, JPROM)
C---- Hydrogen Lyman alpha & beta PRD GMMA switch
      call GEMMA (KALTG)
C     !END
      call BYE ('MADGE')
C
      return
      end
