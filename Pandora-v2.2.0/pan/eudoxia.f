      subroutine EUDOXIA
     $(X,W,IW,XCBL,XLB1)
C
C     Rudolf Loeser, 1977 Sep 13
C---- Initializes LSF background Continuum blocks.
C     (This is version 2 of EUDOXIA.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XCBL, XLB1, dummy
      integer I, ICE, IFDB, IN, IS, ITYPE, IW, IWAVS, IWS, JN, JOPAC,
     $        JOPAT, KPRD, KTRN, MMDL, MMLAM, MOX, MUX, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(59),MMDL )
      equivalence (MML( 2),MMLAM)
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
      equivalence (LINKDS( 4),ICE  )
      equivalence (LINKDS(15),IFDB )
      equivalence (LINKDS(20),KTRN )
C     !DASH
      external LAVA, MORTAIN, PET, LIDGET, LEONIA, WGIVE, IGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XLB1(Li1len), XCBL(Miklen)
      dimension XLB1(*),      XCBL(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IWAVS )
C
      dimension JN(2)
      equivalence
     $(JN( 1),JOPAC ),(JN( 2),JOPAT )
C     !EJECT
C
      call HI ('EUDOXIA')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call LAVA       (IN, IS,  MOX, 'EUDOXIA')
      call MORTAIN    (JN, IWS, MUX, 'EUDOXIA')
C
C---- Loop over all transitions
      do 100 I = 1,NT
        call PET      (I)
C
        ITYPE = 0
        KPRD  = 0
        if(ICE.ne.0) then
          ITYPE = 4
          KPRD  = 1
        else if(IFDB.gt.0) then
          ITYPE = 16
        end if
C
        if(ITYPE.gt.0) then
          call LIDGET (XLB1, 1, dummy, 0, dummy, 0, I)
C----     Set up Continuum Blocks for this transition
          call LEONIA (X, XLB1(MMDL), KTRN, ITYPE, KPRD, XLB1(MMLAM),
     $                 XCBL, W(IWAVS), IW(JOPAC), IW(JOPAT))
        end if
  100 continue
C
C     (Give back W & IW allotments)
      call WGIVE      (W,  'EUDOXIA')
      call IGIVE      (IW, 'EUDOXIA')
C     !END
      call BYE ('EUDOXIA')
C
      return
      end
