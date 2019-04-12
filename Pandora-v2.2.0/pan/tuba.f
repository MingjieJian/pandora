      subroutine TUBA
     $(X,IX,W,IW,XLB1,XLB2,XCBL)
C
C     Rudolf Loeser, 1980 Aug 21
C---- Controls "POST"-processing for all transitions.
C     XLBL  is the buffer for the Line Intensity data block;
C     XCBL  is the buffer for the Continuum data block.
C     (This is version 2 of TUBA.)
C     !DASH
      save
C     !DASH
      real*8 T1, W, X, XCBL, XLB1, XLB2, dummy
      integer I, IHN1, IN, IPOPK, IS, IW, IX, KHED, KLIN, KUP1, KUP2,
     $        MOX, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
C     !EJECT
      external LUTE, LIDGET, LIDPUT, KASHMIR, POGONIA, BUCKEYE, SECOND,
     $         OAK, PET, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), XLB2(Li2len), XCBL(Miklen)
      dimension XLB1(*),      XLB2(*),      XCBL(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IPOPK ),(IN( 2), IHN1  )
C
      call HI ('TUBA')
C     !BEG
C     (Get, and allocate, W allotment)
      call LUTE          (IN, IS, MOX, 'TUBA')
C
      call POGONIA       (W(IPOPK), W(IHN1), W)
C
      do 100 I = 1,NT
        call PET         (I)
        call OAK
        if(((KLIN.eq.1).or.(KLIN.eq.2))) then
          call SECOND    (T1)
          KUP1 = 0
          KUP2 = 0
          KHED = 0
C----     Read data block
          call LIDGET    (XLB1, 1, XLB2, 1, dummy, 0, I)
C----     Do required processing
          call KASHMIR   (X, IX, W, IW, XLB1, XLB2, XCBL, W(IHN1),
     $                    W(IPOPK), KHED, KUP1, KUP2)
          if((KUP1.eq.1).or.(KUP2.eq.1)) then
C----       Rewrite data block with new results
            call LIDPUT  (XLB1, KUP1, XLB2, KUP2, dummy, 0, I)
          end if
          if(KHED.eq.1) then
C----       Wrap up
            call BUCKEYE (T1)
          end if
        end if
  100 continue
C
C     (Give back W allotment)
      call WGIVE         (W, 'TUBA')
C     !END
      call BYE ('TUBA')
C
      return
      end
