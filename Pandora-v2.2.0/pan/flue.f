      subroutine FLUE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Aug 14
C---- Computes Damping Parameters for radiative transitions.
C     !DASH
      save
C     !DASH
      real*8 W, X, dummy
      integer I, IFBRSW, IHN1, IN, IPOPK, IS, IW, IX, IXLB1, JJTE,
     $        JJXND, JJXNE, JJXNU, JJZ, JPROM, KLIN, LSFP, LU, LUA, LUD,
     $        MOX, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 26),JJXNU)
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
      equivalence (LINKDS( 3),KLIN )
      equivalence (LINKDS(14),LSFP )
C     !DASH
C     !EJECT
      external MULL, ZEUS, WGIVE, LIDGET, TWOBAM, NUT, CHIMNEY, LIDPUT,
     $         POGONIA, GOBY, ABJECT, OAK, PET, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IXLB1 ),(IN( 2),IPOPK ),(IN( 3),IHN1  )
C
      data IFBRSW /0/
C
      call HI ('FLUE')
C     !BEG
C     (Get, and allocate, W allotment)
      call MULL         (IN, IS, MOX, 'FLUE')
C
C---- Set up output units and print heading
      call TWOBAM       (LUD, LUA)
C---- Get populations data
      call POGONIA      (W(IPOPK), W(IHN1), W)
C
C---- Loop over all radiative transitions
      do 100 I = 1,NT
        call PET        (I)
        call OAK
        if(KLIN.eq.1) then
C----     Read Data Block
          call LIDGET   (W(IXLB1), 1, dummy, 0, dummy, 0, I)
C----     Adjust Hydrogen Stark broadening switch
          call GOBY     (W(IXLB1), JPROM)
C----     Compute and print
          call ZEUS     (LUD, LSFP, LU)
          call CHIMNEY  (X(JJZ), X(JJTE), X(JJXNE), JPROM, X(JJXND),
     $                   X(JJXNU), W(IHN1), IFBRSW, LU, W(IXLB1),
     $                   W(IPOPK), W)
C----     Update Data Block
          call LIDPUT   (W(IXLB1), 1, dummy, 0, dummy, 0, I)
C----     Analyze
          call ZEUS     (LUA, LSFP, LU)
          if(LU.gt.0) then
            call ABJECT (LU)
            call NUT    (X, W, IW, W(IXLB1), JPROM, LU)
          end if
        end if
  100 continue
C
C     (Give back W allotment)
      call WGIVE        (W, 'FLUE')
C     !END
      call BYE ('FLUE')
C
      return
      end
