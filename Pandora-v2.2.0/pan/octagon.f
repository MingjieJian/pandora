      subroutine OCTAGON
     $(N,LDL,CRD,XNU,DP,GMA)
C
C     Rudolf Loeser, 2005 Feb 23
C---- Computes GMA for Hydrogen Lyman alpha or beta, according to
C     Cooper, Ballagh & Hubeny (1989).
C     !DASH
      save
C     !DASH
      real*8 CRD, DP, GMA, R1, R2, SA1, SA2, XDEN, XNU, XNUM
      integer I, IL, IU, L, LDL, N
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
C     !DASH
C     !EJECT
      external EDNA, DIVIDE, HI, BYE
C
C               XNU(NSL), DP(N,LDL), GMA(N), CRD(LDL)
      dimension XNU(*),   DP(N,*),   GMA(*), CRD(*)
C
      dimension SA1(3), SA2(3)
C
      data L   /1/
      data SA1 /0.D0, 6.265D8, 1.672D8/
      data SA2 /0.D0, 6.265D8, 1.897D8/
C
      call HI ('OCTAGON')
C     !BEG
      call EDNA     (XNU, IU, IL, R1)
      do 100 I = 1,N
        R2   = DP(I,L)-CRD(L)
        XNUM = R1*SA1(IU)
        XDEN = R1*SA2(IU)+R2
        call DIVIDE (XNUM, XDEN, GMA(I))
  100 continue
C     !END
      call BYE ('OCTAGON')
C
      return
      end
