      subroutine NEAT
     $(X,IX,W,IW,PN,FN,SW,KNT,T,KRJ,LAST)
C
C     Rudolf Loeser, 1984 Oct 23
C---- Computes one or more versions of the Statistical Equilibrium
C     calculation terms PE (stored in PN) and FE (in FN).
C     (This is version 2 of NEAT.)
C     !DASH
      save
C     !DASH
      real*8 FN, PN, T, W, X
      integer IL, IU, IW, IX, KNT, KRJ, N
      logical DUMP, LAST, SW
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
C     !DASH
C     !EJECT
      external LACKEY, NOVA, GRUNCH, CHAIN, VAMOS, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               T(KNT), SW(KNT), PN(N,KNT), FN(N,KNT)
      dimension T(*),   SW(*),   PN(N,*),   FN(N,*)
C
      call HI ('NEAT')
C     !BEG
      call LACKEY   (IU, IL, DUMP)
      if(SW(1)) then
        call NOVA   (X, IX, W, IW, IU, IL,     KRJ, LAST, DUMP,
     $               PN(1,1), FN(1,1), T(1))
      end if
      if(SW(2)) then
        call GRUNCH (X, IX, W, IW, IU, IL, IU, KRJ, LAST, DUMP,
     $               PN(1,2), FN(1,2), T(2))
      end if
      if(SW(3)) then
        call GRUNCH (X, IX, W, IW, IU, IL, IL, KRJ, LAST, DUMP,
     $               PN(1,3), FN(1,3), T(3))
      end if
      if(SW(4)) then
        call CHAIN  (X, IX, W, IW, IU, IL,     KRJ, LAST, DUMP,
     $               PN(1,4), FN(1,4), T(4))
      end if
      if(SW(5)) then
        call VAMOS  (X, IX, W, IW, IU, IL,          LAST, DUMP,
     $               PN(1,5), FN(1,5), T(5))
      end if
C     !END
      call BYE ('NEAT')
C
      return
      end
