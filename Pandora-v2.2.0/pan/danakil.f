      subroutine DANAKIL
     $(X,W,IW,LU,N,XND,BDI,DW,GTN,FXI,EQUL,SMTH)
C
C     Rudolf Loeser, 1980 Apr 09
C---- Computes GTN, an intermediate in the TAU calculation; and FXI,
C     an intermediate for the Sobolev escape probability solution.
C     !DASH
      save
C     !DASH
      real*8 BDI, DW, FXI, GTN, W, X, XND
      integer IL, IN, IS, ISTIM, ISTMA, ISTMAS, ISTMB, ISTMBS, IU, IW,
     $        IXX, JJAIJ, JJOLL, JJP, JJSET, JJXNU, KLIN, LU, MOX, N
      logical EQUL, SMTH
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ(182),JJOLL)
      equivalence (IZOQ(253),JJSET)
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
C     !EJECT
      external WATUSI, KILANDA, AZPRIN, INDARRD, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XND(N,NL), BDI(N,NL), DW(N), GTN(N), FXI(N)
      dimension XND(*),    BDI(*),    DW(*), GTN(*), FXI(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IXX   ),(IN( 2),ISTIM ),(IN( 3),ISTMA ),(IN( 4),ISTMB ),
     $(IN( 5),ISTMAS),(IN( 6),ISTMBS)
C
      call HI ('DANAKIL')
C     !BEG
C     (Get and allocate W allotment)
      call KILANDA (IN, IS, MOX, 'DANAKIL')
C
      call INDARRD (W(IXX), 1, 1, N)
      call AZPRIN  (X, W, IW, IU, IL, KLIN, N, XND, BDI, X(JJP),
     $              X(JJSET), W(IXX), W(ISTIM), EQUL, SMTH, W(ISTMA),
     $              W(ISTMB), W(ISTMAS), W(ISTMBS), LU)
      call WATUSI  (IU, IL, KLIN, X(JJXNU), X(JJP), X(JJAIJ), X(JJOLL),
     $              DW, XND, W(ISTIM), GTN, FXI)
C
C     (Give back W allotment)
      call WGIVE   (W, 'DANAKIL')
C     !END
      call BYE ('DANAKIL')
C
      return
      end
