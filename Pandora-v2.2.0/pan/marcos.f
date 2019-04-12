      subroutine MARCOS
     $(X,W,K,LU,N,GMA,XJBR,PE,FE,EP,XK2,FRD,GRD,XRD,ZRD,YRD)
C
C     Rudolf Loeser, 2005 Jan 13
C---- Attempts to compute XRD, YRD, CHL, DHL (& intermediates), for PRD.
C     !DASH
      save
C     !DASH
      real*8 EP, FE, FRD, GMA, GRD, PE, W, X, XJBR, XK2, XRD, YRD, ZRD
      integer IL, IN, IS, ITF, ITG, IU, JJAIJ, JJALF, JJBAT, JJP, JJXND,
     $        K, LU, MOX, N
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ( 25),JJALF)
      equivalence (IZOQ( 39),JJBAT)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 59),JJXND)
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
      external MOSCA, SPROING, ZHAY, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               XK2(N,K), FRD(N,K), GRD(N,K), XJBR(N), XRD(N,K), EP(N),
      dimension XK2(*),   FRD(*),   GRD(*),   XJBR(*), XRD(*),   EP(*),
C
C               FE(N), GMA(N), YRD(N,K), PE(N), ZRD(N,K)
     $          FE(*), GMA(*), YRD(*),   PE(*), ZRD(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),ITF   ),(IN( 2),ITG   )
C
      call HI ('MARCOS')
C     !BEG
C     (Get, and allocate, W allotment)
      call MOSCA   (IN, IS, MOX, 'MARCOS')
C
C---- Compute intermediate terms TF and TG
      call SPROING (N, IU, IL, X(JJAIJ), X(JJP), X(JJALF), X(JJBAT),
     $              PE, FE, X(JJXND), XJBR, GMA, LU, W(ITF), W(ITG))
C---- Compute FRD, GRD, XRD, ZRD, and YRD
      call ZHAY    (N, K, EP, W(ITF), W(ITG), XK2, FRD, GRD,
     $              XRD, ZRD, YRD)
C
C     (Give back W allotment)
      call WGIVE   (W, 'MARCOS')
C     !END
      call BYE ('MARCOS')
C
      return
      end
