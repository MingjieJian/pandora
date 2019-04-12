      subroutine BOUR
     $(SAP,GMA,PHI,XJNU,XJBR,FAB,FJN,FJJ,GJN,DJB,XK1,XK2,N,K,LDL)
C
C     Rudolf Loeser, 1988 Apr 01
C---- Saves PRD debug checksums.
C     !DASH
      save
C     !DASH
      real*8 DJB, FAB, FJJ, FJN, GJN, GMA, PHI, SAP, XJBR, XJNU, XK1,
     $       XK2
      integer K, KOUNT, LDL, MO, N, NK, NL
      character T3*1, T5*1, TIT*40
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external  CHECKER, MISO, HI, BYE
      intrinsic mod
C
C               PHI(N,K), FAB(N,K), FJN(N,K), FJJ(N,K), SAP(N,LDL),
      dimension PHI(*),   FAB(*),   FJN(*),   FJJ(*),   SAP(*),
C
C               XJNU(N,K), XJBR(N), GJN(N,K), XK1(N,K), XK2(N,K),
     $          XJNU(*),   XJBR(*), GJN(*),   XK1(*),   XK2(*),
C
C               DJB(N,K), GMA(N)
     $          DJB(*),   GMA(*)
C
      data TIT,T3,T5 /'PRDn ---', '3', '5'/
      data KOUNT /0/
C
      call HI ('BOUR')
C     !BEG
      if(MO.gt.0) then
C
C       (Need to fiddle with label since BOUR is called 2x per iter.)
        KOUNT = KOUNT+1
        if(mod(KOUNT,2).eq.1) then
          TIT(4:4) = T3
        else
          TIT(4:4) = T5
        end if
C
        NK = N*K
        NL = N*LDL
C
        call MISO (TIT(9:33))
C     !EJECT
        TIT(6:8) = 'SAP'
        call CHECKER (SAP,  1, NL, TIT)
C
        TIT(6:8) = 'GMA'
        call CHECKER (GMA,  1, N,  TIT)
C
        TIT(6:8) = 'PHI'
        call CHECKER (PHI,  1, NK, TIT)
C
        TIT(6:8) = 'JNU'
        call CHECKER (XJNU, 1, NK, TIT)
C
        TIT(6:8) = 'JBR'
        call CHECKER (XJBR, 1, N,  TIT)
C
        TIT(6:8) = 'FAB'
        call CHECKER (FAB,  1, NK, TIT)
C
        TIT(6:8) = 'FJN'
        call CHECKER (FJN,  1, NK, TIT)
C
        TIT(6:8) = 'FJJ'
        call CHECKER (FJJ,  1, NK, TIT)
C
        TIT(6:8) = 'GJN'
        call CHECKER (GJN,  1, NK, TIT)
C
        TIT(6:8) = 'DJB'
        call CHECKER (DJB,  1, NK, TIT)
C
        TIT(6:8) = 'K1 '
        call CHECKER (XK1,  1, NK, TIT)
C
        TIT(6:8) = 'K2 '
        call CHECKER (XK2,  1, NK, TIT)
C
      end if
C     !END
      call BYE ('BOUR')
C
      return
      end
