      subroutine GALL
     $(L,TE,O3BDU,O3BDL,ITAU,DMPI,S)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes simulated O-III background line source function.
C     !DASH
      save
C     !DASH
      real*8 O3BDL, O3BDU, S, TE
      integer ITAU, L
      logical DMPI
C     !COM
C---- WARGO       as of 2007 Jan 18
      parameter   (MX3L=2, LX3L=6)
      integer     MX3L, LX3L, IUX3, ILX3, LDLX3
      real*8      X3MAS, X3SKE, X3WVL, X3WLO, X3WHI, X3NUU, X3NUL, X3AUL
      real*8      X3PU,  X3PL,  X3DDL, X3CDL, X3CRD, X3CVW, X3CSK
      dimension   X3WVL(MX3L), X3WLO(MX3L), X3WHI(MX3L), X3NUU(MX3L),
     $            X3NUL(MX3L), X3PU(MX3L),  X3PL(MX3L),  X3AUL(MX3L),
     $            IUX3(MX3L),  ILX3(MX3L),  LDLX3(MX3L)
      dimension   X3DDL(LX3L,MX3L), X3CDL(LX3L,MX3L),
     $            X3CRD(LX3L,MX3L), X3CVW(LX3L,MX3L), X3CSK(LX3L,MX3L)
      common      /WARGO0/ X3MAS,X3SKE
      common      /WARGO1/ X3WVL,X3WLO,X3WHI
      common      /WARGO2/ X3NUU,X3NUL,X3PU,X3PL
      common      /WARGO3/ X3AUL,X3DDL,X3CDL,X3CRD,X3CVW,X3CSK
      common      /WARGO4/ IUX3,ILX3,LDLX3
C     Data for Oxygen-III lines in the background.
C     .
C     !DASH
      external ESSEN, HI, BYE
C
      call HI ('GALL')
C     !BEG
      call ESSEN (X3NUU(L), X3NUL(L), TE, O3BDU, O3BDL, S, DMPI, ITAU)
C     !END
      call BYE ('GALL')
C
      return
      end
