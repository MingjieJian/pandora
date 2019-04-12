      subroutine BUSTY
     $(L,TE,O2BDU,O2BDL,ITAU,DMPI,S)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes simulated O-II background line source function.
C     !DASH
      save
C     !DASH
      real*8 O2BDL, O2BDU, S, TE
      integer ITAU, L
      logical DMPI
C     !COM
C---- WURGO       as of 2007 Jan 25
      parameter   (MX2L=1, LX2L=3)
      integer     MX2L, LX2L, IUX2, ILX2, LDLX2
      real*8      X2MAS, X2SKE, X2WVL, X2WLO, X2WHI, X2NUU, X2NUL, X2AUL
      real*8      X2PU,  X2PL,  X2DDL, X2CDL, X2CRD, X2CVW, X2CSK
      dimension   X2WVL(MX2L), X2WLO(MX2L), X2WHI(MX2L), X2NUU(MX2L),
     $            X2NUL(MX2L), X2PU(MX2L),  X2PL(MX2L),  X2AUL(MX2L),
     $            IUX2(MX2L),  ILX2(MX2L),  LDLX2(MX2L)
      dimension   X2DDL(LX2L,MX2L), X2CDL(LX2L,MX2L),
     $            X2CRD(LX2L,MX2L), X2CVW(LX2L,MX2L), X2CSK(LX2L,MX2L)
      common      /WURGO0/ X2MAS,X2SKE
      common      /WURGO1/ X2WVL,X2WLO,X2WHI
      common      /WURGO2/ X2NUU,X2NUL,X2PU,X2PL
      common      /WURGO3/ X2AUL,X2DDL,X2CDL,X2CRD,X2CVW,X2CSK
      common      /WURGO4/ IUX2,ILX2,LDLX2
C     Data for Oxygen-II lines in the background.
C     .
C     !DASH
      external ESSEN, HI, BYE
C
      call HI ('BUSTY')
C     !BEG
      call ESSEN (X2NUU(L), X2NUL(L), TE, O2BDU, O2BDL, S, DMPI, ITAU)
C     !END
      call BYE ('BUSTY')
C
      return
      end
