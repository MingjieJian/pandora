      subroutine GUSTY
     $(L,XLM,EMU,XNE,TE,V,H1,VEX,OX2N1,OX2NL,OX2BDU,OX2BDL,ITAU,
     $ DMPI,OPAC)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes simulated background O-II line opacity.
C     !DASH
      save
C     !DASH
      real*8 CRS, DLK, DW, EMU, GTN, H1, OPAC, OX2BDL, OX2BDU, OX2N1,
     $       OX2NL, PHI, TE, V, VEX, XLM, XNE
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
      external GITANA, PHILO, HI, BYE
C
      dimension CRS(LX2L)
C
      data CRS /LX2L*0.D0/
C
      call HI ('GUSTY')
C     !BEG
C---- Compute DW and GTN
      call GITANA (TE, V, X2NUU(L), X2NUL(L), X2MAS, X2PU(L), X2PL(L),
     $             X2AUL(L), OX2NL, OX2BDU, OX2BDL, DW, GTN, DMPI,
     $             ITAU)
C
      DLK = XLM-X2WVL(L)
C---- Compute DP and PHI
      call PHILO  (EMU, VEX, DLK, X2WVL(L), H1, OX2N1, XNE, TE, DW,
     $             LDLX2(L), X2DDL(1,L), X2CDL(1,L), X2CRD(1,L),
     $             X2CVW(1,L), X2CSK(1,L), X2SKE, CRS, PHI, DMPI, ITAU)
C
      OPAC = GTN*PHI
C     !END
      call BYE ('GUSTY')
C
      return
      end
