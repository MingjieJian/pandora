      subroutine TAIL
     $(L,XLM,EMU,XNE,TE,V,H1,VEX,OX3N1,OX3NL,OX3BDU,OX3BDL,ITAU,
     $ DMPI,OPAC)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes simulated background O-III line opacity.
C     !DASH
      save
C     !DASH
      real*8 CRS, DLK, DW, EMU, GTN, H1, OPAC, OX3BDL, OX3BDU, OX3N1,
     $       OX3NL, PHI, TE, V, VEX, XLM, XNE
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
      external GITANA, PHILO, HI, BYE
C
      dimension CRS(LX3L)
C
      data CRS /LX3L*0.D0/
C
      call HI ('TAIL')
C     !BEG
C---- Compute DW and GTN
      call GITANA (TE, V, X3NUU(L), X3NUL(L), X3MAS, X3PU(L), X3PL(L),
     $             X3AUL(L), OX3NL, OX3BDU, OX3BDL, DW, GTN, DMPI,
     $             ITAU)
C
      DLK = XLM-X3WVL(L)
C---- Compute DP and PHI
      call PHILO  (EMU, VEX, DLK, X3WVL(L), H1, OX3N1, XNE, TE, DW,
     $             LDLX3(L), X3DDL(1,L), X3CDL(1,L), X3CRD(1,L),
     $             X3CVW(1,L), X3CSK(1,L), X3SKE, CRS, PHI, DMPI, ITAU)
C
      OPAC = GTN*PHI
C     !END
      call BYE ('TAIL')
C
      return
      end
