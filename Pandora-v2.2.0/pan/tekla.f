      subroutine TEKLA
     $(L,XLM,EMU,XNE,TE,V,H1,VEX,HE2N1,HE2NL,HE2BDU,HE2BDL,ITAU,
     $ DMPI,OPAC)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Computes simulated background He-II line opacity.
C     (This is version 2 of TEKLA.)
C     !DASH
      save
C     !DASH
      real*8 CRS, DLK, DW, EMU, GTN, H1, HE2BDL, HE2BDU, HE2N1, HE2NL,
     $       OPAC, PHI, TE, V, VEX, XLM, XNE
      integer ITAU, L
      logical DMPI
C     !COM
C---- FARGO       as of 2004 Jul 19
      parameter   (MHEL=7, LHEL=7)
      integer     MHEL, LHEL, IUHE, ILHE, LDLHE
      real*8      HEMAS, HESKE, HEWVL, HEWLO, HEWHI, HENUU, HENUL, HEAUL
      real*8      HEPU,  HEPL,  HEDDL, HECDL, HECRD, HECVW, HECSK
      dimension   HEWVL(MHEL), HEWLO(MHEL), HEWHI(MHEL), HENUU(MHEL),
     $            HENUL(MHEL), HEPU(MHEL),  HEPL(MHEL),  HEAUL(MHEL),
     $            IUHE(MHEL),  ILHE(MHEL),  LDLHE(MHEL)
      dimension   HEDDL(LHEL,MHEL), HECDL(LHEL,MHEL),
     $            HECRD(LHEL,MHEL), HECVW(LHEL,MHEL), HECSK(LHEL,MHEL)
      common      /FARGO0/ HEMAS,HESKE
      common      /FARGO1/ HEWVL,HEWLO,HEWHI
      common      /FARGO2/ HENUU,HENUL,HEPU,HEPL
      common      /FARGO3/ HEAUL,HEDDL,HECDL,HECRD,HECVW,HECSK
      common      /FARGO4/ IUHE,ILHE,LDLHE
C     Data for Helium-II lines in the background.
C     .
C     !DASH
      external GITANA, PHILO, HI, BYE
C
      dimension CRS(LHEL)
C
      data CRS /LHEL*0.D0/
C
      call HI ('TEKLA')
C     !BEG
C---- Compute DW and GTN
      call GITANA (TE, V, HENUU(L), HENUL(L), HEMAS, HEPU(L), HEPL(L),
     $             HEAUL(L), HE2NL, HE2BDU, HE2BDL, DW, GTN, DMPI,
     $             ITAU)
C
      DLK = XLM-HEWVL(L)
C---- Compute DP and PHI
      call PHILO  (EMU, VEX, DLK, HEWVL(L), H1, HE2N1, XNE, TE, DW,
     $             LDLHE(L), HEDDL(1,L), HECDL(1,L), HECRD(1,L),
     $             HECVW(1,L), HECSK(1,L), HESKE, CRS, PHI, DMPI, ITAU)
C
      OPAC = GTN*PHI
C     !END
      call BYE ('TEKLA')
C
      return
      end
