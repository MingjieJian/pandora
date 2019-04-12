      subroutine THEKLA
     $(L,TE,HE2BDU,HE2BDL,ITAU,DMPI,S)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Computes simulated He-II background line source function.
C     (This is version 4 of THEKLA.)
C     !DASH
      save
C     !DASH
      real*8 HE2BDL, HE2BDU, S, TE
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
      external ESSEN, HI, BYE
C
      call HI ('THEKLA')
C     !BEG
      call ESSEN (HENUU(L), HENUL(L), TE, HE2BDU, HE2BDL, S, DMPI, ITAU)
C     !END
      call BYE ('THEKLA')
C
      return
      end
