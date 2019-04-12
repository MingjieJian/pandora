      subroutine HECKLE
     $(XLM,L,CORE,HELIUM2,YES,DUMP)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Tells whether to compute the L'th He-II background line at
C     this wavelength, and sets the debug switch.
C     (This is version 2 of HECKLE.)
C     !DASH
      save
C     !DASH
      real*8 CORE, XLM
      integer L, LHEDS
      logical DUMP, HELIUM2, YES
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(211),LHEDS)
C     !DASH
      external KAKI, HI, BYE
C
      call HI ('HECKLE')
C     !BEG
      call KAKI (XLM, CORE, HELIUM2, HEWLO(L), HEWVL(L), HEWHI(L),
     $           LHEDS, YES, DUMP)
C     !END
      call BYE ('HECKLE')
C
      return
      end
