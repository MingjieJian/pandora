      subroutine SCOTTY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Plays with data needed for He-II background contributor lines.
C     !DASH
      save
C     !DASH
      real*8 DNU, W, X
      integer I, IW, IX, JJBXI, JJTE, JJV
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
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ(254),JJBXI)
C     !DASH
C     !EJECT
      external ANGIE, DIREX, RITTER, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('SCOTTY')
C     !BEG
C---- Set up central wavelengths
      do 100 I = 1,MHEL
        DNU = HENUU(I)-HENUL(I)
        call ANGIE (DNU, HEWVL(I))
  100 continue
C---- Set up wavelength cut-offs
      call DIREX   (X(JJTE), X(JJV), X(JJBXI), HEMAS, HEWVL, HEWLO,
     $              HEWHI, MHEL)
C---- Modify for blends
      call RITTER  (MHEL, LHEL, LDLHE, HEDDL, HEWLO, HEWHI)
C     !END
      call BYE ('SCOTTY')
C
      return
      end
