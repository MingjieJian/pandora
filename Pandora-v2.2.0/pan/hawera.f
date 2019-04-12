      subroutine HAWERA
     $(X,K,L,NMAX,TAB,N)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Sets up a table of wavelengths (Angstroms) to capture
C     one (L=K) or all (L=0) of the He-II background lines.
C     (This is version 2 of HAWERA.)
C     !DASH
      save
C     !DASH
      real*8 TAB, X
      integer JJBXI, JJTE, JJV, K, KBX, L, LL, N, NMAX
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(11),KBX)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(254),JJBXI)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 12),JJV  )
C     !DASH
C     !EJECT
      external MUMBAI, HI, BYE
C
      dimension X(*)
C
C               TAB(NMAX)
      dimension TAB(*)
C
      call HI ('HAWERA')
C     !BEG
      LL = L
      if((LL.le.0).or.(LL.gt.MHEL)) then
        LL = MHEL
      end if
C
      call MUMBAI (K, LL, X(JJBXI), KBX, X(JJTE), X(JJV), HEMAS, HEWVL,
     $             LDLHE, LHEL, HEDDL, N, NMAX, TAB)
C     !END
      call BYE ('HAWERA')
C
      return
      end
