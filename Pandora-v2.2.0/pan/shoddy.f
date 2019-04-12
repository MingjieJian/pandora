      subroutine SHODDY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 2005 Jun 24
C---- Plays with data needed for He-I background contributor line.
C     !DASH
      save
C     !DASH
      real*8 DNU, W, X
      integer I, IW, IX, JJBXI, JJTE, JJV
C     !COM
C---- FIRGO       as of 2005 Jul 07
      parameter   (MHEE=4)
      integer     MHEE, IUHEE, ILHEE
      real*8      HEEMAS, HEEWVL, HEEWLO, HEEWHI, HEENUU, HEENUL
      real*8      HEEAUL, HEEPU,  HEEPL,  HEECRD, HEECVW, HEECSK
      real*8      HEESKE
      dimension   HEEWVL(MHEE), HEEWLO(MHEE), HEEWHI(MHEE),
     $            HEENUU(MHEE), HEENUL(MHEE), HEEPU(MHEE),
     $            HEEPL(MHEE),  HEEAUL(MHEE), HEECRD(MHEE),
     $            HEECVW(MHEE), HEECSK(MHEE),
     $            IUHEE(MHEE),  ILHEE(MHEE)
      common      /FIRGO0/ HEEMAS,HEESKE
      common      /FIRGO1/ HEEWVL,HEEWLO,HEEWHI
      common      /FIRGO2/ HEENUU,HEENUL,HEEPU,HEEPL
      common      /FIRGO3/ HEEAUL,HEECRD,HEECVW,HEECSK
      common      /FIRGO4/ IUHEE,ILHEE
C     Data for Helium lines in the background.
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
      external ANGIE, DIREX, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('SHODDY')
C     !BEG
C---- Set up central wavelengths
      do 100 I = 1,MHEE
        DNU = HEENUU(I)-HEENUL(I)
        call ANGIE (DNU, HEEWVL(I))
  100 continue
C---- Set up wavelength cut-offs
      call DIREX   (X(JJTE), X(JJV), X(JJBXI), HEEMAS, HEEWVL, HEEWLO,
     $              HEEWHI, MHEE)
C     !END
      call BYE ('SHODDY')
C
      return
      end
