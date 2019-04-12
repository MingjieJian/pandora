      subroutine SPOTTY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 2004 Apr 20
C---- Plays with data needed for O-I background contributor line.
C     !DASH
      save
C     !DASH
      real*8 DNU, W, X
      integer I, IW, IX, JJBXI, JJTE, JJV
C     !COM
C---- FURGO       as of 2004 Jun 11
      parameter   (MOXL=11)
      integer     MOXL, IUOX, ILOX
      real*8      OXMAS, OXSKE, OXWVL, OXWLO, OXWHI, OXNUU, OXNUL
      real*8      OXPU,  OXPL,  OXAUL, OXCRD, OXCVW, OXCSK
      dimension   OXWVL(MOXL), OXWLO(MOXL), OXWHI(MOXL), OXNUU(MOXL),
     $            OXNUL(MOXL), OXPU(MOXL),  OXPL(MOXL),  OXAUL(MOXL),
     $            OXCRD(MOXL), OXCVW(MOXL), OXCSK(MOXL),
     $            IUOX(MOXL),  ILOX(MOXL)
      common      /FURGO0/ OXMAS,OXSKE
      common      /FURGO1/ OXWVL,OXWLO,OXWHI
      common      /FURGO2/ OXNUU,OXNUL,OXPU,OXPL
      common      /FURGO3/ OXAUL,OXCRD,OXCVW,OXCSK
      common      /FURGO4/ IUOX,ILOX
C     Data for Oxygen-I lines in the background.
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
      external ANGIE, DIREX, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('SPOTTY')
C     !BEG
C---- Set up central wavelengths
      do 100 I = 1,MOXL
        DNU = OXNUU(I)-OXNUL(I)
        call ANGIE (DNU, OXWVL(I))
  100 continue
C---- Set up wavelength cut-offs
      call DIREX   (X(JJTE), X(JJV), X(JJBXI), OXMAS, OXWVL, OXWLO,
     $              OXWHI, MOXL)
C     !END
      call BYE ('SPOTTY')
C
      return
      end
