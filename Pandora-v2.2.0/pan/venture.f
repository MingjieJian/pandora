      subroutine VENTURE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 2004 Aug 05
C---- Plays with data needed for Hydrogen Lyman background
C     contributor lines.
C     (This is version 2 of VENTURE.)
C     !DASH
      save
C     !DASH
      real*8 DNU, HYMAS, W, X, dummy1, dummy2
      integer I, IL, IU, IW, IX, JJBXI, JJTE, JJV
C     !COM
C---- FERGO       as of 2004 Aug 05
      parameter   (MHYL=24)
      integer     MHYL, IUHY, ILHY
      real*8      HYWVL, HYWLO, HYWHI, HYNUU, HYNUL
      dimension   HYWVL(MHYL), HYWLO(MHYL), HYWHI(MHYL), HYNUU(MHYL),
     $            HYNUL(MHYL), IUHY(MHYL),  ILHY(MHYL)
      common      /FERGO0/ HYWVL,HYWLO,HYWHI
      common      /FERGO1/ HYNUU,HYNUL
      common      /FERGO2/ IUHY,ILHY
C     Data for Hydrogen Lyman lines in the background.
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
      external ANGIE, HYDATA, DIREX, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data HYMAS /1.D0/
C
      call HI ('VENTURE')
C     !BEG
C---- Set up NU values
      do 100 I = 1,MHYL
        IU = IUHY(I)
        IL = ILHY(I)
        call HYDATA (IU, HYNUU(I), dummy1, dummy2)
        call HYDATA (IL, HYNUL(I), dummy1, dummy2)
  100 continue
C---- Set up central wavelengths
      do 101 I = 1,MHYL
        DNU = HYNUU(I)-HYNUL(I)
        call ANGIE  (DNU, HYWVL(I))
  101 continue
C---- Set up wavelength cut-offs
      call DIREX    (X(JJTE), X(JJV), X(JJBXI), HYMAS, HYWVL, HYWLO,
     $               HYWHI, MHYL)
C     !END
      call BYE ('VENTURE')
C
      return
      end
