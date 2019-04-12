      subroutine STORY
     $(DUMP,CALLER,XLM,LIMP,LEV,FAC)
C
C     Rudolf Loeser, 2003 Jul 11
C---- Dump for H-bf emission.
C     !DASH
      save
C     !DASH
      real*8 FAC, XLM
      integer JHBFD, LEV, LIMP, LUEO
      logical DUMP
      character CALLER*(*)
C     !COM
C---- KONOUT      as of 2004 Jan 08
      integer     KONLUN,KONLUR,KONLUD,KONHED
      common      /KONOUT/ KONLUN,KONLUR,KONLUD,KONHED
C     Logical output unit numbers for Continuum Calculations.
C             *** Initialized in PARLOR. ***
C
C     KONLUN: potential output unit number
C     KONLUR: unit number for regular output
C
C     KONLUD: =1 if dump output is authorized
C     KONHED: =1 if wavelength header has already been printed
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
      equivalence (KZQ(149),JHBFD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MALTA, STERK, RANGOON, HI, BYE
C     !EJECT
C
      call HI ('STORY')
C     !BEG
      DUMP = (KONLUD.gt.0).and.(JHBFD.ne.0)
C
      if(DUMP) then
        call MALTA   (XLM, DUMP, CALLER)
        call STERK   (XLM, 'emission')
        write (LUEO,100) LIMP,LEV,FAC
  100   format(' ','LIMP =',I3,5X,'LEV =',I3,5X,'FAC =',1PE16.8)
        call RANGOON (XLM)
      end if
C     !END
      call BYE ('STORY')
C
      return
      end
