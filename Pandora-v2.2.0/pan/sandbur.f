      subroutine SANDBUR
     $(I,IPOP,XLM,DUMP,CALLER)
C
C     Rudolf Loeser, 1988 May 10
C---- Determines whether or not to dump, for EVAN; and writes a dump
C     header if yes.
C     (This is version 2 of SANDBUR.)
C
C     See also VETIVER.
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer I, IPOP, IPPOD, LDINT, LUEO
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(100),IPPOD)
      equivalence (KZQ( 48),LDINT)
C     !DASH
      external DACAPO, LINER, PINNA, MESHED, HI, BYE
C     !EJECT
C
      call HI ('SANDBUR')
C     !BEG
      DUMP = .false.
C
      if(((IPPOD.eq.1).or.(IPPOD.eq.3)).and.(KONLUD.gt.0)) then
        call PINNA      (I, LDINT, DUMP)
C
        if(DUMP) then
          if(I.eq.1) then
            call DACAPO (XLM)
            call MESHED (CALLER, 1)
          end if
C
          call LINER    (2, LUEO)
          write (LUEO,100) IPOP,I,XLM
  100     format(' ','DUMP of opacity contributions from each level ',
     $               'of population ion #',I3,' at depth #',I4,
     $               ' and Lambda =',1PE20.13/
     $           ' ',15X,'N',12X,'BD',10X,'Te',8X,'h*nu/k*T',7X,
     $               'exp(-...)',15X,'H',12X,'term')
        end if
      end if
C     !END
      call BYE ('SANDBUR')
C
      return
      end
