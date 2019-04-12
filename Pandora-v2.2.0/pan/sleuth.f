      subroutine SLEUTH
     $(XLM,N,TE,V,VXS,HN,CON,OPAC)
C
C     Rudolf Loeser, 2004 Aug 31
C---- Computes CO-lines absorption term.
C     (This is version 2 of SLEUTH.)
C     !DASH
      save
C     !DASH
      real*8 CON, HN, OPAC, TE, V, VXS, XLM, ZERO
      integer I, IQCOD, N
      logical DMPI, DUMP
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
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(223),IQCOD)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external BELLA, YELLA, MASHED, HI, BYE
C
C               TE(N), V(N), VXS(N), CON(N), HN(N,LIMP), OPAC(N)
      dimension TE(*), V(*), VXS(*), CON(*), HN(N,*),    OPAC(*)
C
      call HI ('SLEUTH')
C     !BEG
      DUMP = (IQCOD.gt.0).and.(KONLUD.gt.0)
C
      do 100 I = 1,N
        if(CON(I).eq.ZERO) then
          OPAC(I) = ZERO
        else
C
          call YELLA    (XLM, I, DUMP, DMPI, 'SLEUTH')
          call BELLA    (XLM, TE(I), V(I), VXS(I), HN(I,1), CON(I), I,
     $                   DMPI, OPAC(I))
          if(DMPI) then
            call MASHED ('SLEUTH')
          end if
C
        end if
  100 continue
C     !END
      call BYE ('SLEUTH')
C
      return
      end
