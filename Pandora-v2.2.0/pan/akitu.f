      subroutine AKITU
     $(KKU,XKX,AKX,W,IW)
C
C     Rudolf Loeser, 1981 Mar 31
C---- Controls computation of Lyman continuum integration weights.
C
C---- Aborts the run if this calcuation fails.
C     !DASH
      save
C     !DASH
      real*8 AKX, TENTH, W, XKX, Y, YALYM, ZERO
      integer I, IQAED, IW, KBAD, KK, KKU, KNEGA, KODE
      logical BAD, PRNT
      character TIT*8
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
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
      equivalence (RZQ(  8),Y    )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (REST( 3),YALYM)
      equivalence (LEST(56),KNEGA)
C     !EJECT
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
      equivalence (IQQ(343),IQAED)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(19),TENTH )
C     !DASH
C     !EJECT
      external  KOA, WARNPOS, SWAT, HALT, HI, BYE
      intrinsic max
C
      dimension W(*), IW(*)
C
C               XKX(KKX), AKX(KKX)
      dimension XKX(*),   AKX(*)
C
      data KBAD,PRNT /2, .false./
      data TIT       /'Lyman-AK'/
C
      call HI ('AKITU')
C     !BEG
C---- Initialize value of y
      YALYM = Y
C
  100 continue
C---- Compute with current value of y
      call KOA     (XKX, KKU, YALYM, AKX, W, IW, KODE)
      if(KODE.eq.0) then
        write (MSSLIN(1),101) YALYM
  101   format('Lyman continuum integration weights calculation ',
     $         'failed with y =',1PE10.2,' .')
        call HALT  ('AKITU', 1)
      end if
C
C---- Check to see whether any values < 0
      call WARNPOS (AKX, KKU, KBAD, TIT, 'AKITU', PRNT, BAD)
      if(BAD) then
C----   Yes, some are
        if(YALYM.gt.ZERO) then
C----     Try again with smaller y
          YALYM = max((YALYM-TENTH),ZERO)
          KNEGA = KNEGA+1
          goto 100
        else
          if(IQAED.gt.0) then
C----       Replace bad values with 0
            do 102 I = 1,KKU
              if(AKX(I).lt.ZERO) then
                AKX(I) = ZERO
              end if
  102       continue
          end if
        end if
      end if
C     !END
      call BYE ('AKITU')
C
      return
      end
