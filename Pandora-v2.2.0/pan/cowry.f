      subroutine COWRY
     $(XI,K,Y,A,W,IW,LAB,YUSED,GOOD)
C
C     Rudolf Loeser, 1992 Sep 29
C---- Controls the calculation of the frequency integration weights
C     used for a specific LSF.
C     Upon return, GOOD tells whether a good result was obtained.
C     (This is version 3 of COWRY.)
C     !DASH
      save
C     !DASH
      real*8 A, TENTH, W, XI, Y, YUSED, ZERO
      integer I, IQAED, IW, K, KBAD, KNEGA, KODE, KSYM
      logical BAD, GOOD, PRNT
      character LAB*(*), TIT*40
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(56),KNEGA)
C
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
      external  SWAT, WARNPOS, HI, BYE
      intrinsic max
C
      dimension W(*), IW(*)
C
C               XI(K), A(K)
      dimension XI(*), A(*)
C
      data KBAD,PRNT /2, .false./
C
      call HI ('COWRY')
C     !BEG
C---- Initialize value of y
      YUSED = Y
C
  100 continue
C---- Compute with current value of y
      call SWAT      (XI, K, YUSED, A, W, IW, KSYM, KODE)
      GOOD = KODE.eq.1
C
      if(GOOD) then
C----   Check to see whether any values < 0
        if(KSYM.eq.1) then
          TIT = LAB//', half'
        else
          TIT = LAB//', full'
        end if
        call WARNPOS (A, K, KBAD, TIT, 'COWRY', PRNT, BAD)
        if(BAD) then
C----     Yes, some are
          if(YUSED.gt.ZERO) then
C----       Try again with smaller y
            YUSED = max((YUSED-TENTH),ZERO)
            KNEGA = KNEGA+1
            goto 100
          else
            if(IQAED.gt.0) then
C----         Replace bad values with 0
              do 101 I = 1,K
                if(A(I).lt.ZERO) then
                  A(I) = ZERO
                end if
  101         continue
            end if
          end if
        end if
      end if
C     !END
      call BYE ('COWRY')
C
      return
      end
