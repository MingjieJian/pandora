      subroutine NAVAGA
     $(NO,LAB,LEN)
C
C     Rudolf Loeser, 1987 Aug 20
C---- Provides printout place-markers.
C     (This is version 2 of NAVAGA.)
C     !DASH
      save
C     !DASH
      integer I, IOVER, IQMIX, ITER, ITHSL, LEN, LITER, LUIX, LURO, NO,
     $        NSCT
      character BLANK*1, LAB*(*), LINE*128
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
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 3),ITER )
      equivalence (LEST(19),ITHSL)
      equivalence (LEST(24),LITER)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(11),LUIX )
      equivalence (LUNITS( 5),LURO )
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
      equivalence (IQQ(217),IQMIX)
C     !EJECT
C---- ILION       as of 1987 Aug 20
      integer     NUMSCT
      common      /ILION/ NUMSCT
C     Next available identifier for printout sections.
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  HI, BYE
      intrinsic mod
C
      call HI ('NAVAGA')
C     !BEG
      if(IQMIX.gt.0) then
        NSCT   = mod(NUMSCT,1000000)
        NUMSCT = NUMSCT+1
C
        LINE = BLANK
        write (LINE,100) NSCT,LAB(:LEN),IOVER,ITER,ITHSL,LITER
  100   format(' ','PSN',I6,2X,A,T35,'OVER=',I3,5X,'SUB=',I3,5X,
     $             'HSL=',I3,5X,'LYM=',I3)
C
        do 101 I = 5,9
          if(LINE(I:I).eq.BLANK) then
            LINE(I:I) = NUMBS(1)
          end if
  101   continue
C
        write (NO,102) LINE(2:)
  102   format('1',A)
C
        if(NO.eq.LURO) then
          write (LUIX,103) LINE
  103     format(A)
        end if
      end if
C     !END
      call BYE ('NAVAGA')
C
      return
      end
