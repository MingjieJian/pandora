      subroutine CUP
     $(F1,S,N,B1,BDI,NL,B1NW,WEIT)
C
C     Rudolf Loeser, 1980 Jul 14
C---- Computes new, weighted B1, for ROPE.
C     (This is version 3 of CUP.)
C     !DASH
      save
C     !DASH
      real*8 B1, B1NW, BDI, F1, ONE, S, WBD, WEIT
      integer IQWSE, KOLEV, KWSS, N, NL
      character TIT*9
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
      equivalence (RZQ( 45),WBD  )
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
      equivalence (IQQ(124),IQWSE)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external ARRDIV, SET1, CAPRA, WEITING, MOVE1, HI, BYE
C
C               F1(N), S(N), B1(N), BDI(N,NL), B1NW(N), WEIT(N)
      dimension F1(*), S(*), B1(*), BDI(N,*),  B1NW(*), WEIT(*)
C
      call HI ('CUP')
C     !BEG
C---- Compute new B1
      call ARRDIV    (F1,S,B1NW,N)
C
      if(WBD.lt.ONE) then
C----   Get final, weighted B1
C
        call CAPRA   (KWSS)
        write (TIT,100) KOLEV
  100   format('Lyman-B',I2)
C
        call SET1    (WEIT,N,WBD)
        call WEITING (BDI(1,KOLEV),B1NW,B1,N,WEIT,IQWSE,KWSS,TIT)
C
      else
C----   No weightin required
        call MOVE1   (B1NW,N,B1)
      end if
C     !END
      call BYE ('CUP')
C
      return
      end
