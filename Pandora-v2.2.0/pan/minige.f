      subroutine MINIGE
     $(IN,IS,MUX)
C
C     Rudolf Loeser, 1997 Jun 16
C---- Allocates the integer general data block.
C     !DASH
      save
C     !DASH
      integer IN, IQAMD, IQVLG, IS, MUL, MUX, N, NAB, NCP, NL, NL2, NLM,
     $        NSL, NSLP, NVX, NZU
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(44),NCP)
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(42),NVX)
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
      equivalence (IQQ(219),IQAMD)
      equivalence (IQQ(221),IQVLG)
C     !DASH
C     !EJECT
      external HI, BYE
C
      dimension IN(*)
C
      call HI ('MINIGE')
C     !BEG
      NL2  = NL*NL
      NLM  = NL-1
      MUL  = (NL*NLM)/2
      NSLP = NSL+1
      NZU  = 0
      if((IQAMD.gt.0).or.(IQVLG.gt.0)) then
        NZU = N*20
      end if
C
      IN(  1) = IS
C
      IN(  2) = IN(  1)+NSLP
      IN(  3) = IN(  2)+NL2
      IN(  4) = IN(  3)+NL
      IN(  5) = IN(  4)+NSL
      IN(  6) = IN(  5)+NSL
      IN(  7) = IN(  6)+NCP
      IN(  8) = IN(  7)+NAB
      IN(  9) = IN(  8)+MUL
      IN( 10) = IN(  9)+NSL
      IN( 11) = IN( 10)+NSL
C
      IN( 12) = IN( 11)+NSL
      IN( 13) = IN( 12)+NSL
      IN( 14) = IN( 13)+NVX
      IN( 15) = IN( 14)+N
      IN( 16) = IN( 15)+NZU
      IN( 17) = IN( 16)+NL2
      MUX     = IN( 17)+NSL
C     !END
      call BYE ('MINIGE')
C
      return
      end
