      subroutine MISTY
     $(IN,IS,MUX,CALLER,NW)
C
C     Rudolf Loeser, 1995 Aug 24
C---- Allocates integer scratch storage for OOBLECK.
C     (This is version 2 of MISTY.)
C     !DASH
      save
C     !DASH
      integer IN, IQOSH, IS, LSG, MSG, MUX, N, NW
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ(190),IQOSH)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('MISTY')
C     !BEG
      call IGET (IS ,CALLER)
C
      LSG = NW*N
      MSG = LSG
      if(IQOSH.gt.0) then
        MSG = 0
      end if
C
      IN( 1) = IS
      IN( 2) = IN( 1)+MSG
      MUX    = IN( 2)+LSG
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('MISTY')
C
      return
      end
