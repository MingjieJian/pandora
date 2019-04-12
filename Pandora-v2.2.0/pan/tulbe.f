      subroutine TULBE
     $(CALLER,DUMP)
C
C     Rudolf Loeser, 2002 Oct 29
C---- Sets DUMP switch and writes a header, b-from-b-ratios calculation.
C     !DASH
      save
C     !DASH
      integer IQPBD, LUEO, MO
      logical DUMP
      character CALLER*(*)
C     !COM
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
      equivalence (IQQ(130),IQPBD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('TULBE')
C     !BEG
      DUMP = (IQPBD.gt.0).and.(MO.gt.0)
      if(DUMP) then
        call MESHED (CALLER, 2)
        write (LUEO,100)
  100   format(' ','Details of "b from b-ratios" calculation.',58X,
     $             '(Controlled by option BDMP.)')
      end if
C     !END
      call BYE ('TULBE')
C
      return
      end
