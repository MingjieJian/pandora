      subroutine TULERI
     $(DDT,DUMP,CALLER)
C
C     Rudolf Loeser, 2003 Dec 31
C---- Sets up dump for Dust Temperature calculation.
C     (This is version 2 of TULERI.)
C     !DASH
      save
C     !DASH
      real*8 DDT
      integer IQD2D, LUEO, MO
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
      equivalence (IQQ(211),IQD2D)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, HI, BYE
C     !EJECT
C
      call HI ('TULERI')
C     !BEG
      DUMP = (IQD2D.gt.0).and.(MO.gt.0)
      if(DUMP) then
        call MESHED (CALLER, 2)
        write (LUEO,100) DDT
  100   format(' ','Dump for Type-2 Dust Temperature calculation.',
     $             T112,'(Option DUSTDMP)'//
     $         ' ','DDT =',1PE12.5)
        call LINER  (1, LUEO)
      end if
C     !END
      call BYE ('TULERI')
C
      return
      end
