      subroutine PRIAM
     $(NO,LAB,LEN)
C
C     Rudolf Loeser, 1987 Aug 08
C---- Provides a GIANT-letters Label for a printout section, and
C     place-markers (if needed).
C     !DASH
      save
C     !DASH
      integer IQMIX, LEN, NO
      character BLANK*1, LAB*(*)
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
      equivalence (IQQ(217),IQMIX)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external NAVAGA, ABJECT, LINER, HECTOR, HI, BYE
C     !EJECT
C
      call HI ('PRIAM')
C     !BEG
      if(NO.gt.0) then
        if(IQMIX.gt.0) then
          call NAVAGA (NO, LAB, LEN)
          call LINER  (3, NO)
        else
          call ABJECT (NO)
        end if
        call HECTOR   (LAB, LEN, LEN, 0, NO, BLANK)
      end if
C     !END
      call BYE ('PRIAM')
C
      return
      end
