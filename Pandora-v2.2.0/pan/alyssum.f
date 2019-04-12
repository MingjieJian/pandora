      subroutine ALYSSUM
     $(NO,L,I,MLAB,LEGS)
C
C     Rudolf Loeser, 1989 Apr 17
C---- Prints for FOP.
C     !DASH
      save
C     !DASH
      integer I, IQAOP, L, NO
      character LEGS*111, MLAB*4
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
      equivalence (IQQ(258),IQAOP)
C     !DASH
      external ARALIA, ALLIUM, HI, BYE
C
      call HI ('ALYSSUM')
C     !BEG
      if(IQAOP.gt.0) then
        call ARALIA   (NO,L,  ONAME(I),MLAB     )
      else
        if(MLAB.ne.'ZZZZ') then
          call ALLIUM (NO,L,I,ONAME(I),MLAB,LEGS)
        end if
      end if
C     !END
      call BYE ('ALYSSUM')
C
      return
      end
