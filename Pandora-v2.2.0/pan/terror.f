      subroutine TERROR
     $(N,MN1,MNG1)
C
C     Rudolf Loeser, 1989 Sep 12
C---- Default MN1 and MNG1 values.
C     (This is version 2 of TERROR.)
C     !DASH
      save
C     !DASH
      integer IQAN1, MN1, MNG1, N
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
      equivalence (IQQ(272),IQAN1)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  HALT, HI, BYE
      intrinsic min
C     !EJECT
C
      call HI ('TERROR')
C     !BEG
      if(IQAN1.gt.0) then
C
        if(MN1.le.0) then
          MN1 = N
        end if
        MN1 = min(MN1,N)
        if(MNG1.eq.0) then
          MNG1 = -MN1
        end if
C
        if((MN1.gt.0).and.(MN1.lt.2)) then
          write (MSSLIN(1),100) MN1
  100     format('MN1 =',I5,'. When MN1 is greater then 0, it must be ',
     $           'greater than 1')
          call HALT ('TERROR',1)
        end if
C
      end if
C     !END
      call BYE ('TERROR')
C
      return
      end
