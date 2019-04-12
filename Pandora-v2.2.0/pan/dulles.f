      subroutine DULLES
     $(A,N)
C
C     Rudolf Loeser, 1992 Apr 01
C---- Imposes limits on extrapolated input values.
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IQIXT, N
C     !COM
C---- EXTRSW      as of 1992 Apr 01
      real*8      XTRALMU,XTRALML
      common      /EXTRSW/ XTRALMU,XTRALML
C     Parameters controlling the extra(inter)polation of input data
C     from an auxiliary Z-scale to the Z-scale of the run.
C     XTRALMU is the allowed upper limit for extrapolated values;
C     XTRALML is the allowed lower limit for extrapolated values.
C     .
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
      equivalence (IQQ(297),IQIXT)
C     !DASH
      external  HI, BYE
      intrinsic abs, sign
C
C               A(N)
      dimension A(*)
C     !EJECT
C
      call HI ('DULLES')
C     !BEG
      if((N.gt.0).and.(IQIXT.gt.0)) then
C
        do 100 I = 1,N
          if(abs(A(I)).lt.XTRALML) then
            A(I) = sign(XTRALML,A(I))
          else if(abs(A(I)).gt.XTRALMU) then
            A(I) = sign(XTRALMU,A(I))
          end if
  100   continue
C
      end if
C     !END
      call BYE ('DULLES')
C
      return
      end
