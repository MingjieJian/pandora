      subroutine ZAIRA
     $(S,Z,TAU,B,IU,IL,LU,SPHERE,IJECT,W)
C
C     Rudolf Loeser, 1977 Jan 27
C---- Supervises Line Source Function plotting.
C     !DASH
      save
C     !DASH
      real*8 B, S, TAU, W, Z
      integer IBL, IJECT, IL, IN, IQENH, IQLSG, IS, ISL, ISR, ISUL, IU,
     $        IZL, LU, MOX, N
      logical ENHANCE, SPHERE
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
      equivalence (IQQ(250),IQLSG)
      equivalence (IQQ( 38),IQENH)
C     !DASH
      external ISIDORA, RISK, SOBER, WGIVE, HI, BYE
C
      dimension W(*)
C
C               S(N), Z(N), TAU(N), B(N)
      dimension S(*), Z(*), TAU(*), B(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),ISR   ),(IN( 2),ISL   ),(IN( 3),IBL   ),(IN( 4),IZL   ),
     $(IN( 5),ISUL  )
C     !EJECT
C
      call HI ('ZAIRA')
C     !BEG
      if((LU.gt.0).and.(IQLSG.le.0)) then
C       (Get, and allocate, W allotment)
        call ISIDORA (IN,IS,MOX,'ZAIRA')
C
        ENHANCE = (.not.SPHERE).and.(IQENH.gt.0)
        if(ENHANCE) then
          call RISK  (S,N,Z,W(ISR))
        end if
C
        call SOBER   (N,TAU,Z,W(ISR),S,B,IU,IL,LU,W(ISL),W(IBL),W(IZL),
     $                W(ISUL),ENHANCE)
        IJECT = 0
C
C       (Give back W allotment)
        call WGIVE   (W,'ZAIRA')
      end if
C     !END
      call BYE ('ZAIRA')
C
      return
      end
