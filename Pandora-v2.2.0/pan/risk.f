      subroutine RISK
     $(SO,N,Z,SN)
C
C     Rudolf Loeser, 1975 Feb 12
C---- Applies R**2-enhancement to Source Function.
C     !DASH
      save
C     !DASH
      real*8 R, R1N, SN, SO, Z, ZERO, ZR
      integer I, IQENH, N
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
      equivalence (RZQ( 23),R1N  )
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
      equivalence (IQQ( 38),IQENH)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MOVE1, DIVIDE, HI, BYE
C
C               SO(N), Z(N), SN(N)
      dimension SO(*), Z(*), SN(*)
C     !EJECT
C
      call HI ('RISK')
C     !BEG
      if((IQENH.gt.0).and.(R1N.ne.ZERO)) then
        R = Z(N)+R1N
        do 100 I = 1,N
          call DIVIDE ((R-Z(I)),R1N,ZR)
          SN(I) = SO(I)*(ZR**2)
  100   continue
      else
        call MOVE1    (SO,N,SN)
      end if
C     !END
      call BYE ('RISK')
C
      return
      end
