      subroutine ALKIMA
     $(N,Z,HND, FAC,FR)
C
C     Rudolf Loeser, 1998 Mar 18
C---- Computes FAC and FR, for velocity calculations.
C     Assume that HND(i) .ge. HND(i-1), 2 .le. i .le. N.
C     !DASH
      save
C     !DASH
      real*8 DIV, FAC, FR, HND, HNDREF, ONE, R1N, T, Z, ZSTAR
      integer I, IQSFS, IRET, N
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
      equivalence (IQQ( 31),IQSFS)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external SET1, LININT, DIVIDE, HI, BYE
C
C               Z(N), HND(N), FR(N)
      dimension Z(*), HND(*), FR(*)
C
      data HNDREF /1.D10/
C
      call HI ('ALKIMA')
C     !BEG
      if(IQSFS.le.0) then
C----   Plane-parallel case (FR=1 by definition)
        FAC = HNDREF
        call SET1     (FR,N,ONE)
      else
C----   Spherical case
        if(HND(1).gt.HNDREF) then
C----     HND unusually large
          FAC = HND(1)
          ZSTAR = Z(1)
        else if(HND(N).lt.HNDREF) then
C----     HND unusually small
          FAC = HND(N)
          ZSTAR = Z(N)
        else
C----     normal HND
          FAC = HNDREF
C----     Get ZSTAR, the value of Z at HND=HNDREF
          call LININT (HND,1,Z,1,N, HNDREF,ZSTAR, 1,1,IRET)
        end if
C----   Compute FR for spherical case
        T = R1N+Z(N)
        DIV = T-ZSTAR
        do 100 I = 1,N
          call DIVIDE ((T-Z(I)),DIV,FR(I))
  100   continue
      end if
C     !END
      call BYE ('ALKIMA')
C
      return
      end
