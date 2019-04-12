      subroutine BAG
     $(N,NSL,NL,XND,BDI,BDL,RLC,RKC,CRL)
C
C     Rudolf Loeser, 1979 Oct 24
C---- Computes Cooling Rates for bound-free transitions.
C     !DASH
      save
C     !DASH
      real*8 BDI, BDL, BDX, CRL, RAT, RKC, RLC, XND
      integer I, IQLYM, J, KOLEV, N, NL, NSL
      logical FLAG
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
      equivalence (KZQ( 33),KOLEV)
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
      equivalence (IQQ( 13),IQLYM)
C     !DASH
      external DIVIDE, HI, BYE
C
C               XND(N,NL), BDI(N,NL), RLC(N,NSL), RKC(N,NSL), CRL(N,NL),
      dimension XND(N,*),  BDI(N,*),  RLC(N,*),   RKC(N,*),   CRL(N,*),
C
C               BDL(N)
     $          BDL(*)
C     !EJECT
C
      call HI ('BAG')
C     !BEG
      do 101 J=1,NL
C
        FLAG = ((IQLYM.gt.0).and.(J.eq.KOLEV))
C
        do 100 I = 1,N
          if(FLAG) then
            BDX = BDL(I)
          else
            BDX = BDI(I,J)
          end if
          call DIVIDE (RLC(I,J),BDX,RAT)
          CRL(I,J) = XND(I,J)*(RAT-RKC(I,J))
  100   continue
C
  101 continue
C     !END
      call BYE ('BAG')
C
      return
      end
