      subroutine SVEIN
     $(N,J,K,DL,SNU,WVL,VXA,Z,FRR,S,W)
C
C     Rudolf Loeser, 1983 Sep 02
C---- Controls computation of shifted SNU, for Disk Rays.
C     The result returns in S.
C     !DASH
      save
C     !DASH
      real*8 DL, FRR, R1N, S, SNU, VXA, W, WVL, Z
      integer IDLR, IDLS, IDV, IFL, IMU, IN, IS, ISNR, IVP, J, K, MOX,
     $        N
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
C     !DASH
      external MORA, SENLAC, WGIVE, HI, BYE
C
      dimension W(*)
C
C               DL(KM), SNU(N,KM), VXA(N), Z(N), S(N)
      dimension DL(*),  SNU(*),    VXA(*), Z(*), S(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IMU   ),(IN( 2),IVP   ),(IN( 3),IDV   ),(IN( 4),IDLS  ),
     $(IN( 5),IDLR  ),(IN( 6),ISNR  ),(IN( 7),IFL   )
C
      call HI ('SVEIN')
C     !BEG
C     (Get, and allocate, W allotment)
      call MORA   (IN,IS,MOX,'SVEIN')
C
      call SENLAC (N,J,K,DL,SNU,WVL,VXA,Z,FRR,R1N,S,W(IMU),W(IVP),
     $             W(IDV),W(IDLS),W(IDLR),W(ISNR),W(IFL))
C
C     (Give back W allotment)
      call WGIVE  (W,'SVEIN')
C     !END
      call BYE ('SVEIN')
C
      return
      end
