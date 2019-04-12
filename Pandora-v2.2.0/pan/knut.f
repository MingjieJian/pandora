      subroutine KNUT
     $(N,J,K,DL,SNU,WVL,VXA,Z,IRAY,S,W)
C
C     Rudolf Loeser, 1983 Sep 02
C---- Controls computation of shifted SNU, for Shell Rays.
C     The result returns in S.
C     !DASH
      save
C     !DASH
      real*8 DL, R1N, S, SNU, VXA, W, WVL, Z
      integer IDLR, IDLS, IDV, IFL, IMU, IN, IRAY, IS, ISNR, IVP, IVXX,
     $        J, K, MOX, N
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
      external HYTHE, FECAMP, WGIVE, HI, BYE
C
      dimension W(*)
C
C               DL(KM), SNU(N,KM), VXA(N), Z(N), S(NRPMX)
      dimension DL(*),  SNU(*),    VXA(*), Z(*), S(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),IMU   ),(IN( 2),IVXX  ),(IN( 3),IVP   ),(IN( 4),IDV   ),
     $(IN( 5),IDLS  ),(IN( 6),IDLR  ),(IN( 7),ISNR  ),(IN( 8),IFL   )
C
      call HI ('KNUT')
C     !BEG
C     (Get, and allocate, W allotment)
      call HYTHE  (IN,IS,MOX,'KNUT')
C
      call FECAMP (N,J,K,DL,SNU,WVL,VXA,Z,R1N,IRAY,S,W(IMU),W(IVXX),
     $             W(IVP),W(IDV),W(IDLS),W(IDLR),W(ISNR),W(IFL))
C
C     (Give back W allotment)
      call WGIVE  (W,'KNUT')
C     !END
      call BYE ('KNUT')
C
      return
      end
