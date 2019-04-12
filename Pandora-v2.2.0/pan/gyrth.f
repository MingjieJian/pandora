      subroutine GYRTH
     $(N,J,K,DL,SNU,WVL,VXA,EMU,S,W)
C
C     Rudolf Loeser, 1983 Sep 01
C---- Controls computation of shifted SNU, for the plane-parallel case.
C     The result returns in S.
C     !DASH
      save
C     !DASH
      real*8 DL, EMU, S, SNU, VXA, W, WVL
      integer IDLR, IDLS, IDV, IFL, IMU, IN, IS, ISNR, IVP, J, K, MOX,
     $        N
C     !DASH
      external ESGAR, BOSHAM, WGIVE, HI, BYE
C
      dimension W(*)
C
C               DL(KM), SNU(N,KM), VXA(N), S(N)
      dimension DL(*),  SNU(*),    VXA(*), S(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IMU   ),(IN( 2),IVP   ),(IN( 3),IDV   ),(IN( 4),IDLS  ),
     $(IN( 5),IDLR  ),(IN( 6),ISNR  ),(IN( 7),IFL   )
C
      call HI ('GYRTH')
C     !BEG
C     (Get, and allocate, W allotment)
      call ESGAR  (IN,IS,MOX,'GYRTH')
C
      call BOSHAM (N,J,K,DL,SNU,WVL,EMU,VXA,S,W(IMU),W(IVP),W(IDV),
     $             W(IDLS),W(IDLR),W(ISNR),W(IFL))
C
C     (Give back W allotment)
      call WGIVE  (W,'GYRTH')
C     !END
      call BYE ('GYRTH')
C
      return
      end
