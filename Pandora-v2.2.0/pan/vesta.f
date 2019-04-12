      subroutine VESTA
     $(TAU,XA,YA,CX,Z,N,II,TAUR,XAR,YAR,CXR,ZR,NR)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Makes reduced tables, for TUAREG.
C     !DASH
      save
C     !DASH
      real*8 CX, CXR, TAU, TAUR, XA, XAR, YA, YAR, Z, ZERO, ZR
      integer II, K, N, NR, NRM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  MOVE1, HI, BYE
      intrinsic max
C
C               TAU(N), XA(N), YA(N), TAUR(N), XAR(N), YAR(N), CXR(N),
      dimension TAU(*), XA(*), YA(*), TAUR(*), XAR(*), YAR(*), CXR(*),
C
C               CX(N), Z(N), ZR(N)
     $          CX(*), Z(*), ZR(*)
C
      call HI ('VESTA')
C     !BEG
      if(II.gt.0) then
        K = max((II-1),1)
        NRM = NR-1
        TAUR(1) = ZERO
        XAR(1) = XA(K)
        YAR(1) = YA(K)
        CXR(1) = CX(K)
        ZR(1) = Z(K)
        call MOVE1 (TAU(II),NRM,TAUR(2))
        call MOVE1 (XA(II) ,NRM,XAR(2) )
        call MOVE1 (YA(II) ,NRM,YAR(2) )
        call MOVE1 (CX(II) ,NRM,CXR(2) )
        call MOVE1 (Z(II)  ,NRM,ZR(2)  )
      else
        call MOVE1 (TAU    ,N  ,TAUR   )
        call MOVE1 (XA     ,N  ,XAR    )
        call MOVE1 (YA     ,N  ,YAR    )
        call MOVE1 (CX     ,N  ,CXR    )
        call MOVE1 (Z      ,N  ,ZR     )
      end if
C     !END
      call BYE ('VESTA')
C
      return
      end
