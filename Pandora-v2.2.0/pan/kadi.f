      subroutine KADI
     $(LU,LUNP,N,MN1,MNG1,NL,Z,GVL1,G1,DIDG1,GNVCHK,GVL,GVI,GRF,WONE,
     $ GVO,W,IW)
C
C     Rudolf Loeser, 1998 Jul 21
C---- Fiddles with GNV-L and GNV-I, for PONTAR.
C     !DASH
      save
C     !DASH
      real*8 G1, GNVCHK, GRF, GVI, GVL, GVL1, GVO, W, Z
      integer IW, LU, LUNP, MN1, MNG1, N, NL
      logical DIDG1, WONE
C     !DASH
      external LETEM, DIKA, KULAN, PUDGY, PUDDING, HI, BYE
C
      dimension W(*), IW(*)
C
C               GVL(N,NL), GNVCHK(N), GVO(N,NL), GVI(N), GRF(N), G1(N),
      dimension GVL(N,*),  GNVCHK(*), GVO(*),    GVI(*), GRF(*), G1(*),
C
C               Z(N), GVL1(N)
     $          Z(*), GVL1(*)
C
      call HI ('KADI')
C     !BEG
C---- Compute GNVCHECK (and print ?)
      call DIKA    (GVL1, G1, DIDG1, GNVCHK, MN1, LUNP)
C
C---- (Replace GVL-1 by G1 ? [GNVCALC] )
      call LETEM   (LUNP, DIDG1, N, NL, MNG1, G1, GVL)
C
C---- (Sequential smoothing ? [DSMOOTH] , and print ? )
      call KULAN   (N, NL, Z, GVL, GVI, GVO, LU, W, IW)
C
C---- (Fudge GNV-values ? , and print ? )
      call PUDGY   (N, NL, GVL, GVI, GRF, WONE, LU)
C
C---- (Suppress selected GVL ? )
      call PUDDING (N, NL, GVL, LU)
C     !END
      call BYE ('KADI')
C
      return
      end
