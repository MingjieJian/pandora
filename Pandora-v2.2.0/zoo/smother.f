      subroutine SMOTHER
     $(X,F,N,ITMX, LU,LD,TITLE, D,W, KRET)
C     Rudolf Loeser, 1997 Feb 04
C---- (97 Jan 31):
C     Sequential smoothing, irregular point spacing.
C     The smoothed values replace the original values in F.
C
C     Returns with KRET .ge. 0 if all seems OK:
C                            the value is the number of changes made
C             with KRET .lt. 0 in case of error:
C                       = -1 if N .lt. 5
C                       = -2 if, for any i>1, x(i)-x(i-1) .le. 0.
C
C     D and W are scratch storage.
C     !DASH
      save
C     !DASH
      real*8 D, F, HALF, ONE, P, W, X, ZERO
      integer I, ITER, ITMX, J, K, KALL, KRET, L, LD, LOOP, LU, M, N
      character TITLE*(*)
C     !DASH
      external  STEEPLY, SLURP
      intrinsic sign
C
      dimension X(N), F(N), D(N), W(N,2), LOOP(3,2)
C
      data ZERO, HALF, ONE /0.D0, 5.D-1, 1.D0/
C
C     !BEG
      call STEEPLY (X,D,N, KRET)
      if(KRET.lt.0) goto 105
C
      M = N-1
      L = N-2
C
      W(1,1) = F(1)
      W(1,2) = F(1)
      W(2,1) = F(2)
      W(2,2) = F(2)
      W(M,1) = F(M)
      W(M,2) = F(M)
      W(N,1) = F(N)
      W(N,2) = F(N)
C
      LOOP(1,1) =  3
      LOOP(2,1) =  L
      LOOP(3,1) = +1
C
      LOOP(1,2) =  L
      LOOP(2,2) =  3
      LOOP(3,2) = -1
C     !EJECT
      do 104 ITER = 1,ITMX
C
        do 100 I = 3,L
          W(I,1) = F(I)
          W(I,2) = F(I)
  100   continue
C
        KALL = 0
        do 102 J = 1,2
          K = 0
C
          do 101 I = LOOP(1,J), LOOP(2,J), LOOP(3,J)
            if(sign(ONE,(F(I-1)-F(I))).eq.sign(ONE,(F(I+1)-F(I)))) then
C
              call SLURP (D,W(1,J),I, P)
C
              if(P.ne.W(I,J)) then
                W(I,J) = P
                K      = K+1
              end if
            end if
  101     continue
C
          KALL = KALL+K
  102   continue
C
        do 103 I = 3,L
          F(I) = HALF*(W(I,1)+W(I,2))
  103   continue
        KRET = KRET+KALL
        if(KALL.le.0) goto 105
C
  104 continue
C
  105 continue
C     !END
C
      return
      end
