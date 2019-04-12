      subroutine DODDER
     $(Z,F,N,J,KODE)
C
C     Rudolf Loeser, 1981 Jul 09
C---- Attempts to repair a dip, for IMPROVE.
C     Returns with KODE=1 if able to repair, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 D, F, Z
      integer I, J, K, KODE, L, M, N
C     !DASH
      external HI, BYE
C
C               Z(N), F(N)
      dimension Z(*), F(*)
C
      call HI ('DODDER')
C     !BEG
      KODE = 1
C
C---- Look for end of dip.
      I = J-1
      K = J+1
      M = 0
      do 100 L = K,N
        if(F(L).gt.F(I)) then
          M = L
          goto 101
        end if
  100 continue
  101 continue
C
      if(M.eq.0) then
C----   Repair by extrapolation (if possible).
        if(I.le.1) then
          KODE = 0
        else
          D = (F(I)-F(1))/(Z(I)-Z(1))
          do 102 L = J,N
            F(L) = F(I)+(Z(L)-Z(I))*D
  102     continue
        end if
      else
C----   Repair by interpolation.
        D = (F(M)-F(I))/(Z(M)-Z(I))
        L = M-1
        do 103 K = J,L
          F(K) = F(I)+(Z(K)-Z(I))*D
  103   continue
      end if
C     !END
      call BYE ('DODDER')
C
      return
      end
