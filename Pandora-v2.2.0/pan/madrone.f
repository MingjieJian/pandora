      subroutine MADRONE
     $(XI,K,Y,F,W,IW,KODE)
C
C     Rudolf Loeser, 1968 May 29
C---- Computes the inverse-F Matrix, for MYRTLE.
C     Returns with KODE=1 if all seems ok, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 F, W, XI, Y
      integer I, IW, J, K, KODE
      character TIT*40
C     !DASH
      external SEGMENT, MOTOR, HI, BYE
C
      dimension W(*), IW(*)
C
C               XI(K), F(K,K)
      dimension XI(*), F(K,*)
C
      data TIT /'F-Matrix for frequency sum weights'/
C
      call HI ('MADRONE')
C     !BEG
      do 101 I = 1,K
        do 100 J = 1,K
          call SEGMENT (J, (K+1), Y, XI(I), XI(J), F(I,J))
  100   continue
  101 continue
C
      call MOTOR       (F, K, TIT, W, IW, KODE)
C     !END
      call BYE ('MADRONE')
C
      return
      end
