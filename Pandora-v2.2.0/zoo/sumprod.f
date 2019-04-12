      subroutine SUMPROD
     $(SUM,A,ISTRA,B,ISTRB,N)
C     Rudolf Loeser, 1993 May 26
C---- Computes the sum of the products A("i")*B("i"),
C     for 1 .le. "i" .le. N, going
C     through A with stride ISTRA, and
C     through B with stride ISTRB.
C     !DASH
      save
C     !DASH
      real*8 A, B, SUM, ZERO
      integer I, IA, IB, ISTRA, ISTRB, N
C     !DASH
      dimension A(*), B(*)
C
      data ZERO /0.D0/
C
C     !BEG
      IA = 1-ISTRA
      IB = 1-ISTRB
      SUM = ZERO
      do 100 I = 1,N
        IA = IA+ISTRA
        IB = IB+ISTRB
        SUM = SUM+A(IA)*B(IB)
  100 continue
C     !END
C
      return
      end
