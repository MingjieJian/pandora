      subroutine COLAPSE
     $(KE,FE,NE,F,N)
C
C     Rudolf Loeser, 1971 Jan 04
C---- Collapses the table FE of length NE by retaining the first value,
C     eliminating the next KE values, retaining the next value,
C     eliminating the next KE values, etc.
C     !DASH
      save
C     !DASH
      real*8 F, FE
      integer KE, L, N, NE
C     !DASH
      external  MOVED, HI, BYE
C
C               FE(NE), F(N)
      dimension FE(*),  F(*)
C
      call HI ('COLAPSE')
C     !BEG
      L = KE+1
      N = NE/L+1
      call MOVED (FE,L,N,F,1,N)
C     !END
      call BYE ('COLAPSE')
C
      return
      end
