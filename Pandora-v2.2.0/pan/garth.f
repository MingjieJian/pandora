      subroutine GARTH
     $(C,N,YA,WN,RR)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Accumulates an angle contribution, for RR-calculation.
C     !DASH
      save
C     !DASH
      real*8 C, RR, SUM, WN, YA
      integer I, N
C     !DASH
      external SUMPROD, HI, BYE
C
C               YA(N,N), WN(N,N), RR(N), C(N)
      dimension YA(N,*), WN(N,*), RR(*), C(*)
C
      call HI ('GARTH')
C     !BEG
      do 100 I = 1,N
        call SUMPROD (SUM,WN(I,1),N,YA(I,1),N,N)
C
C       (Adding the diagonal element of YA again is needed because YA
C       needs to be multiplied by (WN+1), whereas, in the preceding call
C       to SUMPROD, it is multiplied by just (WN).)
        SUM = SUM+YA(I,I)
C
        RR(I) = RR(I)+C(I)*SUM
  100 continue
C     !END
      call BYE ('GARTH')
C
      return
      end
