      subroutine ECKE
     $(A,N,IS)
C
C     Rudolf Loeser, 1990 Aug 02
C---- Computes "signals" describing the run of A-values.
C     Given the array A(i), 1 .le. i .le. N,
C     this routine computes IS(i) such that
C     IS(i) = -1 if A(i) is a local minimum,
C     IS(i) = +1 if A(i) is a local maximum, or
C     IS(i) =  0 if neither.
C     (This is version 2 of ECKE.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IS, N
C     !DASH
      external HI, BYE
C
C               A(N), IS(N)
      dimension A(*), IS(*)
C
      call HI ('ECKE')
C     !BEG
      IS(1) = 0
C
      do 100 I = 2,(N-1)
        if      ((A(I).lt.A(I-1)).and.(A(I).lt.A(I+1))) then
                                                            IS(I) = -1
C
        else if ((A(I).gt.A(I-1)).and.(A(I).gt.A(I+1))) then
                                                            IS(I) = +1
C
        else
                                                            IS(I) =  0
C
        end if
  100 continue
C
      IS(N) = 0
C     !END
      call BYE ('ECKE')
C
      return
      end
