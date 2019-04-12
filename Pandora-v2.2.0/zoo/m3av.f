      subroutine M3AV
     $(FOLD,FNEW,N)
C     Rudolf Loeser, 1999 Jan 13
C---- Moving-3-point average, weighted 1-2-1.
C     !DASH
      save
C     !DASH
      real*8 FNEW, FOLD, FOUR, THREE, TWO
      integer I, N
C     !DASH
      dimension FOLD(N), FNEW(N)
C
      data      TWO, THREE, FOUR /2.D0, 3.D0, 4.D0/
C
C     !BEG
      if(N.gt.0) then
        if(N.lt.3) then
          do 100 I = 1,N
            FNEW(I) = FOLD(I)
  100     continue
        else
          FNEW(1) = (TWO*FOLD(1)+FOLD(2))/THREE
          do 101 I = 2,(N-1)
            FNEW(I) = (FOLD(I-1)+TWO*FOLD(I)+FOLD(I+1))/FOUR
  101     continue
          FNEW(N) = (FOLD(N-1)+TWO*FOLD(N))/THREE
        end if
      end if
C     !END
C
      return
      end
