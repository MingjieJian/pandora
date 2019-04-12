      subroutine MOYBBA
     $(F,N,DOLOG,FF)
C
C     Rudolf Loeser, 1998 Oct 16
C---- Sets up the table to be smoothed, for SMOOTH.
C     !DASH
      save
C     !DASH
      real*8 F, FF
      integer I, N
      logical DOLOG
C     !DASH
      external  MOVE1, HI, BYE
      intrinsic abs
C
C               F(N), FF(N)
      dimension F(*), FF(*)
C
      call HI ('MOYBBA')
C     !BEG
      if(DOLOG) then
        do 100 I = 1,N
          FF(I) = log(abs(F(I)))
  100   continue
      else
        call MOVE1 (F, N, FF)
      end if
C     !END
      call BYE ('MOYBBA')
C
      return
      end
