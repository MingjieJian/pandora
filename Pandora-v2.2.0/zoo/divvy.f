      subroutine DIVVY
     $(A,B,RATIO)
C
C     Rudolf Loeser, 2004 Apr 27
C---- Checks and computes A/B.
C
C     (Stripped version of DIVIDE (q.v.))
C
C     !DASH
      save
C     !DASH
      real*8 A, B, RATIO, SMALL, ZERO
C     !DASH
      external  ABORT
      intrinsic abs
C
      data ZERO,SMALL /0.D0, 1.D-307/
C
C     !BEG
      if(abs(B).gt.SMALL) then
        RATIO = A/B
      else
        if(A.ne.ZERO) then
          write(*,100) A,B
  100     format('DIVVY:  A/B = ? because A =',1PE16.8,' and B =',E10.2)
          call ABORT
        else
          RATIO = ZERO
          write(*,101) A,B
  101     format('DIVVY:  A/B = 0,  with A =',E10.2,' and B =',E10.2)
        end if
      end if
C     !END
C
      return
      end
