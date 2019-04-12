      subroutine INDARRD
     $(X,INC,IBEG,IEND)
C     Rudolf Loeser, 1998 Oct 20
C---- Sets up an array of consecutive integers.
C     !DASH
      save
C     !DASH
      real*8 X
      integer I, IBEG, IEND, INC, J
C     !DASH
      dimension X(*)
C
C     !BEG
      if(IEND.ge.IBEG) then
        J = IBEG-INC
        do 100 I = IBEG,IEND
          J = J+INC
          X(J) = I
  100   continue
      end if
C     !END
C
      return
      end
