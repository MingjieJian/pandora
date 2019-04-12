      subroutine POM
     $(LU,KE,LABEL,X,F,N)
C
C     Rudolf Loeser, 1985 Jul 01
C---- Starts dump printout, for "standard" second order integration.
C     (This is version 3 of POM.)
C     !DASH
      save
C     !DASH
      real*8 F, X
      integer KE, LU, N
      character LABEL*(*)
C     !DASH
      external VECOUT, HI, BYE
C
C               X(N), F(N)
      dimension X(*), F(*)
C
      call HI ('POM')
C     !BEG
      if(LU.gt.0) then
        write (LU,100) N,KE,LABEL
  100   format(' ','Details of Integration.',89X,I10,I5/
     $         ' ',A)
C
        call VECOUT (LU, X, N, 'Table of X values')
        call VECOUT (LU, F, N, 'Table of F values')
      end if
C     !END
      call BYE ('POM')
C
      return
      end
