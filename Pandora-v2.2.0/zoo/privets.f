      subroutine PRIVETS
     $(LU,V,N,LABEL)
C     Rudolf Loeser, 1982 Feb 04
C---- Special version of PRIVETL.
C     !DASH
      save
C     !DASH
      real*8 V
      integer LU, N
      character LABEL*(*), LEFTL*127
C     !DASH
      external  PRIVET, LINER
C
C               V(N)
      dimension V(*)
C
C     !BEG
      if(LU.gt.0) then
C
        LEFTL = LABEL
        LEFTL(88:127) = '----------------------------------------'
        call LINER  (1,LU)
        write (LU,100) LEFTL
  100   format(' ',A127)
C
        call PRIVET (LU,V,N)
C
      end if
C     !END
C
      return
      end
