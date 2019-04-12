      subroutine PYRA
     $(SET,KNT, LINE)
C     Rudolf Loeser, 1998 Jul 20
C---- Encodes a line, for PROXY.
C     !DASH
      save
C     !DASH
      real*8 SET, ZERO
      integer I, K, KNT
      character AZRO*1, BLANK*1, LINE*117
C     !DASH
      dimension SET(9)
C
      data ZERO /0.D0/
      data BLANK,AZRO /' ', '0'/
C
C     !BEG
      LINE = BLANK
C
      K = -12
      do 101 I = 1,KNT
        K = K+13
        if(SET(I).eq.ZERO) then
          LINE(K+7:K+7) = AZRO
        else
          write (LINE(K:K+12),100) SET(I)
  100     format(1PE13.5)
        end if
  101 continue
C     !END
C
      return
      end
