      subroutine IONNAME
     $(QELSM,IONST,QION)
C     Rudolf Loeser, 1986 May 09
C---- Encodes a compact form of ion name.
C     !DASH
      save
C     !DASH
      integer IONST
      character BLANK*1, H*2, QELSM*8, QION*8
C     !DASH
      data H,BLANK /'H ', ' '/
C
C     !BEG
      if(QELSM(1:2).eq.H) then
        QION = QELSM
      else if(QELSM(2:2).eq.BLANK) then
        if(IONST.lt.10) then
          write (QION,100) QELSM,IONST
  100     format(A1,I1,6X)
        else
          write (QION,101) QELSM,IONST
  101     format(A1,I2,5X)
        end if
      else
        if(IONST.lt.10) then
          write (QION,102) QELSM,IONST
  102     format(A2,I1,5X)
        else
          write (QION,103) QELSM,IONST
  103     format(A2,I2,4X)
        end if
      end if
C     !END
C
      return
      end
