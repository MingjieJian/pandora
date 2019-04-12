      subroutine AMURA
     $(I,INDX)
C
C     Rudolf Loeser, 2002 Dec 17
C---- Encodes an index for RAUMA.
C     !DASH
      save
C     !DASH
      integer I
      character INDX*12
C     !DASH
      intrinsic mod
C
C     !BEG
      if(I.gt.999999) then
        write (INDX,100) I
  100   format(I12)
      else
        if(mod(I,5).eq.1) then
          write (INDX,101) I
  101     format('row = ',I6)
        else
          write (INDX,100) I
        end if
      end if
C     !END
C
      return
      end
