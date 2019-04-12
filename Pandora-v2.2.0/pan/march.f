      subroutine MARCH
     $(IU,IL,INDEX,CALLER,INDX)
C
C     Rudolf Loeser, 1984 Jan 16
C---- Computes an index value.
C
C     INDEX =  IJ  or UL  or NT   is meaningful.
C     !DASH
      save
C     !DASH
      integer IL, INDX, IU
      character CALLER*(*), INDEX*2
C     !DASH
      external  INDXIJ, INTRANS, INDXUL, HI, BYE
C
      call HI ('MARCH')
C     !BEG
      if(INDEX.eq.'IJ') then
        call INDXIJ  (IU, IL, INDX)
C
      else if(INDEX.eq.'NT') then
        call INTRANS (IU, IL, 'MARCH from '//CALLER, INDX)
C
      else if(INDEX.eq.'UL') then
        call INDXUL  (IU, IL, INDX)
C
      end if
C     !END
      call BYE ('MARCH')
C
      return
      end
