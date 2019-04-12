      subroutine HAY
     $(NO,IU,IL)
C
C     Rudolf Loeser, 1982 May 05
C---- Prints an error message, for MAY.
C     !DASH
      save
C     !DASH
      integer IL, IU, NO
C     !DASH
      external HI, BYE
C
      call HI ('HAY')
C     !BEG
      if(IU.gt.0) then
        write (NO,100) IU,IL
  100   format(' ','Log(Tau',I2,',',I2,') is bad.')
      else
        write (NO,101)
  101   format(' ','Log(Tauk) is bad.')
      end if
C     !END
      call BYE ('HAY')
C
      return
      end
