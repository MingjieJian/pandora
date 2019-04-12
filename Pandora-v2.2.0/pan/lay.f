      subroutine LAY
     $(NO,IMAGE,IU,IL,KODE)
C
C     Rudolf Loeser, 1982 May 05
C---- Prints the TAU Scale, for MAY.
C     !DASH
      save
C     !DASH
      integer IL, IU, KODE, NO
      character IMAGE*(*), LINE*117
C     !DASH
      external KGIVE, HI, BYE
C
      call HI ('LAY')
C     !BEG
      call KGIVE (IMAGE,1,LINE)
      write (NO,100) LINE(11:117)
  100 format(' ',20X,A107)
C
      call KGIVE (IMAGE,2,LINE)
      if(KODE.eq.1) then
        write (NO,101) IU,IL,LINE(11:117)
  101   format(' ','Log(TAU  ',I2,',',I2,')',5X,A107)
      else if(KODE.eq.2) then
        write (NO,102) IU,IL,LINE(11:117)
  102   format(' ','Log(TAU-M',I2,',',I2,')',5X,A107)
      else
        write (NO,103) LINE(11:117)
  103   format(' ','Log(TAUK)',11X,A107)
      end if
C     !END
      call BYE ('LAY')
C
      return
      end
