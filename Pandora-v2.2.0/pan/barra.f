      subroutine BARRA
     $(NO,IMAGE,TIT,IU,IL)
C
C     Rudolf Loeser, 1996 Mar 06
C---- Prints the plot, for SKYE.
C     !DASH
      save
C     !DASH
      integer IL, IU, NO
      character IMAGE*(*), TIT*10
C     !DASH
      external ABJECT, LINER, KPRINT, HI, BYE
C
      call HI ('BARRA')
C     !BEG
      call ABJECT (NO)
      write (NO,100) TIT, IU,IL
  100 format(' ','Scattering albedo analysis, vs. ',A10,'; for line ',
     $           '(',I2,'/',I2,')',10X,'(Option LINECOMP)')
      call LINER  (1,NO)
      call KPRINT (IMAGE,NO)
C     !END
      call BYE ('BARRA')
C
      return
      end
