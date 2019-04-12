      subroutine ALBAN
     $(IBNDE,ITYPE)
C
C     Rudolf Loeser, 1993 Jun 01
C---- Sets up ITYPE, for HARRAN.
C     (This is version 2 of ALBAN.)
C     !DASH
      save
C     !DASH
      integer IBNDE, ITYPE
C     !DASH
      external HI, BYE
C
      call HI ('ALBAN')
C     !BEG
      if(IBNDE.le.0) then
C----   Composite line opacity wavelength, no eclipse
        ITYPE = 13
      else
C----   Composite line opacity wavelength, with eclipse
        ITYPE = 15
      end if
C     !END
      call BYE ('ALBAN')
C
      return
      end
