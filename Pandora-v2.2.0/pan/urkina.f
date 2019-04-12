      subroutine URKINA
     $(KINOUT,I4DIO,INOUT)
C
C     Rudolf Loeser, 1999 Feb 18
C---- Selects which 4-diag version to use, for KURNAI.
C     !DASH
      save
C     !DASH
      integer I4DIO, INOUT, KINOUT
C     !DASH
      external HI, BYE
C
      call HI ('URKINA')
C     !BEG
      INOUT = KINOUT
      if(INOUT.eq.0) then
        INOUT = I4DIO
      end if
C     !END
      call BYE ('URKINA')
C
      return
      end
