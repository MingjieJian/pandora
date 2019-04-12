      subroutine CLOVIS
     $(TI,E2T,E3T,E4T)
C
C     Rudolf Loeser, 1981 Apr 13
C---- Computes functions for weight matrix calculations.
C     (This is version 3 of CLOVIS.)
C     !DASH
      save
C     !DASH
      real*8 E2T, E3T, E4T, TI, dummy
C     !COM
C---- TIGARA      as of 1988 Feb 17
      real*8      TIGTIS,TIGES2,TIGES3,TIGES4
      common      /TIGARA/ TIGTIS,TIGES2,TIGES3,TIGES4
C     Intermediates for subroutine CLOVIS.
C     .
C     !DASH
      external EXPINT, HI, BYE
C
      call HI ('CLOVIS')
C     !BEG
      if(TIGTIS.ne.TI) then
        TIGTIS = TI
        call EXPINT (2,TIGTIS,TIGES2,dummy)
        call EXPINT (3,TIGTIS,TIGES3,dummy)
        call EXPINT (4,TIGTIS,TIGES4,dummy)
      end if
C
      E2T = TIGES2
      E3T = TIGES3
      E4T = TIGES4
C     !END
      call BYE ('CLOVIS')
C
      return
      end
