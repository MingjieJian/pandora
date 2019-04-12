      subroutine DORIS
     $(TI,E2T,E3T,E4T)
C
C     Rudolf Loeser, 1981 Apr 13
C---- Computes functions for weight matrix calculations.
C     (This is version 3 of DORIS.)
C     !DASH
      save
C     !DASH
      real*8 E2T, E3T, E4T, TI
C     !COM
C---- SIRIT       as of 1988 Apr 22
      real*8      DORTIS,DORE2S,DORE3S,DORE4S
      common      /SIRIT/ DORTIS,DORE2S,DORE3S,DORE4S
C     Intermediates for subroutine DORIS.
C     .
C     !DASH
      external MARDUK, HI, BYE
C
      call HI ('DORIS')
C     !BEG
      if(DORTIS.ne.TI) then
        DORTIS = TI
        call MARDUK (DORTIS,2,DORE2S)
        call MARDUK (DORTIS,3,DORE3S)
        call MARDUK (DORTIS,4,DORE4S)
      end if
C
      E2T = DORE2S
      E3T = DORE3S
      E4T = DORE4S
C     !END
      call BYE ('DORIS')
C
      return
      end
