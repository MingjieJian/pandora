      subroutine KAKAPO
     $(I,WAVE,WTAB,LABEL,SAVE)
C
C     Rudolf Loeser, 1986 Feb 21
C---- Processes a wavelength label.
C     !DASH
      save
C     !DASH
      real*8 WAVE, WTAB
      integer I
      character BLANK*1, LABEL*29, SAVE*5
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  POMPANO, HI, BYE
      intrinsic mod
C
      call HI ('KAKAPO')
C     !BEG
      call POMPANO (WAVE,WTAB,LABEL)
      if(mod(I,5).eq.1) then
        SAVE = LABEL(25:29)
      else
        if(LABEL(25:29).eq.SAVE) then
          LABEL(25:29) = BLANK
        else
          SAVE = LABEL(25:29)
        end if
      end if
C     !END
      call BYE ('KAKAPO')
C
      return
      end
