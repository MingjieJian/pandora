      subroutine CHORE
     $(ITYPE,FACT,BULT,FUJ)
C
C     Rudolf Loeser, 2002 Nov 19
C---- Encodes a value of opacity multiplier, for BLURT.
C     (This is version 2 of CHORE.)
C     !DASH
      save
C     !DASH
      real*8 BULT, FACT
      integer ITYPE
      logical LINE, lummy
      character FUJ*7, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external PADDLE, SOWBANE, HI, BYE
C
      call HI ('CHORE')
C     !BEG
      call PADDLE    (ITYPE,LINE,lummy)
      if(LINE) then
        call SOWBANE (BULT,FUJ)
        FUJ(7:7) = STAR
      else
        call SOWBANE (FACT,FUJ)
      end if
C     !END
      call BYE ('CHORE')
C
      return
      end
