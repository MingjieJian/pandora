      subroutine GARLIC
     $(I,TAB,NT,SYM)
C
C     Rudolf Loeser, 1990 Oct 15
C---- Gets a plot character from a table.
C     (This is version 2 of GARLIC.)
C     !DASH
      save
C     !DASH
      integer I, J, NT
      character STAR*1, SYM*1, TAB*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external  HI, BYE
      intrinsic mod
C
C               TAB(NT)
      dimension TAB(*)
C
      call HI ('GARLIC')
C     !BEG
      if(NT.le.0) then
        SYM = STAR
      else
        J   = mod((I-1),NT)+1
        SYM = TAB(J)
      end if
C     !END
      call BYE ('GARLIC')
C
      return
      end
