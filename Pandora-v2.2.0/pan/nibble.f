      subroutine NIBBLE
     $(LAB,IND,IS,IE)
C
C     Rudolf Loeser, 1999 Jan 08
C---- Sets printout alerts.
C     !DASH
      save
C     !DASH
      integer I, IE, IND, IS, J
      character BLANK*1, LAB*1, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external HI, BYE
C
      dimension LAB(*)
C
      call HI ('NIBBLE')
C     !BEG
      if(IE.ge.IS) then
        J = 0
        do 100 I = IS,IE
          J = J+1
          if(IND.eq.I) then
            LAB(J) = STAR
          else
            LAB(J) = BLANK
          end if
  100   continue
      end if
C     !END
      call BYE ('NIBBLE')
C
      return
      end
