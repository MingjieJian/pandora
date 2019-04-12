      subroutine DEGEN
     $(NO,KMULT,MULT,NAB,FABD)
C
C     Rudolf Loeser, 1983 Nov 14
C---- Prints multipliers information, for RUSAS.
C     !DASH
      save
C     !DASH
      real*8 FABD, ONE
      integer I, KLIN, KMULT, MULT, NAB, NID, NO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external FRAGAR, LINER, HI, BYE
C
C               MULT(NAB)
      dimension MULT(*)
C
      call HI ('DEGEN')
C     !BEG
      KLIN = 0
C
      if((NO.gt.0).and.(KMULT.gt.0)) then
        call FRAGAR  (MULT,NAB,NID)
        call LINER   (1,NO)
        write (NO,100)
  100   format(' ','Continuum Opacity Multipliers differing from 1 ',
     $             'affect the following bands:')
        write (NO,101)(MULT(I),I=1,NID)
  101   format(' ',10X,20I5)
        call LINER   (1,NO)
        KLIN = 1
      end if
C
      if(FABD.ne.ONE) then
        if(KLIN.eq.0) then
          call LINER (1,NO)
        end if
        write (NO,102) FABD
  102   format(' ','All Composite Line Opacity values will be ',
     $             'multiplied by FABD =',1PE12.4)
        call LINER   (1,NO)
      end if
C     !END
      call BYE ('DEGEN')
C
      return
      end
