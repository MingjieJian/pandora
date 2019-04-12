      subroutine RIG
     $(NO,I,J,C,P,R,KNT)
C
C     Rudolf Loeser, 1985 Feb 08
C---- Does a pair of print lines, for PIG.
C     (This is version 2 of RIG.)
C     !DASH
      save
C     !DASH
      real*8 C, P, R
      integer I, IN, J, KLIN, KNT, NO
      character BLANK*1, LOWER*121, UPPER*121
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external ARRADD, WIG, LINER, HI, BYE
C
C               C(KNT), P(KNT), R(KNT)
      dimension C(*),   P(*),   R(*)
C
      call HI ('RIG')
C     !BEG
      call ARRADD (C,P,R,KNT)
C
      UPPER = BLANK
      LOWER = BLANK
C
      KLIN = 0
      do 100 IN = 1,KNT
        call WIG  (C(IN),P(IN),R(IN),UPPER((KLIN+1):(KLIN+11)),
     $                               LOWER((KLIN+1):(KLIN+11)))
        KLIN = KLIN+11
  100 continue
C
      call LINER (1,NO)
      write (NO,101) I,J,UPPER,LOWER
  101 format(' ',I2,'/',I2,2X,A121/
     $       ' ',7X,          A121)
C     !END
      call BYE ('RIG')
C
      return
      end
