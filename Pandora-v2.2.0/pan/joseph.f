      subroutine JOSEPH
     $(NO)
C
C     Rudolf Loeser, 1996 Apr 16
C---- Standard output sections separator.
C     !DASH
      save
C     !DASH
      integer NO
      character STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external ABJECT, RULER, LINER, HI, BYE
C
      call HI ('JOSEPH')
C     !BEG
      if(NO.gt.0) then
        call LINER  (5,NO)
        call RULER  (NO,STAR,1000)
        call RULER  (NO,STAR,1000)
        call RULER  (NO,STAR,1000)
        call ABJECT (NO)
      end if
C     !END
      call BYE ('JOSEPH')
C
      return
      end
