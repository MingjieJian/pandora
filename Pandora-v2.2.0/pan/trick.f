      subroutine TRICK
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Converts "PRD" into explanatory partial redistribution message.
C     (This is version 3 of TRICK.)
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, IPRD, KNT
      character BLANK*1, QAR*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HI, BYE
C
C               ARR(8), QAR(16)
      dimension ARR(*), QAR(*)
C
      call HI ('TRICK')
C     !BEG
      do 100 I = 1,KNT
        QAR(I) = BLANK
        IPRD = ARR(I)
C
        if(IPRD.eq.1) then
          QAR(I) = '  Complete'
        else if((IPRD.eq.2).or.(IPRD.eq.3)) then
          QAR(I) = '   Partial'
        else if(IPRD.ne.0) then
          QAR(I) = '      Huh?'
        end if
C
  100 continue
C     !END
      call BYE ('TRICK')
C
      return
      end
