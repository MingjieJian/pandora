      subroutine CLICK
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Converts biased numerical value of Damping Parameter into
C     alphabetic source function method description, for LIZARD.
C     (This is version 2 of CLICK.)
C     !DASH
      save
C     !DASH
      real*8 ARR, TEN, Y, ZERO
      integer I, KNT
      character BLANK*1, QAR*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(11),TEN   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HAKO, HI, BYE
C
C               ARR(8), QAR(16)
      dimension ARR(*), QAR(*)
C
      call HI ('CLICK')
C     !BEG
      do 100 I = 1,KNT
        QAR(I) = BLANK
C
        if(ARR(I).gt.ZERO) then
          Y = ARR(I)-TEN
          call HAKO (Y,QAR(I))
        end if
  100 continue
C     !END
      call BYE ('CLICK')
C
      return
      end
