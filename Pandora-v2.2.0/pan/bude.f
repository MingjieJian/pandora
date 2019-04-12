      subroutine BUDE
     $(IS,IE,NDW,LABS,KOUNT)
C
C     Rudolf Loeser, 1982 Dec 23
C---- Sets up depth indices heading, for BOHUN.
C     !DASH
      save
C     !DASH
      integer I, IE, IS, KOUNT, NDW
      character LABS*15, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external HI, BYE
C
C               LABS(7)
      dimension LABS(*)
C
      call HI ('BUDE')
C     !BEG
      KOUNT = 0
      do 101 I = IS,IE
        KOUNT = KOUNT+1
C
        write (LABS(KOUNT),100) I
  100   format(I14,1X)
C
        if(I.eq.NDW) then
          LABS(KOUNT)(15:15) = STAR
        end if
C
  101 continue
C     !END
      call BYE ('BUDE')
C
      return
      end
