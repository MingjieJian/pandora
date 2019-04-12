      subroutine PODGE
     $(LU,MM,N,LEN)
C
C     Rudolf Loeser, 1998 Apr 16
C---- Prints a message, if needed, for diffusion.
C     (This is version 3 of PODGE.)
C     !DASH
      save
C     !DASH
      integer LEN, LU, MM, N
      character BLANK*1, LINE*127
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
      call HI ('PODGE')
C     !BEG
      if((LU.gt.0).and.(MM.lt.N)) then
        LINE = BLANK
        LINE(:LEN) = LINE(:(LEN-10))//'(rest = 0)'
C
        write (LU,100) LINE(:LEN)
  100   format(' ',A)
      end if
C     !END
      call BYE ('PODGE')
C
      return
      end
