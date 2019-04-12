      subroutine SINOP
     $(N,C,VAL)
C
C     Rudolf Loeser, 2006 Apr 14
C---- Encodes an array for printing.
C     !DASH
      save
C     !DASH
      real*8 C, ZERO
      integer I, N
      character BLANK*1, VAL*14
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
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
C               C(N), VAL(N)
      dimension C(*), VAL(*)
C
      call HI ('SINOP')
C     !BEG
      do 101 I = 1,N
        if(C(I).eq.ZERO) then
          VAL(I) = BLANK
        else
          write (VAL(I),100) C(I)
  100     format(1PE14.6)
        end if
  101 continue
C     !END
      call BYE ('SINOP')
C
      return
      end
