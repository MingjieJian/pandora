      subroutine RINEW
     $(A,REF,IS,IE,VAL,SIG,KNT)
C
C     Rudolf Loeser, 2001 Jun 27
C---- Sets up values to be printed by FONSECA.
C     (This is version 2 of RINEW.)
C     !DASH
      save
C     !DASH
      real*8 A, REF, VAL
      integer I, IE, IS, KNT
      character BLANK*1, SIG*1, STAR*1
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
C               A(N), REF(N), VAL(10), SIG(10)
      dimension A(*), REF(*), VAL(*),  SIG(*)
C
      call HI ('RINEW')
C     !BEG
      KNT = 0
      do 100 I = IS,IE
        KNT = KNT+1
        VAL(KNT) = A(I)
        if(A(I).eq.REF(I)) then
          SIG(KNT) = BLANK
        else
          SIG(KNT) = STAR
        end if
  100 continue
C     !END
      call BYE ('RINEW')
C
      return
      end
