      subroutine PIXY
     $(A,IS,IE,VAL,KNT)
C
C     Rudolf Loeser, 2000 Mar 07
C---- Edits values of iterative summary.
C     !DASH
      save
C     !DASH
      real*8 A, CRIT, VAL, VLIM, ZERO
      integer I, IE, IS, KNT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
      intrinsic abs
C
      dimension  A(*), VAL(*)
C
      data CRIT,VLIM /1.D5, 9999.99989D0/
C
      call HI ('PIXY')
C     !BEG
      KNT = 0
      do 100 I = IS,IE
        KNT = KNT+1
        if(abs(A(I)).ge.CRIT) then
          if(A(I).lt.ZERO) then
            VAL(KNT) = -VLIM
          else
            VAL(KNT) =  VLIM
          end if
        else
          VAL(KNT) = A(I)
        end if
  100 continue
C     !END
      call BYE ('PIXY')
C
      return
      end
