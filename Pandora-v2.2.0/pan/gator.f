      subroutine GATOR
     $(A,JU,JL,IB,IE,KIJ,ARR,IN)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Selects radiative transitions data
C     to be printed, for LIZARD.
C     (This is version 2 of GATOR.)
C     !DASH
      save
C     !DASH
      real*8 A, ARR
      integer I, IB, IE, IN, IT, JL, JU, KIJ
C     !DASH
      external ZERO1, INDXUL, HI, BYE
C
C               A(MUL), KIJ(MUL), JU(MUL), JL(MUL), ARR(8)
      dimension A(*),   KIJ(*),   JU(*),   JL(*),   ARR(*)
C
      call HI ('GATOR')
C     !BEG
      call ZERO1    (ARR,8)
      IN = 1
      do 100 I = IB,IE
        IN = IN+1
        call INDXUL (JU(I),JL(I),IT)
        if(KIJ(IT).eq.1) then
          ARR(IN) = A(IT)
        end if
  100 continue
C     !END
      call BYE ('GATOR')
C
      return
      end
