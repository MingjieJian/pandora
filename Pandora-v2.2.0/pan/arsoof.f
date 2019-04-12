      subroutine ARSOOF
     $(LIJ,JU,JL,IB,IE,KIJ,ARR,IN)
C
C     Rudolf Loeser, 1996 Feb 21
C---- Selects data, for all transitions except THICK,
C     to be printed, for LIZARD.
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, IB, IE, IN, IT, JL, JU, KIJ, LIJ
C     !DASH
      external ZERO1, INDXUL, HI, BYE
C
C               LIJ(MUL), KIJ(MUL), JU(MUL), JL(MUL), ARR(8)
      dimension LIJ(*),   KIJ(*),   JU(*),   JL(*),   ARR(*)
C
      call HI ('ARSOOF')
C     !BEG
      call ZERO1    (ARR,8)
      IN = 1
      do 100 I = IB,IE
        IN = IN+1
        call INDXUL (JU(I),JL(I),IT)
        if((KIJ(IT).ge.1).and.(KIJ(IT).le.4)) then
          ARR(IN) = LIJ(IT)
        end if
  100 continue
C     !END
      call BYE ('ARSOOF')
C
      return
      end
