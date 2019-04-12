      subroutine HULU
     $(NL,LCX,LL,LIST)
C
C     Rudolf Loeser, 1990 Dec 03
C---- Sets up the list of "charge exchange levels".
C     !DASH
      save
C     !DASH
      integer J, LCX, LIST, LL, NL
C     !DASH
      external HI, BYE
C
C               LCX(NL), LIST(NL)
      dimension LCX(*),  LIST(*)
C
      call HI ('HULU')
C     !BEG
      LL = 0
      do 100 J = 1,NL
        if(LCX(J).gt.0) then
          LL = LL+1
          LIST(LL) = J
        end if
  100 continue
C     !END
      call BYE ('HULU')
C
      return
      end
