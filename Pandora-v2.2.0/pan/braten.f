      subroutine BRATEN
     $(A,N,NO)
C
C     Rudolf Loeser, 2000 Nov 03
C---- Prints values to be restricted to a range.
C     !DASH
      save
C     !DASH
      real*8 A
      integer IE, IS, KNT, N, NO
      character LINE*102
C     !DASH
      external  LINER, BANTER, HI, BYE
      intrinsic min, mod
C
C               A(N)
      dimension A(*)
C
      call HI ('BRATEN')
C     !BEG
      IE = 0
  100 continue
        IS =  IE+1
        IE =  min((IE+10),N)
C
        KNT = IE-IS+1
        call BANTER  (A(IS), KNT, LINE)
C
        write (NO,101) IS,IE,LINE
  101   format(' ',5X,'depths',I4,'-',I4,5X,A102)
C
        if((mod(IE,50).eq.0).and.(IE.lt.N)) then
          call LINER (1, NO)
        end if
C
      if(IE.lt.N) goto 100
C     !END
      call BYE ('BRATEN')
C
      return
      end
