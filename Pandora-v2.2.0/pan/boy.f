      subroutine BOY
     $(MODE,ARR,LIMP,QNAME,LZA,ZAUX,Z,W)
C
C     Rudolf Loeser, 1981 Apr 28
C---- Reads data tables, for POMP.
C     (This is version 3 of BOY.)
C     !DASH
      save
C     !DASH
      real*8 ARR, W, Z, ZAUX
      integer LIMP, LZA, MODE
      character QNAME*8
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external CREAM, CARDAMN, HALT, HI, BYE
C
      dimension W(*)
C
C               ARR(N,LIMP), LZA(50), ZAUX(NZM,LZM), Z(N)
      dimension ARR(*),      LZA(*),  ZAUX(*),       Z(*)
C
      call HI ('BOY')
C     !BEG
      if(MODE.eq.1) then
        call CREAM   (ARR,QNAME,0,0,LZA,ZAUX,Z,W)
      else if(MODE.eq.2) then
        call CARDAMN (2,ARR,LIMP,QNAME,LZA,ZAUX,Z,W)
      else
        write (MSSLIN(1),100) MODE
  100   format('MODE =',I12,', which is neither 1 nor 2.')
        call HALT ('BOY',1)
      end if
C     !END
      call BYE ('BOY')
C
      return
      end
