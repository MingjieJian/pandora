      subroutine CARDAMN
     $(ISI,ARR,LIMP,QNAME,LZA,ZAUX,Z,W)
C
C     Rudolf Loeser, 1969 Aug 06
C---- Reads population data or departure coefficients.
C     !DASH
      save
C     !DASH
      real*8 ARR, W, Z, ZAUX
      integer ISI, J, LIMP, LZA
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MINT, HALT, CREAM, HI, BYE
C
      dimension W(*)
C
C               ARR(N,LIMP), LZA(50), ZAUX(NZM,LZM), Z(N)
      dimension ARR(N,*),    LZA(*),  ZAUX(*),       Z(*)
C
      call HI ('CARDAMN')
C     !BEG
      if(ISI.eq.1) then
        J = 1
      else if(ISI.eq.2) then
        call MINT (QNAME,J)
      else
        write (MSSLIN(1),100) ISI
  100   format('ISI =',I12,', which is neither 1 nor 2.')
        call HALT ('CARDAMN',1)
      end if
C
      call CREAM  (ARR(1,J),QNAME,J,0,LZA,ZAUX,Z,W)
C     !END
      call BYE ('CARDAMN')
C
      return
      end
