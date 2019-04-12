      subroutine BRISTOL
     $(Y,KASE)
C
C     Rudolf Loeser, 2003 May 09
C---- Computes KASE, the weight matrix method selector.
C     KASE = 1 means: QR-method, direct;
C            2      : QR-method, mapped;
C            3      : RT-method, and
C            4      : GR-method.
C     (This is version 2 of BRISTOL.)
C     !DASH
      save
C     !DASH
      real*8 ONE, THREE, TWO, Y, ZERO
      integer KASE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('BRISTOL')
C     !BEG
      if(Y.eq.-THREE) then
        KASE = 4
      else if(Y.eq.-ONE) then
        KASE = 3
      else if(Y.eq.-TWO) then
        KASE = 2
      else if((Y.ge.ZERO).and.(Y.le.ONE)) then
        KASE = 1
      else
        write (MSSLIN(1),100) Y
  100   format('Y =',1PE24.16,' is invalid.')
        call HALT ('BRISTOL', 1)
      end if
C     !END
      call BYE ('BRISTOL')
C
      return
      end
