      subroutine KONYA
     $(X,IX,NO,CTE,CNC)
C
C     Rudolf Loeser, 2006 Apr 14
C---- Compares Hydrogen CI values.
C     !DASH
      save
C     !DASH
      real*8 CI, CNC, CTE, X, ZERO
      integer I, IX, J, NL, NO
      logical DUMP, HYDR, MESS, lummy
      character VAL*14
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external LINER, SHIM, CHIGNON, DULLARD, DALBO, DOLBI, ROMATIN,
     $         MURRE, SINOP, HI, BYE
C
      dimension X(*), IX(*)
C
      dimension VAL(6), CI(6)
C
      data HYDR,DUMP,MESS /.true., .false., .false./
C
      call HI ('KONYA')
C     !BEG
      call LINER       (1, NO)
      write (NO,100)
  100 format(' ',5X,'level',6X,'this run',9X,'CLARK',7X,'JOHNSON',
     $           12X,'VS',12X,'AR',10X,'SHAH')
      call LINER       (1, NO)
C
      do 102 J = 1,NL
        call CHIGNON   (X, IX, J, CTE, CNC, DUMP, CI(1))
        call DULLARD   (X, IX, J, CTE, CI(2))
        call DALBO     (J, CTE, CNC, DUMP, CI(3))
        call DOLBI     (J, CTE, CNC, DUMP, CI(4))
        if(J.eq.1) then
          call MURRE   (CTE, DUMP, MESS, CI(5), lummy)
          call ROMATIN (CTE, CI(6))
        else
          CI(5) = ZERO
          CI(6) = ZERO
        end if
C
        call SINOP     (6, CI, VAL)
        write (NO,101) J,VAL
  101   format(' ',I10,6A14)
        call SHIM      (J, 5, NO)
  102 continue
C     !END
      call BYE ('KONYA')
C
      return
      end
