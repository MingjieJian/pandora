      subroutine SIVAS
     $(X,IX,NO,CTE,CNC)
C
C     Rudolf Loeser, 2006 Apr 17
C---- Compares non-Hydrogen CI values.
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
      external LINER, SHIM, CHIGNON, DULLARD, POLAR, MURRE, SINOP,
     $         HI, BYE
C
      dimension X(*), IX(*)
C
      dimension VAL(4), CI(4)
C
      data HYDR,DUMP,MESS /.false., .false., .false./
C
      call HI ('SIVAS')
C     !BEG
      call LINER       (1, NO)
      write (NO,100)
  100 format(' ',5X,'level',6X,'this run',9X,'CLARK',12X,'AR',
     $           7X,'VORONOV')
      call LINER       (1, NO)
C
      do 102 J = 1,NL
        call CHIGNON   (X, IX, J, CTE, CNC, DUMP, CI(1))
        call DULLARD   (X, IX, J, CTE, CI(2))
        if(J.eq.1) then
          call MURRE   (CTE, DUMP, MESS, CI(3), lummy)
          call POLAR   (CTE, DUMP, MESS, CI(4), lummy)
        else
          CI(3) = ZERO
          CI(4) = ZERO
        end if
C
        call SINOP     (4, CI, VAL)
        write (NO,101) J,VAL
  101   format(' ',I10,4A14)
        call SHIM      (J, 5, NO)
  102 continue
C     !END
      call BYE ('SIVAS')
C
      return
      end
