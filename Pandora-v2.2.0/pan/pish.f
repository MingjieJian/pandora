      subroutine PISH
     $(NAUX,QNAME,I1,I2)
C
C     Rudolf Loeser, 1982 Nov 29
C---- Reads ZAUX-table index, if present.
C     (This is version 2 of PISH.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer I1, I2, KERR, LUEO, MODE, NAUX, jummy
      character LPAREN*1, QNAME*8, QNEXT*8, qummy*8
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(49),LPAREN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external KIWI, MACE, MESHED, ABORT, CARMEN, CHLOE, HI, BYE
C     !EJECT
C
      call HI ('PISH')
C     !BEG
      KERR = 0
      call KIWI    (MODE, dummy, jummy, QNEXT, jummy)
      if((QNEXT.eq.'Z').or.(QNEXT.eq.'z'))then
        call KIWI  (MODE, dummy, NAUX, qummy, jummy)
        if(MODE.ne.3) goto 203
        call MACE
        goto 199
      end if
      if(QNEXT.ne.LPAREN) goto 206
      goto 199
C
C---- Errors
  206 KERR = KERR+1
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED  ('PISH', 1)
      write (LUEO,200) QNAME,QNEXT,NAUX,I1,I2,'Z','z',LPAREN
  200 format(' ','Error reading for ',2A12,3I10//
     $       ' ','List of expected fields:'//(' ',4X,10A10))
      call CHLOE   (LUEO, QNEXT, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('PISH')
C
      return
      end
