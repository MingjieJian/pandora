      subroutine EBONY
     $(QNAME,LZA)
C
C     Rudolf Loeser, 1981 Apr 28
C---- Reads auxiliary Z-table length, for FENNEL.
C     (This is version 4 of EBONY.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer KERR, LUEO, LZA, MODE, NAUX, jummy
      character QNAME*8, qummy*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external KIWI, WITCH, MESHED, CHLOE, ABORT, CARMEN, HI, BYE
C
C               LZA(50)
      dimension LZA(*)
C
      call HI ('EBONY')
C     !BEG
      KERR = 0
      call KIWI   (MODE, dummy, NAUX, qummy, jummy)
      if(MODE.ne.3) then
        goto 203
      end if
      call WITCH  (QNAME, LZA(NAUX))
      goto 100
C
C---- Error message and exit
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('EBONY', 1)
      write (LUEO,200)
  200 format(' ','Error in EBONY: reading ZAUX-table length.')
      call CHLOE  (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
C
C---- Normal exit
  100 continue
C     !END
      call BYE ('EBONY')
C
      return
      end
