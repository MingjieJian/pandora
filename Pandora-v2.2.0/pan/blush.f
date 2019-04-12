      subroutine BLUSH
     $(GET)
C
C     Rudolf Loeser, 1968 Nov 08
C---- Reads the next input field, and returns control only if
C     that field = GET as specified.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer LUEO, MODE, jummy
      character GET*(*), GOT*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external KIWI, MESHED, CHLOE, ABORT, CARMEN, HI, BYE
C
      call HI ('BLUSH')
C     !BEG
      call KIWI   (MODE, dummy, jummy, GOT, jummy)
      if((MODE.eq.2).and.(GOT.eq.GET)) goto 199
C
C---- Error
      call MESHED ('BLUSH', 1)
      write (LUEO,200) GET
  200 format(' ','Error in BLUSH.'//
     $       ' ','List of expected fields:'//(' ',5X,10A10))
      call CHLOE  (LUEO, GOT, 6)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('BLUSH')
C
      return
      end
