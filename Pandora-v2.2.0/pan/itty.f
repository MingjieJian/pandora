      subroutine ITTY
     $(KODE,MODE)
C
C     Rudolf Loeser, 2002 Nov 05
C---- Checks control parameters, for editing.
C     !DASH
      save
C     !DASH
      integer KODE, LUEO, MODE
      logical KILROY
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MUSHED, ABORT, HI, BYE
C
      call HI ('ITTY')
C     !BEG
      KILROY = .true.
      if((KODE.lt.1).or.(KODE.gt.5)) then
        call MUSHED ('ITTY', 1, KILROY)
        write (LUEO,100) KODE
  100   format('KODE =',I12,', which is not 1, 2, 3, 4, or 5.')
      end if
      if((MODE.lt.1).or.(MODE.gt.2)) then
        call MUSHED ('ITTY', 1, KILROY)
        write (LUEO,101) MODE
  101   format('MODE =',I12,', which is not 1 or 2.')
      end if
      if(.not.KILROY) then
        call ABORT
      end if
C     !END
      call BYE ('ITTY')
C
      return
      end
