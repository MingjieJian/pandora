      subroutine EKASHI
     $(KODE)
C
C     Rudolf Loeser, 1987 Nov 23
C---- Reads output files code.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer KERR, KODE, LUEO, MODE, jummy
      character QNAME*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MICE, MACE, KIWI, MESHED, CHLOE, ABORT, UNMIX, CARMEN,
     $         HI, BYE
C     !EJECT
C
      call HI ('EKASHI')
C     !BEG
      KERR = 0
      call MACE
      call KIWI   (MODE, dummy, jummy, QNAME, jummy)
      if(MODE.ne.2) then
        goto 201
      end if
      call MICE
C
      call UNMIX  (QNAME)
      if(QNAME(1:5).eq.'MERGE') then
        KODE = 0
        goto 199
      else if(QNAME(1:5).eq.'SPLIT') then
        KODE = 1
        goto 199
      end if
C
C---- Error processing
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('EKASHI', 1)
      write (LUEO,200)
  200 format(' ','Trouble reading output file code, which must be ',
     $           'SPLIT or MERGE.')
      call CHLOE  (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('EKASHI')
C
      return
      end
