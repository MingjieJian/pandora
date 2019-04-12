      subroutine DON
C
C     Rudolf Loeser, 2002 Mar 18
C---- Reads storage management dump type.
C     (This is version 2 of DON.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer KERR, LUEO, MODE, jummy
      character DISK*4, QNAME*8, SCREEN*6
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- STORPO      as of 2005 Feb 03
      logical     WRLDHO, WRLDPR, WRLDTY
      common      /STORPO/ WRLDHO,WRLDPR,WRLDTY
C     Storage management debug printout control.
C     (See input parameter WORLDLY in Part B.)
C     .
C     !DASH
      external MACE, MICE, KIWI, MESHED, ABORT, CHLOE, CARMEN, UNMIX,
     $         HI, BYE
C
      data SCREEN,DISK /'SCREEN', 'DISK'/
C     !EJECT
C
      call HI ('DON')
C     !BEG
      KERR = 0
      call MACE
      call KIWI   (MODE, dummy, jummy, QNAME, jummy)
      if(MODE.ne.2) then
        goto 201
      end if
      call UNMIX  (QNAME)
C
      if(QNAME(:6).eq.SCREEN) then
        WRLDTY = .true.
      else if(QNAME(:4).eq.DISK) then
        WRLDPR = .true.
      else
        goto 202
      end if
      WRLDHO = WRLDTY.or.WRLDPR
C
      call MICE
      goto 199
C
C---- Error processing
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('DON', 1)
      write (LUEO,200) SCREEN,DISK
  200 format(' ','Trouble reading dump types. List of valid types:'//
     $      (' ',4X,10A10))
      call CHLOE  (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('DON')
C
      return
      end
