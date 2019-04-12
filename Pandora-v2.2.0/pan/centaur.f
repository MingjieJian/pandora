      subroutine CENTAUR
C
C     Rudolf Loeser, 1988 Apr 25
C---- Reads the kode and the filename of a "FILE" statement.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer KERR, LUEO, MODE, jummy
      character qummy*8
C     !COM
C---- SUKU        as of 1988 Apr 22
      integer     NAF
      parameter   (NAF=5)
      integer     KFLOPN,KODEGN
      character   FILNMS*8, FILSPEC*60
      dimension   KFLOPN(NAF),FILNMS(NAF)
      common      /SUKU1/ KFLOPN
      common      /SUKU2/ FILNMS
      common      /SUKU3/ KODEGN,FILSPEC
C     Names and in-use codes for the main and the auxiliary
C     input files.
C     1=INPUT,  2=MODEL,  3=ATOM,  4=RESTART,  5=GENERAL.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MACE, MICE, KIWI, MESHED, CHLOE, ABORT, CARMEN, HI, BYE
C     !EJECT
C
      call HI ('CENTAUR')
C     !BEG
      KERR = 0
      call MACE
      call KIWI  (MODE,dummy,KODEGN,qummy,jummy)
      if(MODE.ne.3) goto 203
      call KIWI  (MODE,dummy,jummy,FILSPEC,jummy)
      if(MODE.eq.4) goto 208
      if(MODE.ne.2) goto 205
      call MICE
      goto 199
C
C---- Error Processing
  208 KERR = KERR+1
  207 KERR = KERR+1
  206 KERR = KERR+1
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('CENTAUR', 1)
      write (LUEO,200)
  200 format(' ','Trouble reading "general" file name.')
      call CHLOE  (LUEO, 'FILE', KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('CENTAUR')
C
      return
      end
