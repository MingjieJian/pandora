      subroutine CHIRON
C
C     Rudolf Loeser, 1974 Nov 22
C---- Reads input file specification.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer KERR, KIND, LOOK, LUEO, MODE, jummy
      character QNAME*8
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
      external MACE, UNMIX, KIWI, LOOKUC, MESHED, ABORT, CHLOE, CARMEN,
     $         MICE, LID, HI, BYE
C     !EJECT
C
      call HI ('CHIRON')
C     !BEG
      KERR = 0
      call MACE
      call KIWI   (MODE, dummy, jummy, QNAME, jummy)
      if(MODE.ne.2) then
        goto 201
      end if
C
      call UNMIX  (QNAME)
      call LOOKUC (FILNMS, NAF, QNAME, KIND, LOOK)
      if(LOOK.ne.1) then
        goto 202
      end if
C
      call MICE
      call LID    (QNAME, KFLOPN(KIND))
      goto 199
C
C---- Error processing
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('CHIRON', 1)
      write (LUEO,200) FILNMS
  200 format(' ','Trouble reading data file names. List of valid file ',
     $           'names:'//
     $      (' ',4X,10A10))
      call CHLOE  (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('CHIRON')
C
      return
      end
