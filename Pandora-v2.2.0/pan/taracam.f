      subroutine TARACAM
C
C     Rudolf Loeser, 1974 Dec 13
C---- Reads the BD-switch.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer JBD, KERR, KIND, LOOK, LUEO, MODE, NKY, jummy
      character KEY*8, QBD*8
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 10),JBD  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MACE, MICE, LOOKUC, MESHED, ABORT, CHLOE, UNMIX, CARMEN,
     $         KIWI, HI, BYE
C
      dimension KEY(3)
C
      data NKY /3/
      data KEY /'BDJ', 'BDR', 'BDQ'/
C     !EJECT
C
      call HI ('TARACAM')
C     !BEG
      KERR = 0
      call MACE
      call KIWI   (MODE, dummy, jummy, QBD, jummy)
      if(MODE.ne.2) then
        goto 205
      end if
C
      call UNMIX  (QBD)
      call LOOKUC (KEY, NKY, QBD, KIND, LOOK)
      if(LOOK.ne.1) then
        goto 202
      end if
C
      JBD = KIND-1
C
      call MICE
      goto 199
C
C---- Error processing
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('TARACAM', 1)
      write (LUEO,200) KEY
  200 format(' ','Trouble reading BD options. List of valid options:'//
     $      (' ',5X,10A10))
      call CHLOE  (LUEO, QBD, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('TARACAM')
C
      return
      end
