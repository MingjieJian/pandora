      subroutine TARAGON
C
C     Rudolf Loeser, 1968 Nov 08
C---- Reads the RHO-switch.
C     (This is version 2 of TARAGON.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer JRHO, KERR, KIND, LOOK, LUEO, MODE, NKY, jummy
      character KEY*8, QRHO*8
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
      equivalence (KZQ( 12),JRHO )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MICE, MACE, LOOKUC, MESHED, ABORT, UNMIX, CHLOE, CARMEN,
     $         KIWI, HI, BYE
C
      dimension KEY(4)
C
      data NKY /4/
      data KEY /'RHOS', 'RHOJ', 'RHOW', 'RHOP'/
C     !EJECT
C
      call HI ('TARAGON')
C     !BEG
      KERR = 0
      call MACE
      call KIWI   (MODE, dummy, jummy, QRHO, jummy)
      if(MODE.ne.2) then
        goto 205
      end if
      call UNMIX  (QRHO)
      call LOOKUC (KEY, NKY, QRHO, KIND, LOOK)
      if(LOOK.ne.1) then
        goto 202
      end if
      if(KIND.eq.4) then
C       (RHOP is a former name for RHOS)
        KIND = 1
      end if
      JRHO = KIND-1
      call MICE
      goto 199
C
C---- Error processing
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('TARAGON', 1)
      write (LUEO,200) KEY
  200 format(' ','Error reading RHO options. List of valid options:'//
     $      (' ',5X,10A10))
      call CHLOE  (LUEO, QRHO, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('TARAGON')
C
      return
      end
