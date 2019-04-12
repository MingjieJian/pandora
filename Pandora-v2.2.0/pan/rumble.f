      subroutine RUMBLE
     $(LABEL,DL,JUNK,TNU,N)
C
C     Rudolf Loeser, 1983 Dec 01
C---- Dumps, for GREEN.
C     !DASH
      save
C     !DASH
      real*8 DL, TNU
      integer JUNK, LUEO, N
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, PRIVET, ABORT, HI, BYE
C
C               TNU(N)
      dimension TNU(*)
C
      call HI ('RUMBLE')
C     !BEG
      call MESHED ('RUMBLE',1)
      write (LUEO,100) LABEL
  100 format(' ',A)
      call LINER  (1,LUEO)
      write (LUEO,101) DL,JUNK
  101 format(' ','Trouble computing Line Intensity and Flux profiles.'/
     $       ' ','TNU for DL =',F15.7,' is not monotonic at',I3,
     $           '. point')
      call PRIVET (LUEO,TNU,N)
      call ABORT
C     !END
      call BYE ('RUMBLE')
C
      return
      end
