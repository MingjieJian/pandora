      subroutine MIMBLE
     $(XLM,LABEL)
C
C     Rudolf Loeser, 2002 Oct 25
C---- Makes a label for CSF matrix inversion.
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer IOVER, ITER
      character LABEL*80
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 3),ITER )
C     !DASH
      external HI, BYE
C
      call HI ('MIMBLE')
C     !BEG
      write (LABEL,100) XLM,ITER,IOVER
  100 format('Script-M matrix for CSF, lambda =',1PE20.12,' (SUB',I3,
     $       ', OVR',I3,')')
C     !END
      call BYE ('MIMBLE')
C
      return
      end
