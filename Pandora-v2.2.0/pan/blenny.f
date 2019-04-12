      subroutine BLENNY
     $(X)
C
C     Rudolf Loeser, 1986 Oct 02
C---- Final editing and printing of "Artificial TAU".
C     !DASH
      save
C     !DASH
      real*8 X
      integer JJTRA, JJZ, KTRAS
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(173),JJTRA)
      equivalence (IZOQ( 37),JJZ  )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(38),KTRAS)
C     !DASH
      external CARACAL, HI, BYE
C
      dimension X(*)
C
C
      call HI ('BLENNY')
C     !BEG
      if(KTRAS.gt.0) then
        call CARACAL (X(JJZ),X(JJTRA),KTRAS)
      end if
C     !END
      call BYE ('BLENNY')
C
      return
      end
