      subroutine TOUR
     $(CSHL,WSHL,CDSK,WDSK,LFLX)
C
C     Rudolf Loeser, 1988 Apr 01
C---- Saves debug checksums, for NUBA.
C     !DASH
      save
C     !DASH
      real*8 CDSK, CSHL, WDSK, WSHL
      integer LFLX, MRR, N, ND, NS, NSHL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
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
      equivalence (LEST( 4),NSHL )
C     !DASH
      external CHECKER, HI, BYE
C
C               CSHL(N,NSHL), WSHL(N,NSHL), CDSK(N,MRR), WDSK(N,MRR)
      dimension CSHL(*),      WSHL(*),      CDSK(*),     WDSK(*)
C
      call HI ('TOUR')
C     !BEG
      NS = N*NSHL
      ND = N*MRR
C
      call CHECKER   (CSHL, 1, NS, 'Shell weights (WN)')
      call CHECKER   (CDSK, 1, ND, 'Disk  weights (WN)')
      if(LFLX.gt.0) then
        call CHECKER (WSHL, 1, NS, 'Shell weights (WH)')
        call CHECKER (WDSK, 1, ND, 'Disk  weights (WH)')
      end if
C     !END
      call BYE ('TOUR')
C
      return
      end
