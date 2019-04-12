      subroutine ABALLAC
     $(N,LG,WN,WH,ILFLX,PHI,XOBL,XLTIT)
C
C     Rudolf Loeser, 1983 Feb 18
C---- Supervises weight-matrix- and PHI-saving, for SALAMIS.
C     (See also HORSA.)
C     !DASH
      save
C     !DASH
      real*8 PHI, WH, WN, XLTIT, XOBL
      integer ILFLX, IOVER, LG, M, N
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
C     !DASH
      external BALGIN, HI, BYE
C
C               WN(N,N,LG), PHI(N,N,LG), WH(N,N,LG), XOBL(Lodlen)
      dimension WN(N,N,*),  PHI(N,N,*),  WH(N,N,*),  XOBL(*)
C
      call HI ('ABALLAC')
C     !BEG
      if(IOVER.gt.0) then
        do 100 M = 1,LG
          call BALGIN (WN(1,1,M),WH(1,1,M),ILFLX,PHI(1,1,M),M,N,XOBL,
     $                 XLTIT)
  100   continue
      end if
C     !END
      call BYE ('ABALLAC')
C
      return
      end
