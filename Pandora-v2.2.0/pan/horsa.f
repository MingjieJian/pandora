      subroutine HORSA
     $(N,MRR,WNSHL,WNDSK,PHISHL,PHIDSK,XOBL,XLTIT,WHSHL,WHDSK,ILFLX)
C
C     Rudolf Loeser, 1983 Feb 23
C---- Supervises matrix- and PHI-saving, for SAMOS.
C     (See also ABALLAC.)
C     !DASH
      save
C     !DASH
      real*8 PHIDSK, PHISHL, WHDSK, WHSHL, WNDSK, WNSHL, XLTIT, XOBL
      integer I, ILFLX, IOVER, IR, M, MRR, N, NSHL
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
      equivalence (LEST( 4),NSHL )
C     !DASH
      external TATAR, BALGIN, HI, BYE
C
C               WNSHL(N,N,NSHL), PHISHL(N,N,NSHL), WHSHL(N,N,NSHL),
      dimension WNSHL(N,N,*),    PHISHL(N,N,*),    WHSHL(N,N,*),
C
C               WNDSK(N,N,MRR ), PHIDSK(N,N,MRR ), WHDSK(N,N,MRR ),
     $          WNDSK(N,N,*),    PHIDSK(N,N,*),    WHDSK(N,N,*),
C
C               XOBL(Lodlen)
     $          XOBL(*)
C
      call HI ('HORSA')
C     !BEG
      if(IOVER.gt.0) then
        IR = 0
C
        I  = 0
        do 100 M = 1,NSHL
          IR = IR+1
          call TATAR  (I)
          call BALGIN (WNSHL(1,1,M),WHSHL(1,1,M),ILFLX,PHISHL(1,1,M),
     $                 IR,I,XOBL,XLTIT)
  100   continue
C
        do 101 M = 1,MRR
          IR = IR+1
          call BALGIN (WNDSK(1,1,M),WHDSK(1,1,M),ILFLX,PHIDSK(1,1,M),
     $                 IR,N,XOBL,XLTIT)
  101   continue
      end if
C     !END
      call BYE ('HORSA')
C
      return
      end
