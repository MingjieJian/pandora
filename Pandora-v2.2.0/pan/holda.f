      subroutine HOLDA
     $(X,W,IW,TAU,Z,JJ,Y,MOVING,WN,WH,ILFLX,TITLE)
C
C     Rudolf Loeser, 1981 May 08
C---- Sets up GR-weight matrices,
C     for Continuum Source Function calculation.
C     !DASH
      save
C     !DASH
      real*8 TAU, W, WH, WN, X, Y, Z, dummy
      integer ILFLX, IN, IS, ITMU, IW, JJ, JJXMU, KODE, LG, MOX
      logical DUMP, MOVING
      character TITLE*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(34),LG )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(130),JJXMU)
C     !DASH
      external KARUK, TANG, KENT, CRONOS, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TAU(N), Z(N), WN(N,N), WH(N,N)
      dimension TAU(*), Z(*), WN(*),   WH(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),ITMU  )
C
      data KODE,DUMP /9999, .false./
C
      call HI ('HOLDA')
C     !BEG
C     (Get W allotment)
      call KARUK  (IN, IS, MOX, 'HOLDA')
C
      call TANG   (X(JJXMU), LG, TAU, JJ, W(ITMU))
      call KENT   (W(ITMU), JJ, LG, TITLE, KODE, dummy, dummy, DUMP)
      call CRONOS (X, W, IW, W(ITMU), Z, JJ, Y, MOVING, WN, WH, ILFLX,
     $             TITLE)
C
C     (Give back W allotment)
      call WGIVE  (W, 'HOLDA')
C     !END
      call BYE ('HOLDA')
C
      return
      end
