      subroutine MITHRAS
     $(X,W,IW,TAUSHL,TAUDSK,Y,MOVING,WN,WH,ILFLX,TITLE)
C
C     Rudolf Loeser, 1981 Nov 02
C---- Computes weight matrices, WN and WH, from TAU arrays,
C     using rays traced in spherical coordinates.
C     Returns WN = "Lambda-minus-one" operator, and
C             WH = "Phi" operator".
C     (This is version 2 of MITHRAS.)
C     !DASH
      save
C     !DASH
      real*8 TAUDSK, TAUSHL, W, WH, WN, X, Y
      integer ILFLX, IN, IS, IW, IWHD, IWHS, IWND, IWNS, JJCDK, JJCSH,
     $        JJKSR, JJWDK, JJWSH, MOX, MRR, N, NRPMX, NSHL
      logical MOVING
      character TITLE*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(109),JJCSH)
      equivalence (IZOQ(134),JJCDK)
      equivalence (IZOQ(169),JJWSH)
      equivalence (IZOQ(168),JJWDK)
      equivalence (IZOQ(153),JJKSR)
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
      equivalence (LEST( 8),NRPMX)
C     !DASH
C     !EJECT
      external INDRA, HELIOS, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TAUSHL(NRPMX,NSHL), TAUDSK(N,MRR), WN(N,N), WH(N,N)
      dimension TAUSHL(*),          TAUDSK(*),     WN(*),   WH(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IWNS  ),(IN( 2),IWND  ),(IN( 3),IWHS  ),(IN( 4),IWHD  )
C
      call HI ('MITHRAS')
C     !BEG
C     (Get, and allocate, W allotment)
      call INDRA  (IN, IS, MOX, 'MITHRAS')
C
      call HELIOS (N, NSHL, NRPMX, MRR, Y, MOVING, WN, WH, ILFLX,
     $             TAUSHL, X(JJCSH), TAUDSK, X(JJCDK), W(IWNS),
     $             W(IWND), W(IWHS), W(IWHD), X(JJKSR), X(JJWSH),
     $             X(JJWDK), W, IW, TITLE)
C
C     (Give back W allotment)
      call WGIVE  (W, 'MITHRAS')
C     !END
      call BYE ('MITHRAS')
C
      return
      end
