      subroutine PEARL
     $(X,W,XNE,ZME,ETA,ZTRM)
C
C     Rudolf Loeser, 1978 Aug 12
C     Revised RL/SGK Apr  9 2014 
C---- Controls computation of:
C     ZME  - metal-electron ratio, and
C     ETA  - degree of ionization of metals.
C     ZTRM - ZME component
C     !DASH
      save
C     !DASH
      real*8 ETA, W, X, XNE, ZME, ZTRM
      integer IN, IS, ISLVLS, IW1, IW2, IXPBL, JJCHI,
     $        JJPFT, JJTE, MOX, N, JJHND, JJRZM, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 52),JJRZM)
      equivalence (IZOQ(142),JJPFT)
      equivalence (IZOQ(143),JJCHI)
C     !DASH
      external MICA, GUINEA, POPIO, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               XNE(N), ZME(N), ETA(N,NMT), ZTRM(N,NMT)
      dimension XNE(*), ZME(*), ETA(*),     ZTRM(N,*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IXPBL ),(IN( 2),IW1   ),(IN( 3),IW2   )
C     !EJECT
C
      call HI ('PEARL')
C     !BEG
C     (Get, and allocate, W allotment)
      call MICA   (IN,IS,MOX,'PEARL')
C     (Initialize populations buffer)
      call POPIO  ('INIT',jummy,W(IXPBL))
C
      call GUINEA (X,XNE,X(JJTE),X(JJHND),X(JJRZM),
     $             X(JJPFT),X(JJCHI),W(IXPBL),
     $             W(IW1),W(IW2),N,ZME,ETA,ZTRM)
C
C     (Give back W allotment)
      call WGIVE  (W,'PEARL')
C     !END
      call BYE ('PEARL')
C
      return
      end
