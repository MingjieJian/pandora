      subroutine FIZZ
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jul 23
C---- Drives ZIPPY, to do H.S.E.
C     (This is version 3 of FIZZ.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IN, IS, IW, IX, IXCBL, IXPBL, JJAEL, JJDGM, JJH1, JJH2N,
     $        JJHEA, JJHND, JJMSI, JJMSS, JJPEL, JJPEX, JJPGS, JJPMG,
     $        JJPRF, JJPTO, JJPTU, JJRZM, JJT5, JJTE, JJTKI, JJVM, JJVT,
     $        JJXNE, JJZ, JJZME, JJZRN, MOX, jummy
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(140),JJH2N)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 49),JJVT )
      equivalence (IZOQ( 52),JJRZM)
      equivalence (IZOQ( 80),JJAEL)
      equivalence (IZOQ( 48),JJZME)
      equivalence (IZOQ( 54),JJT5 )
      equivalence (IZOQ( 55),JJPEL)
      equivalence (IZOQ( 56),JJPGS)
      equivalence (IZOQ( 57),JJPTU)
      equivalence (IZOQ( 58),JJPTO)
      equivalence (IZOQ(119),JJMSS)
      equivalence (IZOQ( 33),JJMSI)
      equivalence (IZOQ( 75),JJTKI)
      equivalence (IZOQ(126),JJPRF)
      equivalence (IZOQ(181),JJVM )
      equivalence (IZOQ(205),JJPEX)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(236),JJZRN)
      equivalence (IZOQ(124),JJH1 )
      equivalence (IZOQ(151),JJDGM)
      equivalence (IZOQ(265),JJPMG)
C     !DASH
C     !EJECT
      external ICEMEN, POPIO, ZIPPY, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IXPBL ),(IN( 2),IXCBL )
C
      call HI ('FIZZ')
C     !BEG
C     (Get, and allocate, W allotment)
      call ICEMEN (IN, IS, MOX, 'FIZZ')
C     (Initialize populations buffer)
      call POPIO  ('INIT', jummy, W(IXPBL))
C
      call ZIPPY  (X, W, IW, X(JJHND), X(JJH2N), X(JJHEA), X(JJXNE),
     $             X(JJZ), X(JJTE), X(JJVT), X(JJVM), X(JJDGM),
     $             X(JJPMG), X(JJRZM), X(JJAEL), X(JJZME), X(JJT5),
     $             X(JJPEL), X(JJPGS), X(JJPTU), X(JJPEX), X(JJPTO),
     $             X(JJMSS), X(JJMSI), X(JJTKI), X(JJPRF), X(JJZRN),
     $             X(JJH1), W(IXPBL), W(IXCBL))
C
C     (Give back W allotment)
      call WGIVE  (W, 'FIZZ')
C     !END
      call BYE ('FIZZ')
C
      return
      end
