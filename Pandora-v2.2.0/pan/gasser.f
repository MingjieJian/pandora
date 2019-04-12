      subroutine GASSER
     $(X,N,DEN,PGS,W)
C
C     Rudolf Loeser, 1991 Jan 09
C---- Computes gas density, DEN, and gas pressure, PGS.
C     !DASH
      save
C     !DASH
      real*8 DEN, PGS, W, X
      integer IHELA, IN, IPEL, IS, JJH2N, JJHND, JJTE, JJXNE, MOX, N
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(140),JJH2N)
C     !DASH
      external NETAI, GUSHER, POLYP, GASP, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               DEN(N), PGS(N)
      dimension DEN(*), PGS(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IPEL  ),(IN( 2),IHELA )
C
      call HI ('GASSER')
C     !BEG
C     (Get, and allocate, W allotment)
      call NETAI  (IN, IS, MOX, 'GASSER')
C
      call GUSHER (X, N, DEN, W(IHELA))
      call POLYP  (X(JJTE), X(JJXNE), W(IPEL), N)
      call GASP   (X(JJTE), X(JJHND), W(IHELA), X(JJH2N), W(IPEL),
     $             PGS, N)
C
C     (Give back W allotment)
      call WGIVE  (W, 'GASSER')
C     !END
      call BYE ('GASSER')
C
      return
      end
