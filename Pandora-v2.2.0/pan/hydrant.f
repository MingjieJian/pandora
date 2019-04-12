      subroutine HYDRANT
     $(X,W,BDI)
C
C     Rudolf Loeser, 1980 Mar 11
C---- Drives QOUT calculation, for GORSE.
C     (This is version 2 of HYDRANT.)
C     !DASH
      save
C     !DASH
      real*8 BDI, W, X
      integer IESG, IN, IS, JJGM, JJPKS, JJQOU, JJSA, JJXNE, KSHEL, MO,
     $        MOX, N
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
      equivalence (IZOQ( 83),JJQOU)
      equivalence (IZOQ( 88),JJPKS)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(235),JJSA )
      equivalence (IZOQ( 16),JJGM )
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
      equivalence (LEST( 1),KSHEL)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external MALOSE, SALOME, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               BDI(N,NL)
      dimension BDI(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IESG  )
C     !EJECT
C
      call HI ('HYDRANT')
C     !BEG
      if((KSHEL.gt.0).and.(MO.gt.0)) then
C       (Get, and allocate, W allotment)
        call MALOSE (IN,IS,MOX,'HYDRANT')
C
        call SALOME (N,X(JJXNE),X(JJSA),X(JJGM),BDI,X(JJPKS),W(IESG),
     $               X(JJQOU),MO)
C
C       (Give back W allotment)
        call WGIVE  (W,'HYDRANT')
      end if
C     !END
      call BYE ('HYDRANT')
C
      return
      end
