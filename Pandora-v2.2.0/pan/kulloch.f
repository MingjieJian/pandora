      subroutine KULLOCH
     $(X,LEVEL,NLTE,DUMP,BDI,ABDION,XND,SUM,SO)
C
C     Rudolf Loeser, 1980 Aug 13
C---- Drives OLWEN to compute XND, the Number Density of level "LEVEL".
C     If NLTE=1, then nonLTE; if =0, then LTE.
C     !DASH
      save
C     !DASH
      real*8 ABDION, BDI, SO, SUM, X, XND
      integer JJGM, JJPF, JJSA, JJXNE, LEVEL, NL, NLTE
      logical DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(235),JJSA )
      equivalence (IZOQ(141),JJPF )
      equivalence (IZOQ( 16),JJGM )
C     !DASH
      external OLWEN, WOLEN, HI, BYE
C
      dimension X(*)
C
C               XND(N), ABDION(N), SUM(N), SO(N), BDI(N,NL)
      dimension XND(*), ABDION(*), SUM(*), SO(*), BDI(*)
C
      call HI ('KULLOCH')
C     !BEG
      call OLWEN   (LEVEL, NL, NLTE, ABDION, X(JJXNE), X(JJSA), BDI,
     $              X(JJGM), XND, SUM, SO)
      if(DUMP) then
        call WOLEN (NLTE, LEVEL, ABDION, BDI, X(JJGM), X(JJPF), SUM,
     $              X(JJSA), SO, XND)
      end if
C     !END
      call BYE ('KULLOCH')
C
      return
      end
