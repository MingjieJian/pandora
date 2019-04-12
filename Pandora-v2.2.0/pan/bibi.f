      subroutine BIBI
     $(X,W)
C
C     Rudolf Loeser, 1980 Sep 03
C---- Retrieves passive JBARs from Continuum Data Blocks.
C     (This is version 2 of BIBI.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IN, IS, IXCBL, JJXNU, JJYBC, MOX
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 81),JJYBC)
C     !DASH
      external LOUT, WHOM, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IXCBL )
C
      call HI ('BIBI')
C     !BEG
C     (Get W allotment)
      call LOUT  (IN,IS,MOX,'BIBI')
C
      call WHOM  (W(IXCBL),X(JJXNU),X(JJYBC))
C
C     (Give back W allotment)
      call WGIVE (W,'BIBI')
C     !END
      call BYE ('BIBI')
C
      return
      end
