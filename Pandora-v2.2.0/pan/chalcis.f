      subroutine CHALCIS
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 2004 May 24
C---- Controls calculation of SET(i,u,l) = stimulated emission term.
C     (This is version 2 of CHALCIS.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, JJBAT, JJBDI, JJSET, N, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(253),JJSET)
      equivalence (IZOQ( 39),JJBAT)
C     !DASH
      external ZANCLE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('CHALCIS')
C     !BEG
      call ZANCLE (N, NL, X(JJBAT), X(JJBDI), X(JJSET))
C     !END
      call BYE ('CHALCIS')
C
      return
      end
