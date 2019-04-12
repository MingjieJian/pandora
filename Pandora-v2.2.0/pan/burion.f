      subroutine BURION
     $(X,JS,JE,ESG)
C
C     Rudolf Loeser, 2001 Dec 11
C---- Computes ESG = NE * SA * GM, for levels JE through JS.
C     (This is version 2 of BURION.)
C     !DASH
      save
C     !DASH
      real*8 ESG, X
      integer J, JE, JJGM, JJSA, JJXNE, JS, N
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
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(235),JJSA )
      equivalence (IZOQ( 16),JJGM )
C     !DASH
      external URBINO, HI, BYE
C
      dimension X(*)
C
C               ESG(N,NSG)
      dimension ESG(*)
C
      call HI ('BURION')
C     !BEG
      do 100 J = JS,JE
        call URBINO (N, J, X(JJXNE), X(JJSA), X(JJGM), ESG)
  100 continue
C     !END
      call BYE ('BURION')
C
      return
      end
