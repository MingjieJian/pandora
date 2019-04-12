      subroutine SUSAN
     $(BDI,NONE)
C
C     Rudolf Loeser, 2003 Jun 13
C---- Checks whether there were input values of Bs.
C     (This is version 4 of SUSAN.)
C     !DASH
      save
C     !DASH
      real*8 BDI, ONE
      integer N, NL
      logical NONE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external KONSTD, HI, BYE
C
C               BDI(N,NL)
      dimension BDI(*)
C
      call HI ('SUSAN')
C     !BEG
      call KONSTD (BDI, 1, (N*NL), ONE, NONE)
C     !END
      call BYE ('SUSAN')
C
      return
      end
