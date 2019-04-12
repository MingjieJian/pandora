      subroutine THAMOS
     $(XPBL,H1)
C
C     Rudolf Loeser, 2004 Apr 15
C---- Sets up reserved set of Hydrogen level-1 number density.
C     !DASH
      save
C     !DASH
      real*8 H1, XPBL, dummy
      integer N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external POPUTIL, WENDY, HI, BYE
C
C               XPBL(Lenpbl), H1(N)
      dimension XPBL(*),      H1(*)
C
      call HI ('THAMOS')
C     !BEG
      call POPUTIL (XPBL, 1, 1, H1, 0, dummy, 0, dummy, 0, dummy)
      call WENDY   (H1, 1, N, 43, 'THAMOS')
C     !END
      call BYE ('THAMOS')
C
      return
      end
