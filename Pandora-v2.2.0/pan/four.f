      subroutine FOUR
     $(PE,FE)
C
C     Rudolf Loeser, 1988 Apr 01
C---- Saves debug checksums, for SWALLOW.
C     !DASH
      save
C     !DASH
      real*8 FE, PE
      integer N
      character TIT*40
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external CHECKER, MISO, HI, BYE
C
C               PE(N), FE(N)
      dimension PE(*), FE(*)
C
      data TIT /'---'/
C
      call HI ('FOUR')
C     !BEG
      call MISO    (TIT(4:28))
C
      TIT(1:3) = ' PE'
      call CHECKER (PE,1,N,TIT)
C
      TIT(1:3) = ' FE'
      call CHECKER (FE,1,N,TIT)
C     !END
      call BYE ('FOUR')
C
      return
      end
