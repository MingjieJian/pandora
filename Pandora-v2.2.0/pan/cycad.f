      subroutine CYCAD
     $(EP1N,EP2N,METEP,EP1,EP2)
C1
C     Rudolf Loeser, 1987 Nov 05
C---- Selects the EP1 and EP2 results, according to METEP.
C     (Note: 0 .le. METEP .le. 3)
C     !DASH
      save
C     !DASH
      real*8 EP1, EP1N, EP2, EP2N
      integer J, METEP, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external MOVE1, HI, BYE
C
C               EP1N(N,4), EP2N(N,4), EP1(N), EP2(N)
      dimension EP1N(N,*), EP2N(N,*), EP1(*), EP2(*)
C
      call HI ('CYCAD')
C     !BEG
      J = METEP+1
      call MOVE1 (EP1N(1,J),N,EP1)
      call MOVE1 (EP2N(1,J),N,EP2)
C     !END
      call BYE ('CYCAD')
C
      return
      end
