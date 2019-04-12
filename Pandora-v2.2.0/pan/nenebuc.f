      subroutine NENEBUC
     $(CPIJ,ID,W)
C
C     Rudolf Loeser, 1991 Aug 23
C---- Moves the values of CIJ or PIJ at depth ID into W.
C     !DASH
      save
C     !DASH
      real*8 CPIJ, W
      integer I, ID, J, N, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
      external HI, BYE
C
C               CPIJ(N,NL,NL), W(NL,NL)
      dimension CPIJ(N,NL,*),  W(NL,*)
C
      call HI ('NENEBUC')
C     !BEG
      do 101 J = 1,NL
        do 100 I = 1,NL
          W(I,J) = CPIJ(ID,I,J)
  100   continue
  101 continue
C     !END
      call BYE ('NENEBUC')
C
      return
      end
