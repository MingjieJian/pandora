      subroutine GODFREY
     $(I,NW,XI,IND,NZE,XIS)
C
C     Rudolf Loeser, 1993 Jun 15
C---- Extracts selected values, for GRIFFIN.
C     !DASH
      save
C     !DASH
      real*8 XI, XIS
      integer I, IND, J, NW, NZE
C     !DASH
      external HI, BYE
C
C               XI(N), IND(NZE), XIS(NW,NZE)
      dimension XI(*), IND(*),   XIS(NW,*)
C
      call HI ('GODFREY')
C     !BEG
      do 100 J = 1,NZE
        XIS(I,J) = XI(IND(J))
  100 continue
C     !END
      call BYE ('GODFREY')
C
      return
      end
