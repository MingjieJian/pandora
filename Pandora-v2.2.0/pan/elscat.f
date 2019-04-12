      subroutine ELSCAT
     $(INDX,XNE,N,NOPAC,CONT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes a set of Electron Scattering values.
C     (This is version 2 of ELSCAT.)
C     !DASH
      save
C     !DASH
      real*8 CON47, CONT, XNE
      integer INDX, J, N, NOPAC
C     !DASH
      external RIGEL, HI, BYE
C
C               XNE(N), CONT(Nopac,N)
      dimension XNE(*), CONT(NOPAC,*)
C
      call HI ('ELSCAT')
C     !BEG
      call RIGEL (47, CON47)
C
      do 100 J = 1,N
        CONT(INDX,J) = CON47*XNE(J)
  100 continue
C     !END
      call BYE ('ELSCAT')
C
      return
      end
