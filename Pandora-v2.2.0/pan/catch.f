      subroutine CATCH
     $(N,TE,XNE,XNHE1K,XNHE21,A,XNHEP)
C
C     Rudolf Loeser, 1984 May 08
C---- Computes auxiliary functions, for PENDANT.
C     !DASH
      save
C     !DASH
      real*8 A, CON1, RAT, TE, XNE, XNHE1K, XNHE21, XNHEP
      integer I, N
C     !DASH
      external DIVIDE, RIGEL, ARRAVE, HI, BYE
C
C               TE(N), XNE(N), XNHE1K(N), XNHE21(N), A(N), XNHEP(N)
      dimension TE(*), XNE(*), XNHE1K(*), XNHE21(*), A(*), XNHEP(*)
C
      call HI ('CATCH')
C     !BEG
      call RIGEL    (1,CON1)
C
      do 100 I = 1,N
        call DIVIDE (TE(I),XNE(I),RAT)
        A(I) = sqrt(CON1*RAT)
  100 continue
C
      call ARRAVE   (XNHE1K,XNHE21,XNHEP,N)
C     !END
      call BYE ('CATCH')
C
      return
      end
