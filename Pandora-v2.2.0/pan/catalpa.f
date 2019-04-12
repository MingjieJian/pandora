      subroutine CATALPA
     $(TAU,IMAX,N,Y,G)
C
C     Rudolf Loeser, 1981 Jan 20
C---- Computes the Matrix G(i,j), for QR weight matrix.
C     Only the elements (1 .le. i .le. IMAX, 1 .le. j .le. N)
C     are computed, where IMAX is either N or N-2.
C     (This is version 3 of CATALPA.)
C     !DASH
      save
C     !DASH
      real*8 E2TI, E3TI, G, HALF, TAU, TI, TJ, Y, dummy
      integer I, IMAX, J, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external EXPINT, GIN, HI, BYE
C
C               TAU(N), G(N,N)
      dimension TAU(*), G(N,*)
C
      call HI ('CATALPA')
C     !BEG
C---- Loop over all rows.
      do 101 I = 1,IMAX
C       (TI is "TAU")
        TI = TAU(I)
C----   Left-most element.
        call EXPINT (2,TI,E2TI,dummy)
        G(I,1) = -HALF*E2TI
C----   Interior elements.
        do 100 J = 2,(N-1)
C         (TJ is "T")
          TJ = TAU(J)
          call GIN  (TI,TJ,Y,G(I,J))
  100   continue
C----   Right-most element.
        call EXPINT (3,TI,E3TI,dummy)
        G(I,N) = HALF*E3TI
  101 continue
C     !END
      call BYE ('CATALPA')
C
      return
      end
