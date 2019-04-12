      subroutine EUTROPA
     $(N,NL,CIJ,AIJ,CAU1)
C
C     Rudolf Loeser, 1977 Mar 14
C---- Gets intermediates, for SOPHRON.
C     !DASH
      save
C     !DASH
      real*8 A, AIJ, CAU1, CIJ, ZERO
      integer I, J, J1, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external INDXIJ, HI, BYE
C
C               CIJ(N,NL**2), AIJ(NL,NL), CAU1(N,NL)
      dimension CIJ(N,*),     AIJ(NL,*),  CAU1(N,*)
C
      call HI ('EUTROPA')
C     !BEG
      do 101 I = 1,N
        do 100 J = 2,NL
          A = AIJ(J,1)
          if(A.le.ZERO) then
            CAU1(I,J) = ZERO
          else
C
            call INDXIJ (J, 1, J1)
            CAU1(I,J) = CIJ(I,J1)/A
          end if
  100   continue
  101 continue
C     !END
      call BYE ('EUTROPA')
C
      return
      end
