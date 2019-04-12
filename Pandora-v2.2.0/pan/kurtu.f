      subroutine KURTU
     $(N,EM,EP,DEL,PA)
C
C     Rudolf Loeser, 1984 Nov 02
C           revised, 2004 May 07
C
C---- Computes matrix M for LINDEN.
C     !DASH
      save
C     !DASH
      real*8 DEL, EM, EP, OED, ONE, PA
      integer I, J, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external UNITADD, DIVIDE, HI, BYE
C
C               EP(N), DEL(N), EM(N,N), PA(N,N)
      dimension EP(*), DEL(*), EM(N,*), PA(N,*)
C
      call HI ('KURTU')
C     !BEG
      do 101 I = 1,N
        call DIVIDE (ONE, (EP(I)+DEL(I)), OED)
        do 100 J = 1,N
          EM(I,J) = -OED*PA(I,J)
  100   continue
  101 continue
      call UNITADD  (1, N, EM, N)
C     !END
      call BYE ('KURTU')
C
      return
      end
