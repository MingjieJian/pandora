      subroutine ODEON
     $(C,N,WN,XA,XJNU,SUM)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Accumulates an angle contribution, for Jnu-calculation.
C     Note: SUM is initialized by the caller.
C     !DASH
      save
C     !DASH
      real*8 C, RUM, SUM, WN, XA, XJNU, ZERO
      integer I, J, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               WN(N,N), XA(N,N), XJNU(N), SUM(N), C(N)
      dimension WN(N,*), XA(N,*), XJNU(*), SUM(*), C(*)
C
      call HI ('ODEON')
C     !BEG
      do 101 I = 1,N
C
        RUM = ZERO
        do 100 J = 1,N
          RUM = RUM+WN(I,J)*XA(I,J)*XJNU(J)
  100   continue
C
        SUM(I) = SUM(I)+C(I)*RUM
  101 continue
C     !END
      call BYE ('ODEON')
C
      return
      end
