      subroutine FOXTAIL
     $(N,XA,WN,XM)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Sets up matrix XM (= script M), for TREFOIL.
C     !DASH
      save
C     !DASH
      real*8 DELTA, ONE, WN, XA, XM
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
      external UNIT, HI, BYE
C
C               XA(N), WN(N,N), XM(N,N)
      dimension XA(*), WN(N,*), XM(N,*)
C
      call HI ('FOXTAIL')
C     !BEG
      do 101 I = 1,N
        do 100 J = 1,N
          call UNIT (I,J,DELTA)
          XM(I,J) = DELTA*(ONE-XA(I))-XA(J)*WN(I,J)
  100   continue
  101 continue
C     !END
      call BYE ('FOXTAIL')
C
      return
      end
