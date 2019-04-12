      subroutine ENTRAIL
     $(N,M,CK,RS,SUMS,SUMU,EP1,EP2,GVL,KDGV)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Computes Epsilons, for ALTAR.
C     !DASH
      save
C     !DASH
      real*8 CK, EP1, EP2, GVL, ONE, OOR, RS, SUMS, SUMU, VGT, ZERO
      integer I, KDGV, M, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               CK(N,NL), GVL(N,NL), SUMS(N), SUMU(N), EP1(N), EP2(N),
      dimension CK(N,*),  GVL(N,*),  SUMS(*), SUMU(*), EP1(*), EP2(*),
C
C               RS(N)
     $          RS(*)
C
      call HI ('ENTRAIL')
C     !BEG
      VGT = ZERO
C
      do 100 I = 1,N
        if(KDGV.ne.0) then
          VGT = GVL(I,M)
        end if
        call DIVIDE (ONE,RS(I),OOR)
C
        EP1(I)=(CK(I,M)+SUMS(I)    )*OOR
        EP2(I)=(CK(I,M)+SUMU(I)+VGT)*OOR
  100 continue
C     !END
      call BYE ('ENTRAIL')
C
      return
      end
