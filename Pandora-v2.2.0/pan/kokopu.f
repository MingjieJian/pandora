      subroutine KOKOPU
     $(N,CHEFL,DEE,HND,ELL)
C
C     Rudolf Loeser, 1991 Jan 04
C---- Computes L for THALIA.
C     !DASH
      save
C     !DASH
      real*8 CHEFL, DEE, ELL, HND, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ZERO1, DIVIDE, HI, BYE
C
C               DEE(4,5,N), HND(N), ELL(N)
      dimension DEE(4,5,*), HND(*), ELL(*)
C
      call HI ('KOKOPU')
C     !BEG
      if(CHEFL.eq.ZERO) then
        call ZERO1    (ELL,N)
      else
        do 100 I = 1,N
          call DIVIDE (CHEFL,(DEE(2,2,I)*HND(I)),ELL(I))
  100   continue
      end if
C     !END
      call BYE ('KOKOPU')
C
      return
      end
