      subroutine TATTLE
     $(TR,ITAU,N,MTR,TRI,KNT,SOME)
C
C     Rudolf Loeser, 1984 Nov 16
C---- Extracts TR values, for ADDER.
C     (This is version 5 of TATTLE.)
C     !DASH
      save
C     !DASH
      real*8 TR, TRI, ZERO
      integer ITAU, J, KNT, MTR, N
      logical SOME
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
C               TR(N,MTR), TRI(MTR)
      dimension TR(N,*),   TRI(*)
C
      call HI ('TATTLE')
C     !BEG
      KNT = 0
      do 100 J = 1,MTR
        if(TR(ITAU,J).gt.ZERO) then
          KNT = KNT+1
          TRI(KNT) = TR(ITAU,J)
        end if
  100 continue
      SOME = KNT.ge.2
C     !END
      call BYE ('TATTLE')
C
      return
      end
