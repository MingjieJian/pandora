      subroutine BUREKA
     $(N,TE,XNC,AN1S)
C
C     Rudolf Loeser, 2003 Jan 27
C---- Computes a sum of Hydrogen A-values, for H Ly lines background
C     opacity.
C     !DASH
      save
C     !DASH
      real*8 AN1S, ANK, TE, XNC, ZERO
      integer K, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external KERBAU, HI, BYE
C
      call HI ('BUREKA')
C     !BEG
      AN1S = ZERO
      if(N.gt.1) then
        do 100 K = 1,(N-1)
          call KERBAU (N,K,TE,XNC,ANK)
          AN1S = AN1S+ANK
  100   continue
      end if
C     !END
      call BYE ('BUREKA')
C
      return
      end
