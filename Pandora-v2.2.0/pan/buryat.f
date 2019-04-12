      subroutine BURYAT
     $(I,N,W,D,DT)
C
C     Rudolf Loeser, 1989 Jun 29
C---- Modifies row I of a "GR" weight matrix, for the
C     semi-infinite case.
C     (This is version 2 of BURYAT.)
C     !DASH
      save
C     !DASH
      real*8 D, DT, EX, HALF, ONE, W, ZERO, ZK
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               W(N,N), D(N)
      dimension W(N,*), D(*)
C
      call HI ('BURYAT')
C     !BEG
      EX = exp(-DT)
      if(EX.gt.ZERO) then
        ZK = HALF*(EX/D(N-1))
        W(I,N-1) = W(I,N-1)-ZK
        W(I,N  ) = W(I,N  )+ZK*(ONE+D(N-1))
      end if
C     !END
      call BYE ('BURYAT')
C
      return
      end
