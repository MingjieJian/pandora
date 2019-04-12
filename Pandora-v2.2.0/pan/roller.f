      subroutine ROLLER
     $(INDEX,N,TE,ALFHE)
C
C     Rudolf Loeser, 2004 Sep 20
C---- Computes ALFHE (alpha-Helium) for lower-level charge exchange.
C     (This is version 2 of ROLLER.)
C     !DASH
      save
C     !DASH
      real*8 A, ALFHE, B, C, D, ET, FAC, ONE, T4, TE, TP, ZERO, dummy
      integer I, INDEX, N
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
      external RASH, ZERO1, HI, BYE
C
C               TE(N), ALFHE(N)
      dimension TE(*), ALFHE(*)
C
      data FAC /1.D-4/
C
      call HI ('ROLLER')
C     !BEG
      call RASH    (2, INDEX, A, B, C, D, dummy, dummy)
C
      if(A.ne.ZERO) then
C
        do 100 I = 1,N
          T4 = FAC*TE(I)
          TP = T4**B
          ET = exp(T4*D)
C
          ALFHE(I) = A*TP*(ONE+C*ET)
  100   continue
C
      else
        call ZERO1 (ALFHE, N)
      end if
C     !END
      call BYE ('ROLLER')
C
      return
      end
