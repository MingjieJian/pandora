      subroutine ROCKER
     $(INDEX,N,TE,ALFH)
C
C     Rudolf Loeser, 2004 Sep 20
C---- Computes ALFH (alpha-Hydrogen) for lower-level charge exchange.
C     (This is version 2 of ROCKER.)
C     !DASH
      save
C     !DASH
      real*8 A, ALFH, B, C, D, ET, FAC, ONE, T4, TE, TP, dummy
      integer I, INDEX, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RASH, HI, BYE
C
C               TE(N), ALFH(N)
      dimension TE(*), ALFH(*)
C
      data FAC /1.D-4/
C
      call HI ('ROCKER')
C     !BEG
      call RASH (1, INDEX, A, B, C, D, dummy, dummy)
C
      do 100 I = 1,N
        T4 = FAC*TE(I)
        TP = T4**B
        ET = exp(T4*D)
C
        ALFH(I) = A*TP*(ONE+C*ET)
  100 continue
C     !END
      call BYE ('ROCKER')
C
      return
      end
