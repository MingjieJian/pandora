      subroutine MINNOW
     $(N,EMUXK,BD,V)
C
C     Rudolf Loeser, 2003 Jan 03
C---- Computes V for a given level, for Lyman calculations.
C     (This is version 2 of MINNOW.)
C     !DASH
      save
C     !DASH
      real*8 BD, EMUXK, ONE, V
      integer N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ARRDIV, NEGATE, CONADD, HI, BYE
C
C               EMUXK(N), BD(N), V(N)
      dimension EMUXK(*), BD(*), V(*)
C
      call HI ('MINNOW')
C     !BEG
      call ARRDIV (EMUXK, BD, V, N)
      call NEGATE (V, N)
      call CONADD (ONE, V, N)
C     !END
      call BYE ('MINNOW')
C
      return
      end
