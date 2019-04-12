      subroutine DEAL
     $(N,CDW,DW,T)
C
C     Rudolf Loeser, 1984 Dec 12
C---- Computes T, an intermediate quantity for the Line Source Function
C     calculation.
C     (This is version 2 of DEAL.)
C     !DASH
      save
C     !DASH
      real*8 CDW, DW, T
      integer N
C     !DASH
      external SET1, ARRDIV, HI, BYE
C
C               DW(N), T(N)
      dimension DW(*), T(*)
C
      call HI ('DEAL')
C     !BEG
      call SET1   (T,N,CDW)
      call ARRDIV (T,DW,T,N)
C     !END
      call BYE ('DEAL')
C
      return
      end
