      subroutine OLM
     $(N,NL,POPK,POPN,BD)
C
C     Rudolf Loeser, 1986 Apr 24
C---- Zeroes out unneeded populations data.
C     (This is version 2 of OLM.)
C     !DASH
      save
C     !DASH
      real*8 BD, POPK, POPN
      integer N, NL, NNL
C     !DASH
      external ZERO1, HI, BYE
C
C               POPK(N), POPN(N,NL), BD(N,NL)
      dimension POPK(*), POPN(*),    BD(*)
C
      call HI ('OLM')
C     !BEG
      NNL = N*NL
      call ZERO1 (POPK,N  )
      call ZERO1 (POPN,NNL)
      call ZERO1 (BD  ,NNL)
C     !END
      call BYE ('OLM')
C
      return
      end
