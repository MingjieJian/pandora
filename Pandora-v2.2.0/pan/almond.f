      subroutine ALMOND
     $(N,NL,NSL,M,RL,RK,GMI,SPKL,EPCBR,XND,ARHO,PS1,DUMP)
C
C     Rudolf Loeser, 1978 Jun 26
C---- Provides for supplementary levels, for COCOS.
C     (This is version 2 of ALMOND.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, EPCBR, GMI, PS1, RK, RL, SPKL, XND
      integer M, N, NL, NSL
      logical DUMP
C     !DASH
      external ZERO1, DEER, DROP, PIT, HI, BYE
C
C               RL(N,NSL), RK(N,NSL), GMI(N,NSL), XND(N,NL), ARHO(N,NL),
      dimension RL(*),     RK(*),     GMI(*),     XND(*),    ARHO(*),
C
C               SPKL(N), PS1(N)
     $          SPKL(*), PS1(*)
C
      call HI ('ALMOND')
C     !BEG
      if(NSL.gt.NL) then
        call DEER  (N, NL, NSL, M, GMI, RL, RK, SPKL)
        call DROP  (N, NL, NSL, M, EPCBR, XND, ARHO, PS1)
        call PIT   (N, EPCBR, SPKL, PS1, DUMP)
      else
        call ZERO1 (SPKL, N)
        call ZERO1 (PS1 , N)
      end if
C     !END
      call BYE ('ALMOND')
C
      return
      end
