      subroutine KOLAR
     $(KAMB,N,NL,PLK,SPKL,RND,R,PALBET,PBETAL)
C
C     Rudolf Loeser, 1998 May 01
C---- Computes r, for FETTER.
C     (This is version 2 of KOLAR.)
C     !DASH
      save
C     !DASH
      real*8 PALBET, PBETAL, PLK, R, RND, SPKL
      integer KAMB, N, NL
C     !DASH
      external KORAL, MOVE1, ARRADD, HI, BYE
C
C               RND(N,NL), PLK(N,NL), SPKL(N), PALBET(N), PBETAL(N),
      dimension RND(*),    PLK(*),    SPKL(*), PALBET(*), PBETAL(*),
C
C               R(N)
     $          R(*)
C
      call HI ('KOLAR')
C     !BEG
      call KORAL   (N,NL,RND,PLK,R)
      if(KAMB.eq.2) then
        call MOVE1 (R   ,N,PALBET)
        call MOVE1 (SPKL,N,PBETAL)
      end if
      call ARRADD  (R,SPKL,R,N)
C     !END
      call BYE ('KOLAR')
C
      return
      end
