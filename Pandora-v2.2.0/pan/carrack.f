      subroutine CARRACK
     $(NF,FREQ,N,OP,TE,FF,GG)
C
C     Rudolf Loeser, 1987 Mar 20
C---- Gets FF and GG, for WILLET.
C     (This is version 2 of CARRACK.)
C     !DASH
      save
C     !DASH
      real*8 FF, FREQ, GG, OP, TE
      integer J, N, NF
C     !DASH
      external LOACH, HI, BYE
C
C               FREQ(NF), TE(N), OP(N,NF), FF(N,NF), GG(N,NF)
      dimension FREQ(*),  TE(*), OP(N,*),  FF(N,*),  GG(N,*)
C
      call HI ('CARRACK')
C     !BEG
      do 100 J = 1,NF
        call LOACH (FREQ(J),N,OP(1,J),TE,FF(1,J),GG(1,J))
  100 continue
C     !END
      call BYE ('CARRACK')
C
      return
      end
