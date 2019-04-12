      subroutine CHUKOR
     $(NDT,N,OPAC,H,FF,GG)
C
C     Rudolf Loeser, 1987 Mar 24
C---- Gets FF and GG, for SHARK.
C     !DASH
      save
C     !DASH
      real*8 FF, GG, H, OPAC
      integer J, N, NDT
C     !DASH
      external ARRMUL, ABSVECD, HI, BYE
C
C               OPAC(N,NDT), H(N,NDT), FF(N,NDT), GG(N,NDT)
      dimension OPAC(N,*),   H(N,*),   FF(N,*),   GG(N,*)
C
      call HI ('CHUKOR')
C     !BEG
      do 100 J = 1,NDT
        call ABSVECD (H(1,J),1,N,GG(1,J),1,N)
        call ARRMUL  (OPAC(1,J),GG(1,J),FF(1,J),N)
  100 continue
C     !END
      call BYE ('CHUKOR')
C
      return
      end
