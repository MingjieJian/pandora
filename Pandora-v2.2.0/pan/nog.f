      subroutine NOG
     $(N,K,XJNU,JNUX)
C
C     Rudolf Loeser, 1982 Jul 26
C---- Sets up codes describing Jnu, for FOSFOR.
C     (This is version 2 of NOG.)
C     !DASH
      save
C     !DASH
      real*8 XJNU
      integer I, J, JNUX, K, N
C     !DASH
      external  ZEROI, HI, BYE
      intrinsic min, max
C
C               XJNU(N,K), JNUX(N,K)
      dimension XJNU(N,*), JNUX(N,*)
C
      call HI ('NOG')
C     !BEG
      call ZEROI (JNUX, 1, (N*K))
C
      do 101 I = 1,N
C
        if(XJNU(I,1).eq.max(XJNU(I,1),XJNU(I,2))) then
          JNUX(I,1) = +1
        end if
        if(XJNU(I,1).eq.min(XJNU(I,1),XJNU(I,2))) then
          JNUX(I,1) = -1
        end if
C
        do 100 J = 2,(K-1)
          if(XJNU(I,J).eq.max(XJNU(I,J-1),XJNU(I,J),XJNU(I,J+1))) then
            JNUX(I,J) = +1
          end if
          if(XJNU(I,J).eq.min(XJNU(I,J-1),XJNU(I,J),XJNU(I,J+1))) then
            JNUX(I,J) = -1
          end if
  100   continue
C
        if(XJNU(I,K).eq.max(XJNU(I,K-1),XJNU(I,K))) then
          JNUX(I,K) = +1
        end if
        if(XJNU(I,K).eq.min(XJNU(I,K-1),XJNU(I,K))) then
          JNUX(I,K) = -1
        end if
C
  101 continue
C     !END
      call BYE ('NOG')
C
      return
      end
