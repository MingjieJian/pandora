      subroutine OLENEK
     $(N,K,ICE,CS,PSNU,SNU)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Sets up SNU, for TOBOL.
C     (This is version 2 of OLENEK.)
C     !DASH
      save
C     !DASH
      real*8 CS, PSNU, SNU
      integer ICE, J, K, N
C     !DASH
      external MOVE1, HI, BYE
C
C               SNU(N,K), PSNU(N,K), CS(N)
      dimension SNU(N,*), PSNU(*),   CS(*)
C
      call HI ('OLENEK')
C     !BEG
      if(ICE.ne.0) then
C----   PRD case
        call MOVE1   (PSNU,(N*K),SNU     )
      else
C----   CRD case
        do 100 J = 1,K
          call MOVE1 (CS  ,N    ,SNU(1,J))
  100   continue
      end if
C     !END
      call BYE ('OLENEK')
C
      return
      end
