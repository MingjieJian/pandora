      subroutine FULDA
     $(LU,N,K,XJNU,DL,TIT,IW)
C
C     Rudolf Loeser, 1995 Aug 25
C---- Driver for FOSFOR.
C     !DASH
      save
C     !DASH
      real*8 DL, XJNU
      integer IW, IWS, JN, JNUX, K, LU, MUX, N
      character TIT*(*)
C     !DASH
      external FOSFOR, WERRA, IGIVE, HI, BYE
C
      dimension IW(*)
C
C               XJNU(N,K), DL(K)
      dimension XJNU(*),   DL(*)
C
      dimension JN(1)
      equivalence
     $(JN( 1),JNUX )
C
      call HI ('FULDA')
C     !BEG
      if(LU.gt.0) then
C       (Get IW allotment)
        call WERRA  (JN,IWS,MUX,'FULDA',K)
C
        call FOSFOR (LU,N,K,XJNU,DL,IW(JNUX),TIT)
C
C       (Give back IW allotment)
        call IGIVE  (IW,'FULDA')
      end if
C     !END
      call BYE ('FULDA')
C
      return
      end
