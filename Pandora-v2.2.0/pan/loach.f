      subroutine LOACH
     $(XNU,N,OPAC,TE,FF,GG)
C
C     Rudolf Loeser, 1986 Mar 17
C---- Computes FF and GG, for CARRACK.
C     !DASH
      save
C     !DASH
      real*8 EL, EMHNOKT, FF, GG, HNOKT, OPAC, TE, XDEN, XNU, XNUM
      integer I, N
C     !DASH
      external PROD, QEXP1, DIVIDE, ARRDIV, HI, BYE
C
C               OPAC(N), TE(N), FF(N), GG(N)
      dimension OPAC(*), TE(*), FF(*), GG(*)
C
      call HI ('LOACH')
C     !BEG
      do 100 I = 1,N
        call PROD   (TE(I),XNU,1,HNOKT,EMHNOKT)
        call QEXP1  (HNOKT,EMHNOKT,1,EL)
        XNUM = (XNU**4)*EMHNOKT
        XDEN = EL**2
        call DIVIDE (XNUM,XDEN,FF(I))
  100 continue
      call ARRDIV   (FF,OPAC,GG,N)
C     !END
      call BYE ('LOACH')
C
      return
      end
