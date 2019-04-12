      subroutine STURM
     $(N,KK,TNU,XK,XINK,FINK,Z,XNUK,EXT,CNXP)
C
C     Rudolf Loeser, 1994 Sep 16
C---- Computes CNXP, for MIMOSA.
C     (This is version 3 of STURM.)
C     !DASH
      save
C     !DASH
      real*8 CNDT, CNXP, EXT, FINK, FNU, TNU, XINK, XK, XNUK, Z
      integer I, K, KK, N
C     !DASH
      external RAGE, SPUME, HI, BYE
C
C               CNXP(N,KKX), XK(KKX), XINK(INK), FINK(INK), TNU(N,KKX),
      dimension CNXP(N,*),   XK(*),   XINK(*),   FINK(*),   TNU(N,*),
C
C               Z(N), EXT(N)
     $          Z(*), EXT(*)
C
      call HI ('STURM')
C     !BEG
      do 101 K = 1,KK
        FNU = XK(K)*XNUK
C----   Compute extinction
        call RAGE    (TNU(1,K), N, EXT)
        do 100 I = 1,N
C----     Compute incident radiation
          call SPUME (FNU, I, Z, 1, 1, XINK, FINK, CNDT)
          CNXP(I,K) = EXT(I)*CNDT
  100   continue
  101 continue
C     !END
      call BYE ('STURM')
C
      return
      end
