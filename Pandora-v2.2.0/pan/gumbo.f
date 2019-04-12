      subroutine GUMBO
     $(N,K,XQSF,VXI,XJNU,SNU)
C
C     Rudolf Loeser, 1978 Apr 23
C---- Computes SNU, a PRD term.
C     (This is version 3 of GUMBO.)
C     !DASH
      save
C     !DASH
      real*8 SNU, VXI, XJNU, XQSF
      integer K, N, NK
C     !DASH
      external ARRMUL, ARRADD, HI, BYE
C
C               XQSF(N,K), VXI(N,K), XJNU(N,K), SNU(N,K)
      dimension XQSF(*),   VXI(*),   XJNU(*),   SNU(*)
C
      call HI ('GUMBO')
C     !BEG
      NK = N*K
      call ARRMUL (VXI,XJNU,SNU,NK)
      call ARRADD (SNU,XQSF,SNU,NK)
C     !END
      call BYE ('GUMBO')
C
      return
      end
