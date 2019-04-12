      subroutine SARCASM
     $(N,YH,TE,HND,XNE,VT,PMG,DGM,F,G,W)
C
C     Rudolf Loeser, 2003 Nov 04
C---- Computes approximate initial values of F and G (cf. HSE).
C     !DASH
      save
C     !DASH
      real*8 DGM, F, G, HND, PMG, TE, VT, W, XNE, YH
      integer IEMN, IGDN, IH2N, IHELAB, IN, IPEL, IPEX, IPGS, IPNH,
     $        IPTO, IPTU, IS, IVM, MOX, N
C     !DASH
      external MASSARC, CRASSAM, WGIVE, HI, BYE
C
      dimension W(*)
C
C               HND(N), TE(N), VT(N), DGM(N), F(N), PMG(N), XNE(N),
      dimension HND(*), TE(*), VT(*), DGM(*), F(*), PMG(*), XNE(*),
C
C               G(N)
     $          G(*)
C
      dimension IN(11)
      equivalence
     $(IN( 1),IPTO  ),(IN( 2),IPEL  ),(IN( 3),IPGS  ),(IN( 4),IPTU  ),
     $(IN( 5),IPEX  ),(IN( 6),IPNH  ),(IN( 7),IHELAB),(IN( 8),IH2N  ),
     $(IN( 9),IVM   ),(IN(10),IGDN  ),(IN(11),IEMN  )
C
      call HI ('SARCASM')
C     !BEG
C     (Get, and allocate, W allotment)
      call MASSARC (IN, IS, MOX, 'SARCASM')
C
      call CRASSAM (N, YH, TE, HND, XNE, VT, PMG, DGM, F, G, W(IPTO),
     $              W(IPEL), W(IPGS), W(IPTU), W(IPEX), W(IPNH),
     $              W(IHELAB), W(IH2N), W(IVM), W(IGDN), W(IEMN))
C
C     (Give back W allotment)
      call WGIVE   (W, 'SARCASM')
C     !END
      call BYE ('SARCASM')
C
      return
      end
