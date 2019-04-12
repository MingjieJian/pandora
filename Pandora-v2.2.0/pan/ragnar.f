      subroutine RAGNAR
     $(NDT,WAVE,FREQ,OPAC,H,N,Z,HK,HT,IMG,W)
C
C     Rudolf Loeser, 1987 Mar 24
C---- Computes flux-weighted-mean opacity and optical depth.
C     !DASH
      save
C     !DASH
      real*8 FREQ, H, HK, HT, OPAC, W, WAVE, Z
      integer IAA, IBB, IFF, IFINT, IGG, IMG, IN, IS, MOX, N, NDT
C     !DASH
      external MIAOU, SHARK, WGIVE, HI, BYE
C
      dimension W(*)
C
C               WAVE(NDT), H(N,NDT), OPAC(N,NDT), IMG(N), HT(N), HK(N),
      dimension WAVE(*),   H(*),     OPAC(*),     IMG(*), HT(*), HK(*),
C
C               Z(N), FREQ(NDT)
     $          Z(*), FREQ(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IAA   ),(IN( 2),IBB   ),(IN( 3),IFINT ),(IN( 4),IFF   ),
     $(IN( 5),IGG   )
C
      call HI ('RAGNAR')
C     !BEG
C     (Get, and allocate, W allotment)
      call MIAOU (IN, IS, MOX, 'RAGNAR')
C
      call SHARK (Z, N, NDT, FREQ, WAVE, OPAC, H, W(IAA), W(IBB),
     $            W(IFINT), W(IFF), W(IGG), IMG, W, HK, HT)
C
C     (Give back W allotment)
      call WGIVE (W, 'RAGNAR')
C     !END
      call BYE ('RAGNAR')
C
      return
      end
