      subroutine PRION
     $(NW,WAVE,FREQ,OPAC,N,Z,TE,ROSSK,ROSST,IMG,W)
C
C     Rudolf Loeser, 1987 Mar 23
C---- Computes Rosseland-mean opacity and optical depth.
C     !DASH
      save
C     !DASH
      real*8 FREQ, OPAC, ROSSK, ROSST, TE, W, WAVE, Z
      integer IAA, IBB, IFF, IFINT, IGG, IMG, IN, IS, MOX, N, NW
C     !DASH
      external MEROU, WILLET, WGIVE, HI, BYE
C
      dimension W(*)
C
C               WAVE(NW), FREQ(NW), OPAC(N,NW), Z(N), IMG(N), ROSSK(N),
      dimension WAVE(*),  FREQ(*),  OPAC(N,*),  Z(*), IMG(*), ROSSK(*),
C
C               ROSST(N), TE(N)
     $          ROSST(*), TE(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IAA   ),(IN( 2),IBB   ),(IN( 3),IFINT ),(IN( 4),IFF   ),
     $(IN( 5),IGG   )
C
      call HI ('PRION')
C     !BEG
C     (Get, and allocate, W allotment)
      call MEROU  (IN,IS,MOX,'PRION')
C
      call WILLET (Z,TE,N,NW,FREQ,WAVE,OPAC,W(IAA),W(IBB),W(IFINT),
     $             W(IFF),W(IGG),W,IMG,ROSSK,ROSST)
C
C     (give back W allotment)
      call WGIVE  (W,'PRION')
C     !END
      call BYE ('PRION')
C
      return
      end
