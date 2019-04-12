      subroutine HIGRY
     $(N,TE,XNE,HND,BDHM,TDUST)
C
C     Rudolf Loeser, 1988 Oct 24
C---- Initializes some Continuum Recalculation controls.
C     !DASH
      save
C     !DASH
      real*8 BDHM, HND, TDUST, TE, XNE
      integer N
C     !DASH
      external WENDY, HI, BYE
C
C               TE(N), XNE(N), HND(N), BDHM(N), TDUST(N)
      dimension TE(*), XNE(*), HND(*), BDHM(*), TDUST(*)
C
      call HI ('HIGRY')
C     !BEG
      call WENDY (TE   ,1,N,1,'HIGRY')
      call WENDY (XNE  ,1,N,3,'HIGRY')
      call WENDY (HND  ,1,N,4,'HIGRY')
      call WENDY (BDHM ,1,N,5,'HIGRY')
      call WENDY (TDUST,1,N,6,'HIGRY')
C     !END
      call BYE ('HIGRY')
C
      return
      end
