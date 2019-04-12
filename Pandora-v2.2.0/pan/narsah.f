      subroutine NARSAH
     $(KINMAX,KININT,TE,N,INDEX,KNT)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Selects KNT depth indices for plotting.
C     !DASH
      save
C     !DASH
      real*8 TE
      integer I, IMAX, IMIN, INDEX, KININT, KINMAX, KNT, N
C     !DASH
      external  MINMAXD, HI, BYE
      intrinsic min, max
C
C               TE(N), INDEX(KNT)
      dimension TE(*), INDEX(*)
C
      call HI ('NARSAH')
C     !BEG
      if(KINMAX.le.0) then
        call MINMAXD (TE,1,N,IMIN,IMAX)
        KINMAX = IMIN+KININT
      end if
      INDEX(1) = KINMAX
C
      do 100 I = 2,KNT
        INDEX(I) = INDEX(I-1)-KININT
  100 continue
C
      do 101 I = 1,KNT
        INDEX(I) = max(min(INDEX(I),N),1)
  101 continue
C     !END
      call BYE ('NARSAH')
C
      return
      end
