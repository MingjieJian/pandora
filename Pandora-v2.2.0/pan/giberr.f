      subroutine GIBERR
     $(DIJ,N,I,JS,JE,LIST)
C
C     Rudolf Loeser, 1998 Oct 29
C---- Finds maxima, for BIRGER.
C     !DASH
      save
C     !DASH
      real*8 DIJ
      integer I, IJ, IMAX, IMIN, J, JE, JS, L, LIST, N
C     !DASH
      external INDXIJ, MINMAXD, HI, BYE
C
C               DIJ(N,NL**2), LIST(30)
      dimension DIJ(N,*),     LIST(*)
C
      call HI ('GIBERR')
C     !BEG
      L = 0
      do 100 J = JS,JE
        L = L+1
        call INDXIJ  (I,J,IJ)
        call MINMAXD (DIJ(1,IJ),1,N, IMIN,IMAX)
        LIST(L) = IMAX
  100 continue
C     !END
      call BYE ('GIBERR')
C
      return
      end
