      subroutine FYRD
     $(N,K,DL,SNU,IMIN,IMAX,KRED,DLRED,IRAY,NRP,SNURED)
C
C     Rudolf Loeser, 1983 Sep 02
C---- Sets up reduced-Delta-Lambda and reduced-SNU tables,
C     for shell rays.
C     (See also DIVES.)
C     !DASH
      save
C     !DASH
      real*8 DL, DLRED, SNU, SNURED
      integer IMAX, IMIN, IRAY, J, K, KRED, N, NRP
C     !DASH
      external MOVE1, BARBARA, HI, BYE
C
C               DL(KM), SNU(N,KM), DLRED(KM), SNURED(NRPMX,KM)
      dimension DL(*),  SNU(N,*),  DLRED(*),  SNURED(NRP,*)
C
      call HI ('FYRD')
C     !BEG
      KRED = 0
      do 100 J = IMIN,IMAX
        KRED = KRED+1
        call BARBARA (SNU(1,J),IRAY,SNURED(1,KRED))
  100 continue
C
      call MOVE1     (DL(IMIN),KRED,DLRED)
C     !END
      call BYE ('FYRD')
C
      return
      end
