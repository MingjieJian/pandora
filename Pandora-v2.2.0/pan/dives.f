      subroutine DIVES
     $(N,K,DL,SNU,IMIN,IMAX,KRED,DLRED,SNURED)
C
C     Rudolf Loeser, 1983 Sep 01
C---- Sets up reduced Delta-Lambda and SNU tables,
C     for Disk rays, and for rays in plane-parallel atmospheres.
C     (See also FYRD.)
C     !DASH
      save
C     !DASH
      real*8 DL, DLRED, SNU, SNURED
      integer IMAX, IMIN, J, K, KRED, N
C     !DASH
      external MOVE1, HI, BYE
C
C               DL(KM), SNU(N,KM), DLRED(KM), SNURED(N,KM)
      dimension DL(*),  SNU(N,*),  DLRED(*),  SNURED(N,*)
C
      call HI ('DIVES')
C     !BEG
      KRED = 0
      do 100 J = IMIN,IMAX
        KRED = KRED+1
        call MOVE1 (SNU(1,J),N,SNURED(1,KRED))
  100 continue
C
      call MOVE1   (DL(IMIN),KRED,DLRED)
C     !END
      call BYE ('DIVES')
C
      return
      end
