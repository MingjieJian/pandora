      subroutine KALPE
     $(NO,TE,TR,LIM,Z,N,ZLOG,LAB)
C
C     Rudolf Loeser, 1990 Dec 04
C---- Controls production of plots of TR vs. Z.
C     !DASH
      save
C     !DASH
      real*8 TE, TR, Z, ZLOG
      integer IE, IS, KNT, LIM, N, NO
      character LAB*(*), LEVELS*20, REMARK*8
C     !DASH
      external  MURZA, DATUK, COAL, HI, BYE
      intrinsic min
C
C               TR(N,LIM), Z(N), ZLOG(N), TE(N)
      dimension TR(N,*),   Z(*), ZLOG(*), TE(*)
C
      call HI ('KALPE')
C     !BEG
      if((NO.gt.0).and.(LIM.gt.0)) then
        call MURZA   (REMARK,LIM)
C
        IE = 0
  100   continue
          IS = IE+1
          IE = min((IE+26),LIM)
          call DATUK (LEVELS,IS,IE,LIM)
C
          KNT = IE-IS+1
          call COAL  (NO,TE,TR(1,IS),KNT,Z,N,ZLOG,LAB,REMARK,LEVELS)
C
        if(IE.lt.LIM) goto 100
      end if
C     !END
      call BYE ('KALPE')
C
      return
      end
