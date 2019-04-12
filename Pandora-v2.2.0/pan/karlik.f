      subroutine KARLIK
     $(NO,XJNU,LIM,Z,N,ZLOG)
C
C     Rudolf Loeser, 1990 Dec 04
C---- Controls production of plots of Jnu vs. Z.
C     !DASH
      save
C     !DASH
      real*8 XJNU, Z, ZLOG
      integer IE, IS, KNT, LIM, N, NO
      character LEVELS*20, REMARK*8
C     !DASH
      external  MURZA, DATUK, CROSS, HI, BYE
      intrinsic min
C
C               Z(N), ZLOG(N), XJNU(N,LIM)
      dimension Z(*), ZLOG(*), XJNU(N,*)
C
      call HI ('KARLIK')
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
          call CROSS (NO,XJNU(1,IS),KNT,Z,N,ZLOG,REMARK,LEVELS)
C
        if(IE.lt.LIM) goto 100
      end if
C     !END
      call BYE ('KARLIK')
C
      return
      end
