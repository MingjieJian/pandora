      subroutine JARUL
     $(Z,XND,IMAGE,ZLOG,N,M,NO)
C
C     Rudolf Loeser, 1990 Dec 04
C---- Controls production of plots of number densities vs. Z.
C     !DASH
      save
C     !DASH
      real*8 XND, Z, ZLOG
      integer IE, II, IS, M, N, NO
      logical LAST
      character IMAGE*(*), LEVELS*20, REMARK*8
C     !DASH
      external  MURZA, DATUK, NEVER, HI, BYE
      intrinsic min
C
C               Z(N), ZLOG(N), XND(N,M)
      dimension Z(*), ZLOG(*), XND(N,*)
C
C
      call HI ('JARUL')
C     !BEG
      if((NO.gt.0).and.(M.gt.0)) then
        call MURZA   (REMARK,M)
        IE = 0
  100   continue
          IS = IE+1
          IE = min((IE+26),M)
          II = IE-IS+1
C
          call DATUK (LEVELS,IS,IE,M)
          LAST = IE.eq.M
C
          call NEVER (Z,XND(1,IS),IMAGE,ZLOG,N,II,LAST,NO,REMARK,LEVELS)
C
        if(IE.lt.M) goto 100
      end if
C     !END
      call BYE ('JARUL')
C
      return
      end
