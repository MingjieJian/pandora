      subroutine ZAMORIN
     $(Z,BDI,TAUIJ,LABIJ,NRAD,IMAGE,BLOG,ZLOG,N,NL,NO)
C
C     Rudolf Loeser, 1990 Oct 16
C---- Controls production of plots of BDI (departure coefficients) and
C     TAU scales vs. Z
C     !DASH
      save
C     !DASH
      real*8 BDI, BLOG, TAUIJ, Z, ZLOG
      integer IE, II, IS, JE, JJ, JS, LABIJ, N, NL, NO, NRAD
      character IMAGE*(*), LEVELS*20, REMARK*8
C     !DASH
      external  MURZA, DATUK, YEAH, HI, BYE
      intrinsic min, max
C
C               Z(N), BDI(N,NL), TAUIJ(N,NRAD), LABIJ(NRAD), BLOG(N,NL),
      dimension Z(*), BDI(N,*),  TAUIJ(N,*),    LABIJ(*),    BLOG(*),
C
C               ZLOG(N)
     $          ZLOG(*)
C
      call HI ('ZAMORIN')
C     !BEG
      if((NO.gt.0).and.(NRAD.gt.0)) then
        call MURZA    (REMARK,max(NL,NRAD))
C
        IE = 0
  100   continue
          IS = IE+1
          IE = min((IE+26),NL)
          II = IE-IS+1
C
          call DATUK  (LEVELS,IS,IE,NL)
C
          JE = 0
  101     continue
            JS = JE+1
            JE = min((JE+26),NRAD)
            JJ = JE-JS+1
C
            call YEAH (Z,BDI(1,IS),TAUIJ(1,JS),LABIJ(JS),JJ,IMAGE,BLOG,
     $                 ZLOG,N,II,NO,REMARK,LEVELS)
C
          if(JE.lt.NRAD) goto 101
C
        if(IE.lt.NL) goto 100
      end if
C     !END
      call BYE ('ZAMORIN')
C
      return
      end
