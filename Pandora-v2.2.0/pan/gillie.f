      subroutine GILLIE
     $(NO,N,NRAD,Z,BDIJ,TAUIJ,LABIJ,DLOG,ZLOG,IMAGE)
C
C     Rudolf Loeser, 1990 Oct 16
C---- Controls production of plots
C     of BDIJ (ratios of departure coefficients) and TAU scales vs. Z
C     !DASH
      save
C     !DASH
      real*8 BDIJ, DLOG, TAUIJ, Z, ZLOG
      integer IE, INC, IS, LABIJ, N, NO, NRAD
      character IMAGE*(*), REMARK*8
C     !DASH
      external  MURZA, PLINK, HI, BYE
      intrinsic min
C
C               Z(N), BDIJ(N,NL), DLOG(N,NRAD), TAUIJ(N,NRAD), ZLOG(N),
      dimension Z(*), BDIJ(*),    DLOG(*),      TAUIJ(N,*),    ZLOG(*),
C
C               LABIJ(NRAD)
     $          LABIJ(*)
C
      call HI ('GILLIE')
C     !BEG
      if((NO.gt.0).and.(NRAD.gt.0)) then
        call MURZA   (REMARK,NRAD)
C
        IE = 0
  100   continue
          IS  = IE+1
          IE  = min((IE+26),NRAD)
          INC = IE-IS+1
C
          call PLINK (NO,N,INC,Z,BDIJ,TAUIJ(1,IS),LABIJ(IS),DLOG,ZLOG,
     $                IMAGE,REMARK)
C
        if(IE.lt.NRAD) goto 100
      end if
C     !END
      call BYE ('GILLIE')
C
      return
      end
