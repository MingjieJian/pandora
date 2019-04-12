      subroutine GAHOL
     $(NO,N,NRAD,IMAGE,TAUIJ,LABIJ,TE)
C
C     Rudolf Loeser, 1990 Oct 16
C---- Controls production of plots of TE vs. TAU scales.
C     !DASH
      save
C     !DASH
      real*8 TAUIJ, TE
      integer IE, II, IS, LABIJ, N, NO, NRAD
      character IMAGE*(*), REMARK*8
C     !DASH
      external  MURZA, DITHER, HI, BYE
      intrinsic min
C
C               TAUIJ(N,NRAD), TE(N), LABIJ(NRAD)
      dimension TAUIJ(N,*),    TE(*), LABIJ(*)
C
      call HI ('GAHOL')
C     !BEG
      if((NO.gt.0).and.(NRAD.gt.0)) then
        call MURZA    (REMARK,NRAD)
        IE = 0
  100   continue
          IS = IE+1
          IE = min((IE+26),NRAD)
          II = IE-IS+1
C
          call DITHER (NO,N,II,IMAGE,TAUIJ(1,IS),LABIJ(IS),TE,REMARK)
C
        if(IE.lt.NRAD) goto 100
      end if
C     !END
      call BYE ('GAHOL')
C
      return
      end
