      subroutine ATHENA
     $(N,XLM,NSHL,CSHL,XASHL,WNSHL,MRR,CDSK,XADSK,WNDSK,RR,XJNU,XM,KODE,
     $ W,IW,DUMP)
C
C     Rudolf Loeser, 1982 Feb 09
C---- Computes XJNU by matrix method, for angle-dependent
C     Continuum Calculations.
C     (See also GODEMAR.)
C     !DASH
      save
C     !DASH
      real*8 CDSK, CSHL, RR, W, WNDSK, WNSHL, XADSK, XASHL, XJNU, XLM,
     $       XM
      integer IW, KODE, MRR, N, NSHL
      logical DUMP
      character HEAD*80
C     !DASH
      external HESTIA, MIMBLE, ENGINE, BEEBALM, HI, BYE
C
      dimension W(*), IW(*)
C
C               CSHL(N,NSHL), XADSK(N,N,MRR), WNSHL(N,N,NSHL), XJNU(N),
      dimension CSHL(*),      XADSK(*),       WNSHL(*),        XJNU(*),
C
C               WNDSK(N,N,MRR), XM(N,N), XASHL(N,N,NSHL), CDSK(N,MRR),
     $          WNDSK(*),       XM(*),   XASHL(*),       CDSK(*),
C
C               RR(N)
     $          RR(*)
C
      call HI ('ATHENA')
C     !BEG
C---- Compute matrix
      call HESTIA    (N, NSHL, CSHL, XASHL, WNSHL, MRR, CDSK, XADSK,
     $                WNDSK, XM)
C
C---- Invert (and print ?) matrix.
      call MIMBLE    (XLM, HEAD)
      call ENGINE    (XM, W, IW, N, HEAD, DUMP, KODE)
C
      if(KODE.eq.1) then
C----   Compute XJNU
        call BEEBALM (N, XM, RR, XJNU)
      end if
C     !END
      call BYE ('ATHENA')
C
      return
      end
