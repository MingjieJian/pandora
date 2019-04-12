      subroutine GODEMAR
     $(N,LG,XLM,XA,WN,RR,XJNU,XM,KODE,CI,W,IW,DUMP)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Computes XJNU by matrix method, for angle-dependent
C     Continuum Calculations.
C     (See also ATHENA.)
C     (This is version 2 of GODEMAR.)
C     !DASH
      save
C     !DASH
      real*8 CI, RR, W, WN, XA, XJNU, XLM, XM
      integer IW, KODE, LG, N
      logical DUMP
      character HEAD*80
C     !DASH
      external NOTIUM, MIMBLE, ENGINE, BEEBALM, HI, BYE
C
      dimension W(*), IW(*)
C
C               XA(N,N,LG), WN(N,N,LG), XJNU(N), XM(N,N), CI(N,LG),
      dimension XA(*),      WN(*),      XJNU(*), XM(*),   CI(*),
C
C               RR(N)
     $          RR(*)
C
      call HI ('GODEMAR')
C     !BEG
C---- Compute matrix
      call NOTIUM    (N, LG, CI, XA, WN, XM)
C
C---- Invert (and print ?) matrix
      call MIMBLE    (XLM, HEAD)
      call ENGINE    (XM, W, IW, N, HEAD, DUMP, KODE)
C
      if(KODE.eq.1) then
C----   Compute XJNU
        call BEEBALM (N, XM, RR, XJNU)
      end if
C     !END
      call BYE ('GODEMAR')
C
      return
      end
