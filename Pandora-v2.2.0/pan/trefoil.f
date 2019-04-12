      subroutine TREFOIL
     $(N,XA,RR,WN,XM,W,IW,XLM,DUMP,XJNU,KODE)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Computes Jnu by Matrix method, for CRANE.
C     Returns with KODE=1 if all seems OK, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 RR, W, WN, XA, XJNU, XLM, XM
      integer IW, KODE, N
      logical DUMP
      character HEAD*80
C     !DASH
      external FOXTAIL, MIMBLE, ENGINE, BEEBALM, HI, BYE
C
      dimension W(*), IW(*)
C
C               XA(N), RR(N), WN(N,N), XM(N,N), XJNU(N)
      dimension XA(*), RR(*), WN(*),   XM(*),   XJNU(*)
C
      call HI ('TREFOIL')
C     !BEG
C---- Compute matrix.
      call FOXTAIL   (N, XA, WN, XM)
C
C---- Invert (and print ?) matrix.
      call MIMBLE    (XLM, HEAD)
      call ENGINE    (XM, W, IW, N, HEAD, DUMP, KODE)
C
      if(KODE.eq.1) then
C----   Compute Jnu.
        call BEEBALM (N, XM, RR, XJNU)
      end if
C     !END
      call BYE ('TREFOIL')
C
      return
      end
