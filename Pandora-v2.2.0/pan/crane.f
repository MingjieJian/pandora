      subroutine CRANE
     $(X,W,IW,DUMP,T,XA,YA,XJNU,N,XM,WN,WH,MOVING,ILFLX,IQINC,IMG,CNXP,
     $ YDAMP,RR,XJNUO,ITS,XLM,CSFCRIT,Z,OPAC)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Computes reduced intensity (XJNU) directly.
C     !DASH
      save
C     !DASH
      real*8 CNXP, CSFCRIT, OPAC, RR, T, W, WH, WN, X, XA, XJNU, XJNUO,
     $       XLM, XM, YA, YDAMP, Z
      integer ILFLX, IMG, IQINC, ITS, IW, KODE, N
      logical DUMP, MOVING
      character TITLE*100
C     !DASH
      external DERVISH, CRESS, BALL, ERYNGO, ZERO1, ARBUTUS, TREFOIL,
     $         BELL, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               T(N), OPAC(N), Z(N), WN(N,N), WH(N,N), XM(N,N), IMG(N),
      dimension T(*), OPAC(*), Z(*), WN(*),   WH(*),   XM(*),   IMG(*),
C
C               YA(N), CNXP(N), RR(N), XJNUO(N), XJNU(N), XA(N)
     $          YA(*), CNXP(*), RR(*), XJNUO(*), XJNU(*), XA(*)
C     !EJECT
C
      call HI ('CRANE')
C     !BEG
C---- Compute weight matrices.
      write (TITLE,100) XLM
  100 format(' ','Weight Matrices for Continuum Calculation; ',
     $           'wavelength:',1PE18.10)
      call DERVISH      (X, W, IW, N, T, OPAC, Z, YDAMP, MOVING,
     $                   WN, WH, ILFLX, TITLE, DUMP, KODE, IMG)
C
C---- Compute auxiliary function RR
      call CRESS        (N, YA, WN, KODE, CNXP, RR)
      if(DUMP) then
        call BALL       (N, Z, T, XA, YA, CNXP, RR)
      end if
C
C---- Now compute Jnu.
      if(KODE.le.0) then
C----   Matrix calculation failed - set up default Jnu.
        call ERYNGO     (N, RR, XA, XJNU)
      else
C----   Attempt to compute Jnu by iterative method.
        call ZERO1      (XJNUO, N)
        call ARBUTUS    (N, WN, XA, RR, XJNU, XJNUO, ITS, KODE,
     $                   CSFCRIT)
        if(KODE.le.0) then
C----     Iterative method failed - use matrix method for Jnu.
          call TREFOIL  (N, XA, RR, WN, XM, W, IW, XLM, DUMP, XJNU,
     $                   KODE)
          if(KODE.le.0) then
C----       Matrix method failed - set up default Jnu.
            call ERYNGO (N, RR, XA, XJNU)
          end if
        end if
      end if
C
      if(DUMP) then
        call BELL       (N, XJNUO, XJNU)
      end if
C     !END
      call BYE ('CRANE')
C
      return
      end
