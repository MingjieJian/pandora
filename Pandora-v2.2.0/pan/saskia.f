      subroutine SASKIA
     $(X,W,IW,DUMP,T,R,B,S,XJNU,N,XM,WN,WH,MOVING,ILFLX,CNDT,IQINC,IMG,
     $ CNXP,YDAMP,BB,CQ,ITS,XLM,CSFCRIT,Z,OPAC)
C
C     Rudolf Loeser, 1980 Nov 04
C---- Computes Intensity (XJNU) from Continuum Source function,
C     pertaining to the reduced depth table.
C     (This is version 3 of SASKIA.)
C     !DASH
      save
C     !DASH
      real*8 B, BB, CNDT, CNXP, CQ, CSFCRIT, OPAC, R, S, T, W, WH, WN,
     $       X, XJNU, XLM, XM, YDAMP, Z
      integer IFLG, ILFLX, IMG, IQINC, ITS, IW, JFLG, KODE, N
      logical DUMP, MOVING
      character TITLE*100
C     !DASH
      external WHORL, WALL, DERVISH, MOVE1, CECILIA, WHALE, WRIST, WELL,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               CNXP(N), IMG(N), B(N), S(N), XJNU(N), XM(N,N), WN(N,N),
      dimension CNXP(*), IMG(*), B(*), S(*), XJNU(*), XM(*),   WN(*),
C
C               WH(N,N), CNDT(N), Z(N), CQ(N), T(N), BB(N), OPAC(N),
     $          WH(*),   CNDT(*), Z(*), CQ(*), T(*), BB(*), OPAC(*),
C
C               R(N)
     $          R(*)
C
C     !EJECT
C
      call HI ('SASKIA')
C     !BEG
C---- Compute auxiliary function BB.
      call WHORL       (N, B, CNXP, R, IQINC, BB)
      if(DUMP) then
        call WALL      (N, T, R, B, CNDT, BB, CNXP)
      end if
C
C---- Compute weight matrices.
      write (TITLE,100) XLM
  100 format(' ','Weight Matrices for Continuum Calculation; ',
     $           'wavelength =',1PE18.11)
      call DERVISH     (X, W, IW, N, T, OPAC, Z, YDAMP, MOVING, WN, WH,
     $                  ILFLX, TITLE, DUMP, KODE, IMG)
C
C---- Now compute S and XJNU.
      if(KODE.le.0) then
C----   Matrix calculation failed - set XJNU=S=BB.
        call MOVE1     (BB, N, S)
        call MOVE1     (BB, N, XJNU)
      else
C----   Attempt to compute S by iterative method.
        call CECILIA   (N, WN, R, BB, S, CQ, ITS, IFLG, CSFCRIT)
        if(IFLG.le.0) then
C----     Iterative method failed - use matrix method for S.
          call WHALE   (N, R, BB, WN, XM, W, IW, XLM, DUMP, S, JFLG)
          if(JFLG.le.0) then
C----       Matrix method failed - set S=BB.
            call MOVE1 (BB, N, S)
          end if
        end if
C----   Compute XJNU, using S and weight matrix.
        call WRIST     (N, S, CNXP, WN, IQINC, XJNU)
      end if
C
      if(DUMP) then
        call WELL      (N, S, XJNU, KODE)
      end if
C     !END
      call BYE ('SASKIA')
C
      return
      end
