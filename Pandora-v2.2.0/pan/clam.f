      subroutine CLAM
     $(XLM,LU,LUT,N,FMULT,XLTIT,DAMP,OPAC,SCAT,BHS,Z,TAUK,XJNU,SCON,
     $ B,TE,FD,KACTB,P,ZL,SL,BL,BHSL,XJNUL,ITS,CNXP,TIN,MOVING,INCDNT,
     $ JNUMTH,FRS,TOTIME,KTRU)
C
C     Rudolf Loeser, 2005 Mar 25
C---- Supervises the printing of JNU and CSF, for DEMETER.
C     (This is version 3 of CLAM.)
C
C     See also TERMES and SHARON.
C     !DASH
      save
C     !DASH
      real*8 B, BHS, BHSL, BL, CNXP, DAMP, FD, FMULT, FRS, OPAC, P,
     $       SCAT, SCON, SL, TAUK, TE, TIN, TOTIME, XJNU, XJNUL, XLM,
     $       XLTIT, Z, ZL
      integer ITS, JNUMTH, KACTB, KTRU, LU, LUT, N
      logical INCDNT, LYM, MOVING
      character HEAD*12, TPOP*3
C     !DASH
      external FRONT, SHOOT, DOODLE, BETTY, DAWDLE, LINER, HI, BYE
C
C               BHS(N), Z(N), TAUK(N), XJNU(N), SCON(N), B(N), SCAT(N),
      dimension BHS(*), Z(*), TAUK(*), XJNU(*), SCON(*), B(*), SCAT(*),
C
C               FD(N), LUT(6), OPAC(N), CNXP(N), TE(N), BHSL(N), BL(N),
     $          FD(*), LUT(*), OPAC(*), CNXP(*), TE(*), BHSL(*), BL(*),
C
C               ZL(N), SL(N), FRS(N), XJNUL(N), P(N)
     $          ZL(*), SL(*), FRS(*), XJNUL(*), P(*)
C
      data LYM,TPOP /.false., '   '/
C
      call HI ('CLAM')
C     !BEG
C---- Print header
      call LINER  (4, LU)
      call FRONT  (LU, XLM, XLTIT, HEAD, KTRU, LYM, TPOP)
C
C---- Print C.S.F.
      call SHOOT  (HEAD, N, Z, OPAC, TAUK, SCAT, XJNU, B, SCON, FD,
     $             BHS, DAMP, KACTB, LUT(3), FMULT, TE, ITS, CNXP,
     $             FRS, P, INCDNT, JNUMTH)
C---- Plot C.S.F.
      call DOODLE (HEAD, N, Z, SCON, B, BHS, XJNU, TAUK, ZL, SL, BL,
     $             BHSL, XJNUL, LUT(6))
C---- Plot optical depth
      call DAWDLE (HEAD, N, Z, OPAC, SCAT, TAUK, ZL, SL, BL, BHSL,
     $             LUT(6))
C
C---- Print timing data
      call BETTY  (LUT(3), LUT(4), LUT(5), LUT(6), MOVING, TIN, TOTIME)
C     !END
      call BYE ('CLAM')
C
      return
      end
