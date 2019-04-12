      subroutine SHARON
     $(XLM,LU,LUT,N,NOPAC,FMULT,XLTIT,DAMP,CAPPA,OPAC,SIGSTR,SCAT,
     $ BHSNUM,BHSDEN,BHS,Z,TAUK,XJNU,SCON,B,TE,FD,KACTO,COPAC,KACTB,
     $ CBHS,P,CSO,CSB,ZL,SL,BL,BHSL,XJNUL,KRESN,T1,TR,S1,SR,TS1,TSR,
     $ SS1,SSR,KISLV,ITS,ISWA,ISWE,CNXP,TIN,MOVING,SPHERE,INCDNT,
     $ JNUMTH,FRS,CL,ALBD,BMULT,TOTIME,KODE,KTRU,LINE)
C
C     Rudolf Loeser, 1978 Apr 08
C---- Supervises the printing of Continuum Data.
C     (This is version 2 of SHARON.)
C
C     See also TERMES and CLAM.
C     !DASH
      save
C     !DASH
      real*8 ALBD, B, BHS, BHSDEN, BHSL, BHSNUM, BL, BMULT, CAPPA, CBHS,
     $       CL, CNXP, COPAC, CSB, CSO, DAMP, FD, FMULT, FRS, OPAC, P,
     $       S1, SCAT, SCON, SIGSTR, SL, SR, SS1, SSR, T1, TAUK, TE,
     $       TIN, TOTIME, TR, TS1, TSR, XJNU, XJNUL, XLM, XLTIT, Z, ZL
      integer ISWA, ISWE, ITS, JNUMTH, KACTB, KACTO, KISLV, KODE, KRESN,
     $        KTRU, LU, LUT, N, NOPAC
      logical GRAFA, GRAFE, INCDNT, LINE, LYM, MOVING, SPHERE
      character HEAD*12, PLTID*1, TPOP*3
C     !DASH
      external FRONT, SHIRE, SHEAR, SHOOT, BARE, DEBBIE, DOODLE, BETTY,
     $         DAWDLE, SHARE, HI, BYE
C
C               LUT(6), OPAC(N), SCAT(N), BHSNUM(N), BHSDEN(N), BHS(N),
      dimension LUT(*), OPAC(*), SCAT(*), BHSNUM(*), BHSDEN(*), BHS(*),
C
C               ZL(N), SL(N), BL(N), BHSL(N), XJNUL(N), FD(N), ALBD(N),
     $          ZL(*), SL(*), BL(*), BHSL(*), XJNUL(*), FD(*), ALBD(*),
C
C               CL(N,Nopac), FRS(N), SS1(N), SSR(N), ISWA(Nopac), Z(N),
     $          CL(*),       FRS(*), SS1(*), SSR(*), ISWA(*),     Z(*),
C
C               TAUK(N), XJNU(N), SCON(N), TE(N), COPAC(Nopac,N), B(N),
     $          TAUK(*), XJNU(*), SCON(*), TE(*), COPAC(*),       B(*),
C
C               CSO(Nopac,N), CSB(Nopac,N), ISWE(Nopac), CBHS(Nopac,N),
     $          CSO(*),       CSB(*),       ISWE(*),     CBHS(*),
C
C               T1(N), TR(N), S1(N), SR(N), CNXP(N), TS1(N), SIGSTR(N),
     $          T1(*), TR(*), S1(*), SR(*), CNXP(*), TS1(*), SIGSTR(*),
C
C               P(N), TSR(N), CAPPA(N)
     $          P(*), TSR(*), CAPPA(*)
C
      dimension PLTID(4)
C     !EJECT
C
      call HI ('SHARON')
C     !BEG
C---- Set up for Lyman context (if needed)
      call SHARE  (KODE, KRESN, LYM, TPOP)
C---- Print header
      call FRONT  (LU, XLM, XLTIT, HEAD, KTRU, LYM, TPOP)
C---- Set up plot codes
      call BARE   (LUT(4), LUT(5), COPAC, CBHS, Z, TAUK, ZL, PLTID)
C
C---- Print absorbers
      call SHIRE  (LUT(1), LUT(4), FMULT, BMULT, LYM, KISLV, TPOP,
     $             COPAC, CSO, OPAC, CAPPA, SIGSTR, ALBD, T1, TR, TS1,
     $             TSR, HEAD, LINE, ISWA, PLTID, NOPAC, N, GRAFA)
C---- Print emitters
      call SHEAR  (LUT(2), LUT(5), FMULT, LYM, KISLV, TPOP, CBHS,
     $             CSB, BHSNUM, S1, SR, SS1, SSR, HEAD, BHSDEN, B,
     $             BHS, ISWE, PLTID, NOPAC, N, GRAFE)
C---- Print C.S.F.
      call SHOOT  (HEAD, N, Z, OPAC, TAUK, SCAT, XJNU, B, SCON, FD,
     $             BHS, DAMP, KACTB, LUT(3), FMULT, TE, ITS, CNXP,
     $             FRS, P, INCDNT, JNUMTH)
C---- Plot absorbers
      call DEBBIE (GRAFA, HEAD, N, NOPAC, Z, CSO, LUT(4), ZL, CL,
     $             'Absorbers', TAUK, LYM, TS1, TSR, PLTID)
C---- Plot emitters
      call DEBBIE (GRAFE, HEAD, N, NOPAC, Z, CSB, LUT(5), ZL, CL,
     $             'Emitters ', TAUK, LYM, SS1, SSR, PLTID)
C---- Plot C.S.F.
      call DOODLE (HEAD, N, Z, SCON, B, BHS, XJNU, TAUK, ZL, SL, BL,
     $             BHSL, XJNUL, LUT(6))
C---- Plot optical depth
      call DAWDLE (HEAD, N, Z, OPAC, SCAT, TAUK, ZL, SL, BL, BHSL,
     $             LUT(6))
C
C---- Print timing data
      call BETTY  (LUT(3), LUT(4), LUT(5), LUT(6), MOVING, TIN,
     $             TOTIME)
C     !END
      call BYE ('SHARON')
C
      return
      end
