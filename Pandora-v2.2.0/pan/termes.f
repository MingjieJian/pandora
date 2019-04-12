      subroutine TERMES
     $(XLM,LU,LUT,N,NOPAC,FMULT,XLTIT,CAPPA,SIGSTR,OPAC,BHSNUM,BHSDEN,
     $ BHS,Z,TAUK,B,COPAC,CBHS,CSO,CSB,ZL,ISWA,ISWE,TIN,MOVING,CL,
     $ ALBD,BMULT,TOTIME,KTRU,LINE)
C
C     Rudolf Loeser, 2005 Mar 23
C---- Supervises the printing of absorbers and emitters, for DION.
C     (This is version 2 of TERMES.)
C
C     See also SHARON and CLAM.
C     !DASH
      save
C     !DASH
      real*8 ALBD, B, BHS, BHSDEN, BHSNUM, BMULT, CAPPA, CBHS, CL,
     $       COPAC, CSB, CSO, FMULT, OPAC, SIGSTR, TAUK, TIN, TOTIME,
     $       XLM, XLTIT, Z, ZL, dummy
      integer ISWA, ISWE, KTRU, LU, LUT, N, NOPAC, jummy
      logical GRAFA, GRAFE, LINE, LYM, MOVING
      character HEAD*12, PLTID*1, TPOP*3
C     !DASH
C     !EJECT
      external FRONT, SHIRE, SHEAR, BARE, DEBBIE, BETTY, ABJECT,
     $         HI, BYE
C
C               LUT(6), OPAC(N), BHSNUM(N), BHSDEN(N), BHS(N), ALBD(N),
      dimension LUT(*), OPAC(*), BHSNUM(*), BHSDEN(*), BHS(*), ALBD(*),
C
C               COPAC(Nopac,N), CBHS(Nopac,N), Z(N), CL(N,Nopac), B(N),
     $          COPAC(*),       CBHS(*),       Z(*), CL(*),       B(*),
C
C               TAUK(N), CSO(Nopac,N), ISWA(Nopac), ISWE(Nopac), ZL(N),
     $          TAUK(*), CSO(*),       ISWA(*),     ISWE(*),     ZL(*),
C
C               CSB(Nopac,N), SIGSTR(N), CAPPA(N)
     $          CSB(*),       SIGSTR(*), CAPPA(*)
C
      dimension PLTID(4)
C
      data LYM,TPOP /.false., '   '/
C
      call HI ('TERMES')
C     !BEG
C---- Print header
      call ABJECT (LU)
      call FRONT  (LU, XLM, XLTIT, HEAD, KTRU, LYM, TPOP)
C---- Set up plot codes
      call BARE   (LUT(4), LUT(5), COPAC, CBHS, Z, TAUK, ZL, PLTID)
C
C---- Print absorbers
      call SHIRE  (LUT(1), LUT(4), FMULT, BMULT, LYM, jummy, TPOP,
     $             COPAC, CSO, OPAC, CAPPA, SIGSTR, ALBD, dummy,
     $             dummy, dummy, dummy, HEAD, LINE, ISWA, PLTID,
     $             NOPAC, N, GRAFA)
C---- Print emitters
      call SHEAR  (LUT(2), LUT(5), FMULT, LYM, jummy, TPOP, CBHS, CSB,
     $             BHSNUM, dummy, dummy, dummy, dummy, HEAD, BHSDEN,
     $             B, BHS, ISWE, PLTID, NOPAC, N, GRAFE)
C---- Plot absorbers
      call DEBBIE (GRAFA, HEAD, N, NOPAC, Z, CSO, LUT(4), ZL, CL,
     $             'Absorbers', TAUK, LYM, dummy, dummy, PLTID)
C---- Plot emitters
      call DEBBIE (GRAFE, HEAD, N, NOPAC, Z, CSB, LUT(5), ZL, CL,
     $             'Emitters ', TAUK, LYM, dummy, dummy, PLTID)
C
C---- Print timing data
      call BETTY  (LUT(3), LUT(4), LUT(5), LUT(6), MOVING, TIN,
     $             TOTIME)
C     !END
      call BYE ('TERMES')
C
      return
      end
