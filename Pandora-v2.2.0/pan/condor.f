      subroutine CONDOR
     $(NO,WVL,N,NL,IU,IL,ICE,D,COP,XND,PHI,GTN,TAU,TAUM,VXS,EDINT,
     $ EDTAU,EDTAUM,EDGTO,KTKIN,REFLM,GTO,XKL,XKT,LDL,DDLM,CDLM,LSTMP,
     $ EQUL,SMTH)
C
C     Rudolf Loeser, 1980 Apr 09
C---- Prints, for RAPTURE.
C     !DASH
      save
C     !DASH
      real*8 CDLM, COP, D, DDLM, GTN, GTO, PHI, REFLM, TAU, TAUM, VXS,
     $       WVL, XKL, XKT, XND, ZERO
      integer I, ICE, IL, IU, KODE, KTKIN, LDL, LSTMP, N, NL, NO
      logical BADGTO, EDGTO, EDINT, EDTAU, EDTAUM, EQUL, SMTH, VZERO
      character BLANK*1, SIGNAL*1, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external ARANDA, NAUGHTD, ARISOD, MARKD, SHIM, LINER, HI, BYE
C
C               TAU(N), VXS(N), COP(N), PHI(N), GTN(N), XKT(N), XKL(N),
      dimension TAU(*), VXS(*), COP(*), PHI(*), GTN(*), XKT(*), XKL(*),
C
C               TAUM(N), GTO(N), XND(N,NL), D(N)
     $          TAUM(*), GTO(*), XND(N,*),  D(*)
C
      call HI ('CONDOR')
C     !BEG
      KODE = 1
      if(EDGTO) then
        call ARISOD (GTO, N, GTN, N, ZERO, KODE)
      end if
      BADGTO = KODE.ne.1
C
      call NAUGHTD  (VXS, 1, N, VZERO)
C
      call ARANDA   (NO, N, IU, IL, ICE, WVL, KTKIN, REFLM, EDGTO,
     $               BADGTO, EDINT, EDTAU, EDTAUM, LDL, VZERO, LSTMP,
     $               EQUL, SMTH)
C
      do 101 I = 1,N
        call MARKD  (GTO(I), GTN(I), SIGNAL, BLANK, STAR)
        write (NO,100) I,D(I),VXS(I),XND(I,IU),XND(I,IL),PHI(I),GTO(I),
     $                 SIGNAL,XKL(I),COP(I),XKT(I),TAU(I),TAUM(I)
  100   format(' ',I3,1P,4E10.2,2E11.3,A1,E10.3,2E11.3,E15.7,E14.6)
        call SHIM   (I, 5, NO)
  101 continue
C
      if(LDL.gt.1) then
        call LINER  (1, NO)
        write (NO,102) DDLM,CDLM
  102   format(' ',92X,'DDLM =',1PE15.7/
     $         ' ',92X,'CDLM =',  E15.7)
      end if
C     !END
      call BYE ('CONDOR')
C
      return
      end
