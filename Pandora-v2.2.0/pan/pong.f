      subroutine PONG
     $(NO,N,Z,HND,XNE,PGS,SUM,PTO,GD,T5,GMASS,TE,VT,VM,DGM,PMG,TKIN,
     $ HELABD,ABS,VEC,KASE,LASE,IPTO)
C
C     Rudolf Loeser, 1980 Nov 07
C---- Prints gas parameters.
C     (This is version 2 of PONG.)
C     !DASH
      save
C     !DASH
      real*8 ABS, DGM, GD, GMASS, HELABD, HND, PGS, PMG, PTO, SUM, T5,
     $       TE, TKIN, VEC, VM, VT, XNE, Z
      integer I, IPTO, KASE, LASE, N, NO
      logical KT5
      character SIGNAL*1, TAT*11, TIT*6
C     !DASH
      external MOVE1, NAUGHTD, LINER, YOLK, SYLVIA, PRIVET, SHIM, FOLK,
     $         VECOUT, ECKE, HI, BYE
C
C               ABS(N), HND(N), XNE(N), PGS(N), VM(N), PTO(N), TKIN(N),
      dimension ABS(*), HND(*), XNE(*), PGS(*), VM(*), PTO(*), TKIN(*),
C
C               HELABD(N), SUM(N), GMASS(N), TE(N), VT(N), GD(N), Z(N),
     $          HELABD(*), SUM(*), GMASS(*), TE(*), VT(*), GD(*), Z(*),
C
C               T5(N), IPTO(N), DGM(N), VEC(N), PMG(N)
     $          T5(*), IPTO(*), DGM(*), VEC(*), PMG(*)
C
      dimension SIGNAL(3), TIT(2), TAT(4)
C
      data SIGNAL /'_', ' ', '^'/
C
      call HI ('PONG')
C     !BEG
      if(NO.gt.0) then
C
        call FOLK    (KASE, N, TKIN, Z, ABS, TIT)
        call YOLK    (N, DGM, VT, PMG, VEC, TAT)
        call ECKE    (PTO, N, IPTO)
        call NAUGHTD (T5, 1, N, KT5)
        call SYLVIA  (NO, KT5, LASE)
C     !EJECT
        write (NO,100) TAT(1),TAT(2),TIT(1),TAT(3),TIT(2),TAT(4)
  100   format(' ',20X,'Mass of',13X,A9,3X,'Outward',28X,'Total',
     $             5X,'Neutral'/
     $         ' ',22X,'gas',15X,A9,2X,'mass-motion',4X,'Gas',
     $             7X,'Total',4X,'Hydrogen',3X,'Hydrogen',
     $             3X,'Electron',6X,'Mass'/
     $         ' ',9X,A6,6X,'column',2X,'Temperature',A9,
     $             3X,'velocity',3X,'pressure',3X,'pressure',
     $             4(4X,'density')//
     $         ' ',9X,A6,4X,'(g/cm**2)',5X,'(K)',3X,A11,3X,'(km/s)',
     $             2X,'(dyn/cm**2)','(dyn/cm**2)',2X,'(/cm**3)',
     $             2(3X,'(/cm**3)'),2X,'(g/cm**3)')
        call LINER    (2, NO)
C
        do 102 I = 1,N
          write (NO,101) I,ABS(I),GMASS(I),TE(I),VEC(I),-VM(I),PGS(I),
     $                     PTO(I),SIGNAL(IPTO(I)+2),HND(I),SUM(I),
     $                     XNE(I),GD(I)
  101     format(' ',I4,1PE13.5,6E11.3,A1,E10.3,3E11.3)
          call SHIM   (I, 5, NO)
  102   continue
C
        call LINER    (1, NO)
        write (NO,103)
  103   format(' ',19X,'Mass of gas column = (total pressure - pmag1)',
     $             ' / gravity,'/
     $         ' ',40X,'where pmag1 is the magnetic pressure at the',
     $             ' first depth')
C
        call LINER    (2, NO)
        write (NO,104)
  104   format(' ','Total Helium abundance (relative to total ',
     $             'Hydrogen)')
        call PRIVET   (NO, HELABD, N)
C
        if(.not.KT5) then
          call VECOUT (NO, T5, N, 'Continuum TAU-5000')
        end if
C
      end if
C     !END
      call BYE ('PONG')
C
      return
      end
