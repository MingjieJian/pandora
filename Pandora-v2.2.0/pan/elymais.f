      subroutine ELYMAIS
     $(KODNT,NP,TABP,NT,TABT,NV,TABV,N,C01,C02,C03,C04,C05,C06,
     $ IPJ,ITJ,IVJ,NCP,INWVC,WAVCO,DUMP,CALLER)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Dumps, for AMBRO.
C     !DASH
      save
C     !DASH
      real*8 C01, C02, C03, C04, C05, C06, TABP, TABT, TABV, WAVCO
      integer I, INWVC, IPJ, ITJ, IVJ, KODNT, LUEO, N, NCP, NP, NT, NV
      logical DUMP
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MESHED, VECOUT, LINER, HI, BYE
C
C               C01(N), C02(N), C03(N), C04(N), C05(N), C06(N), IPJ(N),
      dimension C01(*), C02(*), C03(*), C04(*), C05(*), C06(*), IPJ(*),
C
C               TABP(NP), TABT(NT), TABV(NV), WAVCO(NCP), INWVC(NCP),
     $          TABP(*),  TABT(*),  TABV(*),  WAVCO(*),   INWVC(*),
C
C               ITJ(N), IVJ(N)
     $          ITJ(*), IVJ(*)
C
      call HI ('ELYMAIS')
C     !BEG
      DUMP = KODNT.gt.0
C
      if(DUMP) then
        call MESHED (CALLER, 2)
        write (LUEO,100)
  100   format(' ','Dump of Composite Line Opacity data.')
C
        call VECOUT (LUEO, TABP, NP, 'Grid of log(P)')
        call VECOUT (LUEO, TABT, NT, 'Grid of log(TE)')
        call VECOUT (LUEO, TABV, NV ,'Grid of V')
C
        call LINER  (2, LUEO)
        write (LUEO,101)
  101   format(' ','Wavelengths and corresponding Data Record index.')
        call LINER  (1, LUEO)
        write (LUEO,102) (I,WAVCO(I),INWVC(I),I=1,NCP)
  102   format(5(' ',I5,F12.5,'(',I5,')'))
C
        call LINER  (2, LUEO)
        write (LUEO,103)
  103   format(' ','Interpolation data, as defined by Kurucz.'//
     $         ' ',12X,'C01',9X,'C02',9X,'C03',9X,'C04',9X,'C05',9X,
     $             'C06',3X,'IPJ',3X,'ITJ',3X,'IVJ')
        call LINER  (1, LUEO)
        write (LUEO,104) (I,C01(I),C02(I),C03(I),C04(I),C05(I),C06(I),
     $                      IPJ(I),ITJ(I),IVJ(I),I=1,N)
  104   format(5(' ',I3,6F12.6,3I6/))
      end if
C     !END
      call BYE ('ELYMAIS')
C
      return
      end
