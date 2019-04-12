      subroutine RUSAS
     $(LU,TE,XNE,HND,H2N,DEN,PGS,ALB,V,WAVCO,ARRCO,IPJ,ITJ,IVJ,N,NCP,
     $ MULT,NAB,KMULT,FABD,NKA,WAVE,LWNT)
C     Rudolf Loeser, 1983 Jul 05
C---- Prints, for AMBRO.
C     !DASH
      save
C     !DASH
      real*8 ALB, ARRCO, DEN, FABD, H2N, HND, PGS, TE, V, WAVCO, WAVE,
     $       XNE
      integer I, IPJ, ITJ, IVJ, J, JE, JS, K, KMULT, LU, LWNT, MULT, N,
     $        NAB, NCP, NKA
C     !DASH
      external  PRIAM, LINER, DEGEN, ACORUS, SHIM, HI, BYE
      intrinsic min
C
C               V(N), IVJ(N), TE(N), XNE(N), HND(N), H2N(N), MULT(NAB),
      dimension V(*), IVJ(*), TE(*), XNE(*), HND(*), H2N(*), MULT(*),
C
C               PGS(N), ALB(N), WAVCO(NCP), ARRCO(NCP,N), ITJ(N),
     $          PGS(*), ALB(*), WAVCO(*),   ARRCO(NCP,*), ITJ(*),
C
C               DEN(N), IPJ(N)
     $          DEN(*), IPJ(*)
C
      call HI ('RUSAS')
C     !BEG
      if(LU.gt.0) then
        call PRIAM  (LU, 'COMPOSITE', 9)
        call LINER  (1,LU)
        write (LU,100) LWNT
  100   format(' ','Kurucz''s Composite Line Opacity data',10X,
     $             '(LWNT=',I5,')')
        call DEGEN  (LU, KMULT, MULT, NAB, FABD)
        call ACORUS (LU, NKA, WAVE)
C     !EJECT
        JE = 0
  102   continue
          JS = JE+1
          JE = min(JE+9,N)
          call LINER  (2, LU)
          write (LU,103) '       Depth',(J,J=JS,JE)
  103     format(' ',A12,9I12)
          call LINER  (1, LU)
  104     format(' ',A12,1P9E12.5)
          write (LU,104) 'NE          ',(XNE(J),J=JS,JE)
          write (LU,104) 'NH          ',(HND(J),J=JS,JE)
          write (LU,104) 'NH2         ',(H2N(J),J=JS,JE)
          write (LU,104) 'Gas Dens.   ',(DEN(J),J=JS,JE)
          call LINER  (1, LU)
          write (LU,104) 'Gas Press.  ',(PGS(J),J=JS,JE)
          write (LU,103) 'P-Grid index',(IPJ(J),J=JS,JE)
          call LINER  (1, LU)
          write (LU,104) 'TE          ',(TE (J),J=JS,JE)
          write (LU,103) 'T-Grid index',(ITJ(J),J=JS,JE)
          call LINER  (1, LU)
          write (LU,104) 'V           ',(V  (J),J=JS,JE)
          write (LU,103) 'V-Grid index',(IVJ(J),J=JS,JE)
          call LINER  (1, LU)
          write (LU,104) 'Albedo      ',(ALB(J),J=JS,JE)
          call LINER  (1, LU)
          write (LU,105)
  105     format(' ','Wavelength',3X,'O P A C I T I E S')
          call LINER  (1, LU)
C
          K = 0
          do 107 I = 1,NCP,LWNT
            K = K+1
            write (LU,106) WAVCO(I),(ARRCO(I,J),J=JS,JE)
  106       format(' ',1PE11.5,1X,9E12.5)
            call SHIM (K, 5, LU)
  107    continue
C
        if(JE.lt.N) goto 102
      end if
C     !END
      call BYE ('RUSAS')
C
      return
      end
