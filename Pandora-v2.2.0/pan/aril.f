      subroutine ARIL
     $(X,KURIN,LU,KURU,N,KNW,NKA,LWNT,WT,STEP,TE,XNE,GD,CURMI,CURMA,
     $ INDEX,KNT,ALB,WAVEK,ARRK,KUDNT,FKUR,ARRL,WAVE,HND,ELABD)
C
C     Rudolf Loeser, 1973 Jul 24
C---- Preprocesses the Kurucz opacity data.
C     !DASH
      save
C     !DASH
      real*8 ALB, ARRK, ARRL, CURMA, CURMI, ELABD, FKUR, GD, HND, STEP,
     $       TE, WAVE, WAVEK, WT, X, XNE
      integer I, INDEX, KNT, KNW, KUDNT, KURIN, KURU, LU, LUEO, LWNT, N,
     $        NKA
      logical KILROY
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external GUSHER, KINDLE, TANYA, LACK, SUE, DULCIMR, CHECKER,
     $         HI, BYE
C
      dimension X(*)
C
C               WT(10), HND(N), TE(N), XNE(N), ELABD(N), GD(N), ALB(N),
      dimension WT(*),  HND(*), TE(*), XNE(*), ELABD(*), GD(*), ALB(*),
C
C               WAVEK(KNW), ARRK(KNW,N), INDEX(KNTKU), ARRL(KNW,KNTKU),
     $          WAVEK(*),   ARRK(*),     INDEX(*),     ARRL(*),
C
C               STEP(990), FKUR(KNW)
     $          STEP(*),   FKUR(*)
C
      data KILROY /.true./
C
      call HI ('ARIL')
C     !BEG
C---- Compute GD, the gas density
      call GUSHER   (X, N, GD, ELABD)
C---- Loop over all wavelengths
      do 100 I = 1,KNW
C----   Read and dump raw data
        call KINDLE (KURU, KILROY, WAVEK(I), WT, STEP, LU, I, KUDNT)
C----   Interpolate to depth
        call TANYA  (STEP, KURIN, N, TE, XNE, GD, I, KNW, ARRK, FKUR)
  100 continue
C---- Return data file
      call LACK     (KURU, LUEO)
C---- Print array
      call SUE      (LU, CURMI, CURMA, ALB, WAVEK, TE, XNE, HND, GD,
     $               ARRK, KURIN, FKUR, WAVE, N, NKA, KNW, LWNT)
C---- Plot array
      call DULCIMR  (LU, CURMI, CURMA, INDEX, KNT, ARRK, ARRL, WAVEK,
     $               'statistical', KNW)
C---- Checksum
      call CHECKER  (ARRK, 1, (KNW*N), 'Statistical Line Opacity')
C     !END
      call BYE ('ARIL')
C
      return
      end
