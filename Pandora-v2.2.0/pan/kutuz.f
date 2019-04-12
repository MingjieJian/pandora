      subroutine KUTUZ
     $(NO,N,SHE,BETA,HE2K,HEND,RHEAB,PNF,BETAR,DZB)
C
C     Rudolf Loeser, 2001 Dec 18
C---- Prints Helium data, for MEULAN.
C     (This is version 2 of KUTUZ.)
C     !DASH
      save
C     !DASH
      real*8 BETA, BETAR, CH, DZB, HE2K, HEND, PNF, RHEAB, SHE
      integer I, N, NO
C     !DASH
      external LINER, SHIM, HI, BYE
C
C               RHEAB(N), BETAR(N), SHE(N), BETA(N), HE2K(N), HEND(N),
      dimension RHEAB(*), BETAR(*), SHE(*), BETA(*), HE2K(*), HEND(*),
C
C               PNF(N), DZB(N)
     $          PNF(*), DZB(*)
C
      call HI ('KUTUZ')
C     !BEG
      write (NO,100)
  100 format(' ','     HEND(i) = ABD(He) * RHEAB(i) * HND(i)'/
     $       ' ','     check   = ("alpha" + "beta" + "gamma") / HEND'/
     $       ' ','     PNF     = cumulative He Populations ',
     $           'Normalization Factor'/
     $       ' ','     BETAR   = "beta" ratio = "beta" / HEND'//
     $       ' ',10X,'check',9X,'total Helium',6X,'abun. ratio',
     $           22X,'"beta" ratio',5X,'d(BETAR)/d(Z)'/
     $       ' ',28X,'HEND',12X,'RHEAB',13X,'PNF',13X,'BETAR',13X,'DZB')
      call LINER  (1,NO)
      do 102 I = 1,N
        CH = (SHE(I)+BETA(I)+HE2K(I))/HEND(I)
        write (NO,101) I,CH,HEND(I),RHEAB(I),PNF(I),BETAR(I),DZB(I)
  101   format(' ',I4,1P6E17.10)
        call SHIM (I,5,NO)
  102 continue
C     !END
      call BYE ('KUTUZ')
C
      return
      end
