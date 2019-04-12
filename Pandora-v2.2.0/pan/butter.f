      subroutine BUTTER
     $(NO,N,SHE,HEK,BETA,SHE2,HE2K,HE1,HE21,IBTSW)
C
C     Rudolf Loeser, 2001 Dec 18
C---- Prints Helium data, for MEULAN.
C     (This is version 3 of BUTTER.)
C     !DASH
      save
C     !DASH
      real*8 BETA, HE1, HE21, HE2K, HEK, R1, R2, SHE, SHE2
      integer I, IBTSW, LNDX, N, NO
      character LAB*14
C     !DASH
      external  LINER, SHIM, HI, BYE
      intrinsic min, max
C
C               BETA(N), SHE2(N), HE2K(N), HE21(N), SHE(N), HEK(N),
      dimension BETA(*), SHE2(*), HE2K(*), HE21(*), SHE(*), HEK(*),
C
C               HE1(N)
     $          HE1(*)
C
      dimension LAB(3)
C
      data LAB /'(HEK + SHE2)/2', 'HEK           ', 'SHE2          '/
C
      call HI ('BUTTER')
C     !BEG
      LNDX = min(max(IBTSW,0),2)+1
      write (NO,100) IBTSW,LAB(LNDX)
  100 format(' ','     "beta"     = total HeII, and can be either ',
     $           'HEK, SHE2, or the average of the two, depending on ',
     $           'IBETSW.'/
     $       ' ','                  In this run IBETSW =',I2,
     $           ', therefore "beta" = ',A/
     $       ' ','     HeI  ratio = HeI (level 1) / SHE'/
     $       ' ','     HeII ratio = HeII(level 1) / SHE2'//
     $       ' ',7X,'HeI(total)',8X,'HeI(cont)',24X,'HeII(total)',
     $           7X,'HeII(cont)',7X,'HeI ratio',7X,'HeII ratio'/
     $       ' ',9X,'"alpha"',28X,'"beta"',27X,'"gamma"'/
     $       ' ',11X,'SHE',14X,'HEK',14X,'BETA',13X,'SHE2',
     $           12X,'HE2K',12X,'HE1/SHE',9X,'HE21/SHE2')
      call LINER  (1,NO)
      do 102 I = 1,N
        R1 = HE1(I)/SHE(I)
        R2 = HE21(I)/SHE2(I)
        write (NO,101) I,SHE(I),HEK(I),BETA(I),SHE2(I),HE2K(I),R1,R2
  101   format(' ',I4,1P7E17.10)
        call SHIM (I,5,NO)
  102 continue
C     !END
      call BYE ('BUTTER')
C
      return
      end
