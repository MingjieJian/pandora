      subroutine DICK
     $(NO,N,Z,HNDO,XNEO,XNK,G,H,HNDNEW,XNENEW,HND,XNE,HSEC,NEITER,T5,
     $ NTITER,MEITER,T5S,FS,NTIPR,HEL,HTAU)
C
C     Rudolf Loeser, 1980 Nov 07
C---- Prints output for ZAPPY.
C     !DASH
      save
C     !DASH
      real*8 FS, G, H, HEL, HND, HNDNEW, HNDO, HSEC, HTAU, T5, T5S, XNE,
     $       XNENEW, XNEO, XNK, Z
      integer I, MEITER, N, NEITER, NO, NTIPR, NTITER
C     !DASH
      external LINER, HI, BYE
C
C               Z(N), HND(N), HNDO(N), XNEO(N), G(N), T5(N), HNDNEW(N),
      dimension Z(*), HND(*), HNDO(*), XNEO(*), G(*), T5(*), HNDNEW(*),
C
C               XNENEW(N), XNE(N), T5S(12), XNK(N), FS(12), H(N)
     $          XNENEW(*), XNE(*), T5S(*),  XNK(*), FS(*),  H(*)
C
      call HI ('DICK')
C     !BEG
      if(NO.gt.0) then
        call LINER (3,NO)
        write (NO,100) HEL,HSEC,HTAU
  100   format(' ',72X,'HEL',F6.2,25X,'HSEC',F5.2,3X,'HTAU',F5.2//
     $         ' ',50X,'E L E C T R O N  number density',
     $              3X,'H Y D R O G E N  number density'/
     $         ' ',50X,31('*'),3X,31('*'),3X,'Continuum'/
     $         ' ',13X,'Z',10X,'G',10X,'H',9X,'NP',
     $             2(9X,'Old',8X,'New',3X,'Weighted'),4X,'TAU-5000')
        call LINER (1,NO)
C
        write (NO,101) (I,Z(I),G(I),H(I),XNK(I),XNEO(I),XNENEW(I),
     $                    XNE(I),HNDO(I),HNDNEW(I),HND(I),T5(I),I=1,N)
  101   format(5(' ',I3,1P4E11.2,E12.3,2E11.3,E12.3,2E11.3,E12.3/))
C
        write (NO,102) NEITER,NTITER,MEITER,(FS(I),T5S(I),I=1,NTIPR)
  102   format(' ',I70,I57/
     $         ' ',I70/
     $        (' ',19X,1PE17.8,74X,E17.8))
      end if
C     !END
      call BYE ('DICK')
C
      return
      end
