      subroutine TOM
     $(NO,N,HNDO,XNEO,XNK,R,P,HNDNEW,XNENEW,HND,XNE,HSEC,NEITER,T5,
     $ MEITER,HEL,TKIN,NHITER)
C
C     Rudolf Loeser, 1980 Nov 07
C---- Prints output for ZAPPY.
C     !DASH
      save
C     !DASH
      real*8 HEL, HND, HNDNEW, HNDO, HSEC, P, R, T5, TKIN, XNE, XNENEW,
     $       XNEO, XNK
      integer I, MEITER, N, NEITER, NHITER, NO
C     !DASH
      external LINER, HI, BYE
C
C               HNDO(N), XNEO(N), XNK(N), R(N), P(N), HNDNEW(N), T5(N),
      dimension HNDO(*), XNEO(*), XNK(*), R(*), P(*), HNDNEW(*), T5(*),
C
C               HND(N), XNE(N), TKIN(N), XNENEW(N)
     $          HND(*), XNE(*), TKIN(*), XNENEW(*)
C
      call HI ('TOM')
C     !BEG
      if(NO.gt.0) then
C
        call LINER (3,NO)
        write (NO,100) HEL,HSEC
  100   format(' ',72X,'HEL',F6.2,25X,'HSEC',F5.2//
     $         ' ',50X,'E L E C T R O N  number density',
     $              3X,'H Y D R O G E N  number density'/
     $         ' ',50X,31('*'),3X,31('*'),3X,'Continuum'/
     $         ' ',10X,'TAUK',10X,'R',10X,'P',9X,'NP',
     $             2(9X,'Old',8X,'New',3X,'Weighted'),4X,'TAU-5000')
        call LINER (1,NO)
C
        write (NO,101) (I,TKIN(I),R(I),P(I),XNK(I),XNEO(I),XNENEW(I),
     $                    XNE(I),HNDO(I),HNDNEW(I),HND(I),T5(I),I=1,N)
  101   format(5(' ',I3,1P4E11.2,E12.3,2E11.3,E12.3,2E11.3,E12.3/))
C
        write (NO,102) NEITER,NHITER,MEITER
  102   format(' ',I70,I34/
     $         ' ',I70)
C
      end if
C     !END
      call BYE ('TOM')
C
      return
      end
