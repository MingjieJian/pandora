      subroutine HARRY
     $(NO,N,Z,HNDO,XNEO,XNK,G,H,HNDNEW,XNENEW,HND,XNE,HSEC,NEITER,
     $ MEITER,HEL)
C
C     Rudolf Loeser, 1980 Nov 07
C---- Prints output for ZAPPY.
C     !DASH
      save
C     !DASH
      real*8 G, H, HEL, HND, HNDNEW, HNDO, HSEC, XNE, XNENEW, XNEO, XNK,
     $       Z
      integer I, MEITER, N, NEITER, NO
C     !DASH
      external LINER, HI, BYE
C
C               Z(N), HNDO(N), XNEO(N), XNK(N), G(N), H(N), HNDNEW(N),
      dimension Z(*), HNDO(*), XNEO(*), XNK(*), G(*), H(*), HNDNEW(*),
C
C               XNENEW(N), HND(N), XNE(N)
     $          XNENEW(*), HND(*), XNE(*)
C
      call HI ('HARRY')
C     !BEG
      if(NO.gt.0) then
        call LINER (3,NO)
        write (NO,100) HEL,HSEC
  100   format(' ',72X,'HEL',F6.2,25X,'HSEC',F5.2//
     $         ' ',50X,'E L E C T R O N  number density',
     $              3X,'H Y D R O G E N  number density'/
     $         ' ',50X,31('*'),3X,31('*')/
     $         ' ',13X,'Z',10X,'G',10X,'H',9X,'NP',
     $             2(9X,'Old',8X,'New',3X,'Weighted'))
        call LINER (1,NO)
C
        write (NO,101) (I,Z(I),G(I),H(I),XNK(I),XNEO(I),XNENEW(I),
     $                    XNE(I),HNDO(I),HNDNEW(I),HND(I),I=1,N)
  101   format(5(' ',I3,1P4E11.2,E12.3,2E11.3,E12.3,2E11.3/))
C
        write (NO,102) NEITER,MEITER
  102   format(' ',I70)
      end if
C     !END
      call BYE ('HARRY')
C
      return
      end
