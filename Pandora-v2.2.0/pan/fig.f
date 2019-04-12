      subroutine FIG
     $(X,IX,W,LU,N,CIJ,GMI,PE,FE,BTA,EP,EF,BS,IU,IL,KRJ,NL,MET,METNAM,
     $ KNT,LAST)
C
C     Rudolf Loeser, 1974 Apr 03
C---- Prints data from Statistical Equlibrium calculations.
C     !DASH
      save
C     !DASH
      real*8 BS, BTA, CIJ, EF, EP, FE, GMI, PE, R, W, X
      integer I, IL, IN, IS, IU, IX, IZ, KNT, KRJ, LU, MET, MOX, N, NL
      logical LAST
      character METNAM*15
C     !DASH
      external IRANTI, LINER, GIG, SHIM, HEP, ARSENAL, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*)
C
C               BS(N), CIJ(N,NL,NL), GMI(N,NSL), METNAM(KNT), BTA(N),
      dimension BS(*), CIJ(*),       GMI(*),     METNAM(*),   BTA(*),
C
C               FE(N), PE(N), EP(N), EF(N)
     $          FE(*), PE(*), EP(*), EF(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IZ    )
C
      dimension   R(11)
C     !EJECT
C
      call HI ('FIG')
C     !BEG
      if(LU.gt.0) then
C       (Get W allotment)
        call IRANTI      (IN, IS, MOX, 'FIG')
C
        call LINER       (3, LU)
        write (LU,100) METNAM(MET+1)
  100   format(' ','Results and intermediates of the ',
     $             'Statistical Equilibrium calculations - ',A15//
     $         ' ',8X,'CUL',8X,'ZUL',9X,'PE',7X,'GZLU',9X,'FE',
     $             8X,'GLU',8X,'ZLU',8X,'BTA',9X,'EP',9X,'EF',
     $             9X,'BS')
        call LINER       (1, LU)
C
        do 102 I = 1,N
          if(MET.eq.4) then
            call ARSENAL (I, IU, IL, N, NL, LAST, X, IX, W, W(IZ))
          else
            call HEP     (I, IU, IL, N, NL, KRJ, LAST, X, IX, W(IZ))
          end if
          call GIG       (R, I, N, NL, IU, IL, CIJ, W(IZ), GMI, PE, FE,
     $                    BTA, EP, EF, BS)
          write (LU,101) I,R
  101     format(' ',I3,1P11E11.3)
          call SHIM      (I, 5, LU)
  102   continue
C
C       (Give back W allotment)
        call WGIVE       (W, 'FIG')
      end if
C     !END
      call BYE ('FIG')
C
      return
      end
