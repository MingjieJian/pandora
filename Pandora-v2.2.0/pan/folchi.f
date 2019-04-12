      subroutine FOLCHI
     $(EQNL,EQNU,WC,SIGMA,XLIM,XTAB,FTAB,NINT,DMP)
C
C     Rudolf Loeser, 1991 May 07
C---- Computes integral, for DAMALA.
C     !DASH
      save
C     !DASH
      real*8 ATZ, DX, EMX2, EQNL, EQNRAT, EQNU, FTAB, FVTHRD, G, ONE, S,
     $       SIGMA, W, WC, X, XLIM, XTAB, Z
      integer K, LUEO, NINT
      logical DMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external DIVIDE, HART, LINER, HI, BYE
C
C               XTAB(NINT), FTAB(NINT)
      dimension XTAB(*),    FTAB(*)
C
      data FVTHRD,DX /1.6666666666666667D0, 1.D-1/
C
      call HI ('FOLCHI')
C     !BEG
      X = XLIM-DX
      do 101 K = 1,NINT
        X = X+DX
        W = X*WC
        call DIVIDE  ((EQNL**2), (EQNU**2), EQNRAT)
        call DIVIDE  (W, (ONE-EQNRAT), Z)
C
        ATZ  = atan(Z)
        S    = Z**2
        G    = X*(ATZ-(Z*(ONE+FVTHRD*S))/((ONE+S)**2))
        EMX2 = exp(-X**2)
C
        XTAB(K) = X
        FTAB(K) = EMX2*G
C
        if(DMP) then
          call LINER (1, LUEO)
          write (LUEO,100) K,X,EMX2,W,Z,ATZ,G,K,XTAB(K),FTAB(K)
  100     format(' ','k=',I3,3X,'x=',1PE12.4,3X,'emx2=',E12.4,3X,
     $               'w=',E12.4,3X,'z=',E12.4,3X,'atz=',E12.4,3X,
     $               'g=',E12.4/
     $           ' ','k=',I3,3X,'XTAB=',E16.8,3X,'FTAB=',E16.8)
        end if
  101 continue
C
      call HART      (NINT, XTAB, FTAB, SIGMA)
C
      if(DMP) then
        write (LUEO,102) XLIM,SIGMA
  102   format(' ',8X,'xlim=',1PE16.8,4X,'sum=',E16.8)
      end if
C     !END
      call BYE ('FOLCHI')
C
      return
      end
