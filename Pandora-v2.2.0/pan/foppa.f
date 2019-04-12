      subroutine FOPPA
     $(WC,SIGMA,XLIM,XTAB,FTAB,NINT,DMP)
C
C     Rudolf Loeser, 1991 May 07
C---- Computes integral, for FRANCIS.
C     !DASH
      save
C     !DASH
      real*8 ATW, DX, EMX2, F, FTAB, ONE, SIGMA, TWTHRD, U, W, WC, X,
     $       XLIM, XTAB
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
      equivalence (DLIT(17),TWTHRD)
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external HART, LINER, HI, BYE
C
C               XTAB(NINT), FTAB(NINT)
      dimension XTAB(*),    FTAB(*)
C
      data DX /1.D-1/
C
      call HI ('FOPPA')
C     !BEG
      X = XLIM-DX
      do 101 K = 1,NINT
        X    = X+DX
        W    = X*WC
        ATW  = atan(W)
        U    = W**2
        F    = (X**2)*((ONE+TWTHRD*U)/(ONE+U)-ATW/W)
        EMX2 = exp(-X**2)
C
        XTAB(K) = X
        FTAB(K) = EMX2*F
C
        if(DMP) then
          call LINER (1, LUEO)
          write (LUEO,100) K,X,EMX2,W,ATW,F,K,XTAB(K),FTAB(K)
  100     format(' ','k=',I3,3X,'x=',1PE12.4,3X,'emx2=',E12.4,3X,
     $               'w=',E12.4,3X,'atw=',E12.4,3X,'f=',E12.4/
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
      call BYE ('FOPPA')
C
      return
      end
