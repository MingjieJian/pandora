      subroutine PACIFIC
     $(X,F,D1,N,W,IW)
C
C     Rudolf Loeser, 1988 Jul 21
C---- Computes first derivative of F(X) for the diffusion calculations.
C     (This is version 2 of PACIFIC.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, D1, F, W, X
      integer IN, INDX, IS, ITMP1, ITMP2, ITMP3, ITMX, IW, IWS, JN,
     $        JTMP1, KDFD1, KRET, LDFD1, MOX, MUX, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(153),KDFD1)
      equivalence (KZQ(204),LDFD1)
C     !DASH
      external SDERIV1, SDERIV2, ADERIV1, NAPOPE, PONAPE, IGIVE, WGIVE,
     $         DERIV1, SLATHER, HALT, HI, BYE
C
      dimension W(*), IW(*)
C
C               X(N), F(N), D1(N)
      dimension X(*), F(*), D1(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),ITMP1 ),(IN( 2),ITMP2 ),(IN( 3),ITMP3 )
C
      dimension JN(1)
      equivalence
     $(JN( 1),JTMP1 )
C
      data ITMX,CRIT,INDX /20, 1.D-3, 0/
C     !EJECT
C
      call HI ('PACIFIC')
C     !BEG
      if((KDFD1.lt.1).or.(KDFD1.gt.4)) then
        write (MSSLIN(1),100) KDFD1
  100   format('KDFD1 =',I12,', which is not 1, 2, 3 or 4.')
        call HALT    ('PACIFIC', 1)
      end if
      if((LDFD1.lt.0).or.(LDFD1.gt.1)) then
        write (MSSLIN(1),101) LDFD1
  101   format('LDFD1 =',I12,', which is not 0 or 1.')
        call HALT    ('PACIFIC', 1)
      end if
C
C     (Get, and allocate, W & IW allotments)
      call PONAPE    (IN, IS , MOX, 'PACIFIC', N)
      call NAPOPE    (JN, IWS, MUX, 'PACIFIC', N)
C
      if(KDFD1.eq.1) then
C----   Use "bridging" intervals (no smoothing)
        call ADERIV1 (X, F, D1, N)
      else if(KDFD1.eq.2) then
C----   Use smoothing and averaging
        call SDERIV1 (X, F, D1, N, W(ITMP1), W(ITMP2), W(ITMP3),
     $                IW(JTMP1))
      else if(KDFD1.eq.3) then
C----   Use smoothing and cubic-spline fit
        call SDERIV2 (X, F, D1, N, W(ITMP1) ,W(ITMP2), W(ITMP3),
     $                IW(JTMP1))
      else
C----   Use just averaging (no smoothing)
        call DERIV1  (X, F, D1, N)
      end if
C
      if(LDFD1.gt.0) then
C----   Apply smoothing to computed derivative
        call SLATHER (X, D1, N, CRIT, ITMX, INDX, W(ITMP1), W(ITMP2),
     $                W(ITMP3), IW(JTMP1), KRET)
        if(KRET.lt.0) then
          write (MSSLIN(1),102) KRET
  102     format('KRET =',I12,': smoothing of computed derivative ',
     $           'failed.')
          call HALT  ('PACIFIC', 1)
        end if
      end if
C
C     (Give back W & IW allotments)
      call WGIVE     (W , 'PACIFIC')
      call IGIVE     (IW, 'PACIFIC')
C     !END
      call BYE ('PACIFIC')
C
      return
      end
