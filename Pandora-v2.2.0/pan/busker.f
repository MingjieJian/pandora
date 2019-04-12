      subroutine BUSKER
     $(X,VXN,N,CVX,WT,WTP,VEC,FR,KILROY)
C
C     Rudolf Loeser, 2005 Jul 18
C---- Sets up flow-broadening velocities, and
C     adjusts NVX if necessary.
C     !DASH
      save
C     !DASH
      real*8 CVX, CVXF, CVXM, FBVMX, FR, HALF, ONE, VEC, VXN, WFB, WT,
     $       WTP, X, ZERO
      integer J, JJFMV, JJFNH, JJHEA, JJHND, JJHNF, JJZ, N, N1, N2, NFB,
     $        NFH, NVI, NVX
      logical DO1, DO2, KILROY
      character qummy*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(42),NVX)
      equivalence (JZQ(31),NFH)
      equivalence (JZQ( 9),NFB)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(225),JJFMV)
      equivalence (IZOQ( 10),JJHNF)
      equivalence (IZOQ( 21),JJFNH)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 65),CVXM )
      equivalence (RZQ( 67),CVXF )
      equivalence (RZQ( 68),WFB  )
      equivalence (RZQ(110),FBVMX)
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external KALMIA, MELATA, CONMUL, MOVE1, ZERO1, SOFFIT, NEGATE,
     $         BOUNDLO, BOUNDUP, HALT, HI, BYE
C
      dimension X(*)
C
C               CVX(NVX), VXN(N,NVX), VEC(NFH), WT(NVX), WTP(NVX),
      dimension CVX(*),   VXN(N,*),   VEC(*),   WT(*),   WTP(*),
C
C               FR(N)
     $          FR(*)
C
      call HI ('BUSKER')
C     !BEG
C---- Initialize
      call ZERO1      (CVX, NVX)
      call ZERO1      (WT,  NVX)
      call ZERO1      (WTP, NVX)
C
      DO1 = (WFB.lt.ONE).and.(NFB.gt.0)
      DO2 = (CVXM.gt.ZERO).or.(NFH.gt.0)
      NVI = 0
      if(DO1.and.DO2) then
        NVI = 2*NFB
C----   Compute CVX, WT, and WTP
        call SOFFIT   (NFB, NVI, WFB, CVXM, CVX, WT, WTP)
C----   Set up the basic isotropic velocity, ...
        call ZERO1    (VXN(1,1), N)
        call KALMIA   (N, CVXM, X(JJZ), X(JJHND), X(JJHEA), FR,
     $                 X(JJFMV), KILROY, qummy, 0, VXN(1,1))
C       ... modify it for minimum flow velocity (? NFH); and ...
        call MELATA   (N, CVXM, VXN(1,1), X(JJHND), NFH, X(JJHNF),
     $                 X(JJFNH), VEC)
C       ... set up the complete isotropic set
        do 100 J = 2,NVX
          call MOVE1  (VXN(1,1), N, VXN(1,J))
          call CONMUL (WT(J), VXN(1,J), N)
  100   continue
        call CONMUL   (WT(1), VXN(1,1), N)
      end if
C     !EJECT
      DO1 = (WFB.gt.ZERO).and.(CVXF.gt.ZERO)
      N2  = NVI
      if(DO1) then
        N1 = NVI+1
        N2 = NVI+2
C----   Set up basic CVXF-velocity, ...
        call KALMIA      (N, CVXF, X(JJZ), X(JJHND), X(JJHEA), FR,
     $                    X(JJFMV), KILROY, qummy, 0, VXN(1,N1))
C----   ... set up opposite velocity, and ...
        call MOVE1       (VXN(1,N1), N, VXN(1,N2))
        call NEGATE      (VXN(1,N2), N)
C----   ... set up effective generating parameters for printing
        CVX(N1) =  CVXF
        CVX(N2) = -CVXF
        WTP(N1) = HALF*WFB
        WTP(N2) = WTP(N1)
      end if
C
C---- Set up final NVX
      NVX = N2
      if(NVX.le.0) then
        write (MSSLIN(1),101) NFB,WFB,CVXF,NFH,NVX
  101   format('FLWBROAD = on: NFB =',I3,', WFB =',F8.2,', CVXF =',
     $         F8.2,', NFH =',I3,' and thus NVX =',I3,
     $         ' does not make sense.')
        call HALT        ('BUSKER', 1)
      end if
C
      if(FBVMX.gt.ZERO) then
C----   Final editing of flow-broadening velocities
        do 102 J = 1,NVX
          if(VXN(1,J).ge.ZERO) then
            call BOUNDUP (N, VXN(1,J),  FBVMX)
          else
            call BOUNDLO (N, VXN(1,J), -FBVMX)
          end if
  102   continue
      end if
C     !END
      call BYE ('BUSKER')
C
      return
      end
