      subroutine ZED
     $(Z,N, TAU,M, KOPT,IBEG,IEND, XVAL,TIT, CALLER)
C
C     Rudolf Loeser, 1976 May 07
C---- Sets up a graph abscissa.
C     (This is version 4 of ZED.)
C     !DASH
      save
C     !DASH
      real*8 TAU, XVAL, Z
      integer IBEG, IEND, IPEX, IZOPT, JZOPT, KOD, KOPT, KZOPT, LUEO, M,
     $        N
      character CALLER*8, TIT*(*)
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
      equivalence (KZQ( 36),IZOPT)
      equivalence (KZQ( 37),JZOPT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external NADAL, DUMP, PUMP, HUMP, CLUMP, MESHED, MASHED, HALT,
     $         HI, BYE
C
C               Z(N), TAU(N), XVAL(N)
      dimension Z(*), TAU(*), XVAL(*)
C
      call HI ('ZED')
C     !BEG
C---- Coordinate type
      KZOPT = KOPT
      if(KZOPT.le.0) then
        KZOPT = IZOPT
      end if
      if((KZOPT.lt.1).or.(KZOPT.gt.4)) then
        write (MSSLIN(1),100) KZOPT
  100   format('KZOPT =',I12,', which is not 1, 2, 3, or 4.')
        call HALT   ('ZED', 1)
      end if
C
C---- Limiting indices
      call NADAL    (M, TAU, KZOPT, IBEG, IEND)
C
      KOD = 1
      TIT = 'empty'
      if(KZOPT.eq.4) then
        if(M.eq.N) then
C----     Log(TAU)
          call DUMP (TAU, XVAL, IBEG, IEND, TIT, KOD)
        else
          KOD = 0
        end if
      else if(KZOPT.eq.3) then
C----   Log(Z)
        call CLUMP  (Z, XVAL, IBEG, IEND, TIT, KOD)
      end if
      if((KZOPT.eq.1).or.(KOD.eq.0)) then
C----   Depth index
        call PUMP   (XVAL, IBEG, IEND, TIT)
      else if(KZOPT.eq.2) then
C----   Z
        call HUMP   (Z, XVAL, IBEG, IEND, TIT)
      end if
C
      if((IPEX.lt.0).or.(IPEX.eq.19)) then
        call MESHED ('ZED', 2)
        write (LUEO,101) N,KOPT,IBEG,IEND,TIT,KOD,KZOPT,JZOPT,M,CALLER
  101   format(' ',4I10,2X,A10,4I10,2X,A8)
        call MASHED ('ZED')
      end if
C     !END
      call BYE ('ZED')
C
      return
      end
