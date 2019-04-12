      subroutine MARGOT
     $(X,N,MATLEG)
C
C     Rudolf Loeser, 1983 Aug 15
C---- Scans the matrix X, and records elements range data.
C     If requested, saves X in file LUSM.
C     !DASH
      save
C     !DASH
      real*8 AX, RAT, RATMIN, SMATC, X, XMN, XMX, ZERO, ZMAX, ZMIN
      integer I, LUSM, MAMAS, N
      character MATLEG*(*)
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
      equivalence (KZQ( 73),MAMAS)
      equivalence (RZQ( 77),SMATC)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- MATMAG      as of 1989 Jan 25
      integer     MSTMAX
      parameter   (MSTMAX=10)
C     (Remember to recompile all users when changing MSTMAX)
      integer     MATKNT,NMAT,MATSNO
      real*8      ELLRGE,ELSMLL,ELRNGE
      character   MATNAM*50
      dimension   ELLRGE(MSTMAX),ELSMLL(MSTMAX),NMAT(MSTMAX),
     $            ELRNGE(MSTMAX),MATNAM(MSTMAX)
      common      /MATMAG1/ MATNAM
      common      /MATMAG2/ MATKNT,NMAT
      common      /MATMAG3/ MATSNO
      common      /MATMAG4/ ELLRGE,ELSMLL,ELRNGE
C     Elements range characteristics of the MSTMAX most extreme
C     matrices, (collected when MAMAS=1).
C     .
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(27),LUSM )
C     !DASH
      external  LARGO, TOSTIG, HI, BYE
      intrinsic abs, min, max
C
C               X(N,N)
      dimension X(*)
C
      data RATMIN /0.D0/
C
      call HI ('MARGOT')
C     !BEG
      if(MAMAS.gt.0) then
C
        AX  = abs(X(1))
        XMX = AX
        XMN = AX
        do 100 I = 2,N**2
          AX = abs(X(I))
          if(AX.ne.ZERO) then
            XMX = max(XMX,AX)
            XMN = min(XMN,AX)
          end if
  100   continue
C
        ZMAX = log10(XMX)
        ZMIN = log10(XMN)
C
        RAT = ZMAX-ZMIN
        if((RAT.gt.RATMIN).or.(MATKNT.lt.MSTMAX)) then
          call LARGO    (N, XMX, XMN, RAT, MATLEG, RATMIN)
        end if
C
        if(SMATC.gt.ZERO) then
          if(RAT.gt.SMATC) then
            call TOSTIG (LUSM, X, N, MATLEG)
          end if
        end if
C
      end if
C     !END
      call BYE ('MARGOT')
C
      return
      end
