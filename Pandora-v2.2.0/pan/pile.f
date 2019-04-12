      subroutine PILE
     $(N,NT,NL,Z,CRT,CRL,CRH,KCRH,RHMFF,RHFF,KHFF,CONA,CONX,KCOND,VEC,
     $ FRS,RLINS,KLNS,XRAY,KRAY,COL,KCOL)
C
C     Rudolf Loeser, 1980 Feb 26
C---- Computes Integrated Cooling Rates, for HUMBLE.
C     !DASH
      save
C     !DASH
      real*8 COL, CONA, CONX, CRH, CRL, CRT, FRS, RHFF, RHMFF, RLINS,
     $       VEC, XRAY, Z
      integer J, KCOL, KCOND, KCRH, KHFF, KLNS, KRAY, N, NL, NOION, NT
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
      equivalence (KZQ( 94),NOION)
C     !DASH
      external HEAP, HI, BYE
C
C               CRT(N,NT), CRL(N,NL), VEC(N), CRH(N), RHMFF(N), COL(N),
      dimension CRT(N,*),  CRL(N,*),  VEC(*), CRH(*), RHMFF(*), COL(*),
C
C               RHFF(N), CONA(N), CONX(N), FRS(N), XRAY(N), RLINS(N),
     $          RHFF(*), CONA(*), CONX(*), FRS(*), XRAY(*), RLINS(*),
C
C               Z(N)
     $          Z(*)
C
      call HI ('PILE')
C     !BEG
      if(NOION.le.0) then
        do 100 J = 1,NT
          call HEAP (N,Z,CRT(1,J),VEC,FRS)
  100   continue
        do 101 J = 1,NL
          call HEAP (N,Z,CRL(1,J),VEC,FRS)
  101   continue
      end if
C     !EJECT
      if(KCRH.gt.0) then
        call HEAP (N,Z,CRH  ,VEC,FRS)
      end if
C
      if(KHFF.gt.0) then
        call HEAP (N,Z,RHMFF,VEC,FRS)
        call HEAP (N,Z,RHFF ,VEC,FRS)
      end if
C
      if(KCOND.gt.0) then
        call HEAP (N,Z,CONA ,VEC,FRS)
        call HEAP (N,Z,CONX ,VEC,FRS)
      end if
C
      if(KLNS.gt.0) then
        call HEAP (N,Z,RLINS,VEC,FRS)
      end if
C
      if(KRAY.gt.0) then
        call HEAP (N,Z,XRAY ,VEC,FRS)
      end if
C
      if(KCOL.gt.0) then
        call HEAP (N,Z,COL  ,VEC,FRS)
      end if
C     !END
      call BYE ('PILE')
C
      return
      end
