      subroutine EUSTACE
     $(DLJ,DV,DLS,N,DLSMIN,DLSMAX)
C
C     Rudolf Loeser, 1983 Sep 01
C---- Sets up a shifted-Delta-Lambda vs. Depth table, and keeps track
C     of its extrema.
C     !DASH
      save
C     !DASH
      real*8 DLJ, DLS, DLSMAX, DLSMIN, DV
      integer IMAX, IMIN, IPEX, LUEO, N
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MOVE1, CONADD, MINMAXD, MESHED, VECOUT, MASHED, HI, BYE
C
C               DLS(N), DV(N)
      dimension DLS(*), DV(*)
C
      call HI ('EUSTACE')
C     !BEG
      call MOVE1    (DV, N, DLS)
      call CONADD   (DLJ, DLS, N)
      call MINMAXD  (DLS, 1, N, IMIN, IMAX)
C
      DLSMIN = DLS(IMIN)
      DLSMAX = DLS(IMAX)
C
      if((IPEX.lt.0).or.(IPEX.eq.5)) then
        call MESHED ('EUSTACE', 2)
        write (LUEO,100) DLJ,N
  100   format(' ','DLJ =',1PE12.4,5X,'N =',I4)
C
        call VECOUT (LUEO, DLS, N, 'DLS')
        call MASHED ('EUSTACE')
      end if
C     !END
      call BYE ('EUSTACE')
C
      return
      end
