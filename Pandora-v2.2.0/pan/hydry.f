      subroutine HYDRY
     $(N,NT,XKPCR,MS,NS,COP)
C
C     Rudolf Loeser, 1976 May 12
C---- Determines whether to fiddle with input values of continuum
C     opacity at line wavelengths, and whether to set various switches.
C     (This is version 2 of HYDRY.)
C     !DASH
      save
C     !DASH
      real*8 COP, XKPCR
      integer MS, N, NS, NT
      logical ZKPC
C     !DASH
      external NAUGHTD, AMETYST, HI, BYE
C
C               XKPCR(NT), COP(N,NT)
      dimension XKPCR(*),  COP(*)
C
      call HI ('HYDRY')
C     !BEG
      if(NT.gt.0) then
        call NAUGHTD   (XKPCR, 1, NT, ZKPC)
        if(.not.ZKPC) then
          call AMETYST (N, NT, MS, NS, XKPCR, COP)
        end if
      end if
C     !END
      call BYE ('HYDRY')
C
      return
      end
