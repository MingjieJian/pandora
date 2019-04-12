      subroutine HATHOR
     $(N,TE,PGS,XION,DEE)
C
C     Rudolf Loeser, 1989 Sep 26
C---- Supervises the calculation of the matrices DEE,
C     by the "original" method, for the diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 DEE, PGS, TE, XION
      integer I, N
C     !COM
C---- BAMBI       as of 1998 Apr 22
      integer     IPDPAR
      real*8      APDPAR
      dimension   APDPAR(10)
      common      /BAMBI1/ IPDPAR
      common      /BAMBI2/ APDPAR
C     Parameters for "original" d coefficients
C     !DASH
      external MOTUGA, TEAL, HI, BYE
C
C               TE(N), PGS(N), XION(N), DEE(4,5,N)
      dimension TE(*), PGS(*), XION(*), DEE(4,5,*)
C
      call HI ('HATHOR')
C     !BEG
      call MOTUGA (N,IPDPAR)
C
      do 100 I = 1,N
        call TEAL (TE(I),PGS(I),XION(I),DEE(1,1,I))
  100 continue
C     !END
      call BYE ('HATHOR')
C
      return
      end
