      subroutine TOVE
     $(N,XLTITB,WAVEB,XMULTB,TAUKB,SCONB,OPACB,CNXPB,FDB,XLTIT,
     $ WAVES,XMULT,TAUK,SCON,OPAC,CNXP,FD)
C
C     Rudolf Loeser, 1992 Apr 29
C---- Pulls data out of a Continuum Data Block, for BERMUDA.
C     (This is version 4 of TOVE.)
C     !DASH
      save
C     !DASH
      real*8 CNXP, CNXPB, FD, FDB, OPAC, OPACB, SCON, SCONB, TAUK,
     $       TAUKB, WAVEB, WAVES, XLTIT, XLTITB, XMULT, XMULTB
      integer N
C     !DASH
      external MOVE1, HI, BYE
C
C               TAUKB(N), SCONB(N), OPACB(N), CNXPB(N), FD(N), TAUK(N),
      dimension TAUKB(*), SCONB(*), OPACB(*), CNXPB(*), FD(*), TAUK(*),
C
C               SCON(N),  OPAC(N),  CNXP(N),  FDB(N)
     $          SCON(*),  OPAC(*),  CNXP(*),  FDB(*)
C
      call HI ('TOVE')
C     !BEG
      XLTIT = XLTITB
      WAVES = WAVEB
      XMULT = XMULTB
      call MOVE1 (TAUKB,N,TAUK)
      call MOVE1 (SCONB,N,SCON)
      call MOVE1 (OPACB,N,OPAC)
      call MOVE1 (CNXPB,N,CNXP)
      call MOVE1 (FDB  ,N,FD  )
C     !END
      call BYE ('TOVE')
C
      return
      end
