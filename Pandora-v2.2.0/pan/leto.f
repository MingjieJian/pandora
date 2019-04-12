      subroutine LETO
     $(LFB,N,NW,WTAB,Z ,TE ,TAUK ,SCON ,FD ,OPAC ,CNXP ,
     $               ZU,TEU,TAUKU,SCONU,FDU,OPACU,CNXPU)
C
C     Rudolf Loeser, 1987 Jan 28
C---- Sets up atmosphere data to be used by BASHKIR, depending
C     on whether front-face (LFB=1) or back-face (LFB=2) is wanted.
C     (This is version 3 of LETO.)
C     !DASH
      save
C     !DASH
      real*8 CNXP, CNXPU, FD, FDU, OPAC, OPACU, SCON, SCONU, TAUK,
     $       TAUKU, TE, TEU, WTAB, Z, ZU
      integer J, LFB, N, NNW, NW
C     !DASH
      external MOVE1, MARTEL, REVERSD, NEGATE, HI, BYE
C
C               TE(N), TAUK(N,Nmkuse), SCON(N,Nmkuse), CNXPU(N,Nmkuse),
      dimension TE(*), TAUK(*),        SCON(*),        CNXPU(N,*),
C
C               OPACU(N,Nmkuse), FD(N,Nmkuse), TEU(N), TAUKU(N,Nmkuse),
     $          OPACU(N,*),      FD(*),        TEU(*), TAUKU(N,*),
C
C               OPAC(N,Nmkuse), CNXP(N,Nmkuse), ZU(N), SCONU(N,Nmkuse),
     $          OPAC(*),        CNXP(*),        ZU(*), SCONU(N,*),
C
C               WTAB(Nmkuse), Z(N), FDU(Nmkuse)
     $          WTAB(*),      Z(*), FDU(N,*)
C
      call HI ('LETO')
C     !BEG
      NNW = N*NW
      call MOVE1       (Z   ,N  ,ZU   )
      call MOVE1       (TE  ,N  ,TEU  )
      call MOVE1       (TAUK,NNW,TAUKU)
      call MOVE1       (SCON,NNW,SCONU)
      call MOVE1       (FD  ,NNW,FDU  )
      call MOVE1       (OPAC,NNW,OPACU)
      call MOVE1       (CNXP,NNW,CNXPU)
C
      if(LFB.eq.2) then
        call REVERSD   (TEU,1,N)
        call REVERSD   (ZU ,1,N)
        call NEGATE    (ZU   ,N)
        do 100 J = 1,NW
          call MARTEL  (TAUKU(1,J),N,WTAB(J))
          call REVERSD (SCONU(1,J),1,N)
          call REVERSD (FDU(1,J)  ,1,N)
          call REVERSD (OPACU(1,J),1,N)
          call REVERSD (CNXPU(1,J),1,N)
  100   continue
      end if
C     !END
      call BYE ('LETO')
C
      return
      end
