      subroutine SILOA
     $(N,KSE,KSEDA,Z,S,SD,EP,ED,BS,BD,ZL,SL,SDL,EPL,EDL,BSL,BDL,LU)
C
C     Rudolf Loeser, 1998 Aug 11
C---- Supervises Diffusion Analysis source function plots.
C     !DASH
      save
C     !DASH
      real*8 BD, BDL, BS, BSL, ED, EDL, EP, EPL, S, SD, SDL, SL, Z, ZL
      integer KSE, KSEDA, LU, N
C     !DASH
      external MOVE1, ASILO, HI, BYE
C
C               Z(N), S(N), SD(N), ZL(N), SL(N), SDL(N), EP(N), EPL(N),
      dimension Z(*), S(*), SD(*), ZL(*), SL(*), SDL(*), EP(*), EPL(*),
C
C               ED(N), EDL(N), BS(N), BSL(N), BD(N), BDL(N)
     $          ED(*), EDL(*), BS(*), BSL(*), BD(*), BDL(*)
C
      call HI ('SILOA')
C     !BEG
      if((LU.gt.0).and.(KSEDA.eq.2)) then
        if(KSE.eq.1) then
          call MOVE1 (S ,N,SD)
          call MOVE1 (EP,N,ED)
          call MOVE1 (BS,N,BD)
        else
          call ASILO (N,Z,S,SD,EP,ED,BS,BD,ZL,SL,SDL,EPL,EDL,BSL,BDL,LU)
        end if
      end if
C     !END
      call BYE ('SILOA')
C
      return
      end
