      subroutine TAMERS
     $(N,Z,HND,RHEAB,VBMB,FMV, FR,HEND, VM)
C
C     Rudolf Loeser, 1998 Mar 23
C---- Computes VM and HEND, for diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 FMV, FR, HEND, HND, RHEAB, VBMB, VM, Z
      integer N
C     !DASH
      external BRYMBO, ARRMUL, STREAM, HI, BYE
C
C               Z(N), HND(N), VBMB(N), FR(N), HEND(N), RHEAB(N), VM(N),
      dimension Z(*), HND(*), VBMB(*), FR(*), HEND(*), RHEAB(*), VM(*),
C
C               FMV(N)
     $          FMV(*)
C
      call HI ('TAMERS')
C     !BEG
      call BRYMBO (N,RHEAB,HEND)
      call ARRMUL (HEND,HND,HEND,N)
      call STREAM (N,Z,HND,HEND,VBMB,FMV,FR, VM)
C     !END
      call BYE ('TAMERS')
C
      return
      end
