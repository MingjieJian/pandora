      subroutine MUKLUK
     $(N,L1,NL,XNK,VK,GION,SION,XND,VL,GLVL,SLVL,ADD,KODE,Z,W,IW)
C
C     Rudolf Loeser, 1989 Sep 15
C---- Computes diffusion terms.
C     !DASH
      save
C     !DASH
      real*8 ADD, GION, GLVL, SION, SLVL, VK, VL, W, XND, XNK, Z
      integer IW, J, KODE, L1, N, NL
C     !DASH
      external PACIFIC, ARRMUL, ARRADD, HI, BYE
C
      dimension W(*), IW(*)
C
C               XND(N,NL), XNK(N), Z(N), VL(N,NL), GLVL(N,NL), GION(N),
      dimension XND(N,*),  XNK(*), Z(*), VL(N,*),  GLVL(N,*),  GION(*),
C
C               SION(N), SLVL(N,NL), VK(N), ADD(N,NL)
     $          SION(*), SLVL(N,*),  VK(*), ADD(N,*)
C
      call HI ('MUKLUK')
C     !BEG
C---- Compute levels terms
      do 100 J = L1,NL
        call ARRMUL   (XND(1,J),  VL(1,J),  SLVL(1,J), N)
        if(KODE.ne.0) then
          call ARRADD (SLVL(1,J), ADD(1,J), SLVL(1,J), N)
        end if
        call PACIFIC  (Z, SLVL(1,J), GLVL(1,J), N, W, IW)
  100 continue
C
C---- Compute ion term
      call ARRMUL     (XNK, VK, SION, N)
      call PACIFIC    (Z, SION, GION, N, W, IW)
C     !END
      call BYE ('MUKLUK')
C
      return
      end
