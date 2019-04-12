      subroutine CONAN
     $(N,FMULT,SIGMAS,CAPPA,COP)
C
C     Rudolf Loeser, 1983 Feb 23
C---- Computes "Background Opacity", for Continuum Jnu.
C     !DASH
      save
C     !DASH
      real*8 CAPPA, COP, FMULT, SIGMAS
      integer N
C     !DASH
      external ARRADD, CONMUL, HI, BYE
C
C               SIGMAS(N), CAPPA(N), COP(N)
      dimension SIGMAS(*), CAPPA(*), COP(*)
C
      call HI ('CONAN')
C     !BEG
      call ARRADD (SIGMAS,CAPPA,COP,N)
      call CONMUL (FMULT ,COP  ,N)
C     !END
      call BYE ('CONAN')
C
      return
      end
