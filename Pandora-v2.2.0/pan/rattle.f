      subroutine RATTLE
     $(LYM,N,IBEG,IEND,R,SIG,ZL,CL,IMAGE,SYM)
C
C     Rudolf Loeser, 1982 May 14
C---- Plots reserved contributions, for GRIN.
C     (This is version 2 of RATTLE.)
C     !DASH
      save
C     !DASH
      real*8 CL, R, SIG, ZL
      integer IBEG, IEND, N
      logical LYM
      character IMAGE*(*), SYM*1
C     !DASH
      external SHRIMP, LOGO, HI, BYE
C
C               R(N), ZL(N), CL(N)
      dimension R(*), ZL(*), CL(*)
C
      call HI ('RATTLE')
C     !BEG
      if(LYM) then
        call LOGO   (R, N, 1, SIG, CL)
        call SHRIMP (ZL, N, IBEG, IEND, R, 1, SYM, 1, SIG, 2, IMAGE)
      end if
C     !END
      call BYE ('RATTLE')
C
      return
      end
