      subroutine HAZEL
     $(W,N,I,JL,JR,DT,SE2,SE3,SE4,SES2,SES3,SES4,REF,SMLLTAU)
C
C     Rudolf Loeser, 1981 Mar 23 (revised 2000 Jan 25)
C---- Computes diagonal terms, and starting indices for lateral scans,
C     for RT weight matrix calculation.
C     (Note: the bottom-right diagonal term requires special
C     treatment for the REFLECTIVE case; 2002 May 21.)
C     !DASH
      save
C     !DASH
      real*8 DT, ONE, SE2, SE3, SE4, SES2, SES3, SES4, TERM, TERML,
     $       TERMR, W
      integer I, J, JL, JR, N, jummy
      logical REF, SMLLTAU
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  RL, RR, LR, LL, CP, RP, LP, HI, BYE
C
C               DT(2*N), SE2(2*N), SE3(2*N), SES2(2*N), SES3(2*N),
      dimension DT(*),   SE2(*),   SE3(*),   SES2(*),   SES3(*),
C
C               W(N,N), SE4(2*N), SES4(2*N)
     $          W(N,*), SE4(*),   SES4(*)
C     !EJECT
C
      call HI ('HAZEL')
C     !BEG
      J = I
C
      if(J.eq.1) then
C----   Left-most column: do diagonal term,
        call RL (DT,J,TERM,    SE2,SE3,SES2,SES3,jummy)
        W(I,J) = TERM-ONE
C       and step to the right;
        JR = J+1
        call RR (DT,JR,W(I,JR),SE2,SE3,SES2,SES3,jummy)
C       (no scan to the left possible).
        JL = 0
C
      else if((J.eq.N).and.(.not.REF)) then
C----   Right-most column: do diagonal term,
        call LR (DT,J,TERM,    SE2,SE3,SES2,SES3,jummy)
        W(I,J) = TERM-ONE
C       and step to the left;
        JL = J-1
        call LL (DT,JL,W(I,JL),SE2,SE3,SES2,SES3,jummy)
C       (no scan to the right possible).
        JR = N+1
C
      else
C----   Interior columns: do diagonal term,
        call CP (DT,J,W(I,J),     SE3,SE4,     SES3,SES4,SMLLTAU)
C
C       look to the right,
        JR = J+1
        call RP (DT,J,TERMR,  SE2,SE3,SE4,SES2,SES3,SES4,SMLLTAU)
C       and look to the left.
        JL = J-1
        call LP (DT,J,TERML,  SE2,SE3,SE4,SES2,SES3,SES4,SMLLTAU)
C
        if((J.eq.N).and.REF) then
C         (no scan to the right possible)
          W(I,JL) = TERML+TERMR
        else
C         step to the right
          W(I,JR) = TERMR
C         step to the left
          W(I,JL) = TERML
        end if
      end if
C     !END
      call BYE ('HAZEL')
C
      return
      end
