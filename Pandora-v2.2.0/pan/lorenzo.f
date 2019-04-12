      subroutine LORENZO
     $(FG,F,N,CRIT,IWRN)
C
C     Rudolf Loeser, 1992 Apr 01
C---- Computes the warning flag, for LAURA.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, FG, ONE, SFG, ZERO
      integer I, IWRN, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  HI, BYE
      intrinsic sign, abs
C
C               F(N)
      dimension F(*)
C
      call HI ('LORENZO')
C     !BEG
      IWRN = 0
      if(FG.ne.ZERO) then
        SFG = sign(ONE,FG)
C
        do 100 I = 1,N
          if(sign(ONE,F(I)).ne.SFG) then
            IWRN = 1
            go to 102
          end if
          if(abs(F(I)/FG).gt.CRIT) then
            IWRN = 1
            go to 102
          end if
          if(F(I).ne.ZERO) then
            if(abs(FG/F(I)).gt.CRIT) then
              IWRN = 1
              go to 102
            end if
          end if
  100   continue
C
      else
C
        do 101 I = 1,N
          if(abs(F(I)).gt.CRIT) then
            IWRN = 1
            go to 102
          end if
  101   continue
C
      end if
  102 continue
C     !END
      call BYE ('LORENZO')
C
      return
      end
