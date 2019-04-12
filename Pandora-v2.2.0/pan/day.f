      subroutine DAY
     $(TAU,IBEG,IEND,IZ,Z)
C
C     Rudolf Loeser, 1982 May 05
C---- Sets up Tau search marker and exponent, for MAY.
C     !DASH
      save
C     !DASH
      real*8 TAU, TEN, TL, Z, ZERO
      integer IBEG, IEND, IS, IZ
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(11),TEN   )
C     !DASH
      external  HI, BYE
      intrinsic max
C
C               TAU(N)
      dimension TAU(*)
C
      call HI ('DAY')
C     !BEG
      IZ = 100
      IS = IBEG
  100 continue
        if(TAU(IS).le.ZERO) then
          IS = IS+1
          if(IS.gt.IEND) then
            goto 101
          end if
          goto 100
        end if
C
      TL = log10(TAU(IS))
      IZ = TL
      if(IZ.gt.0) then
        IZ = IZ+1
      else
        IZ = IZ-1
      end if
C
      IZ = max(IZ,-10)
      Z  = TEN**IZ
C
  101 continue
C     !END
      call BYE ('DAY')
C
      return
      end
