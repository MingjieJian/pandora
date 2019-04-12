      subroutine BRIT
     $(FNU,N,XJNU,TR)
C
C     Rudolf Loeser, 1984 Apr 11
C---- Computes TR from J-nu.
C     (This is version 4 of BRIT.)
C     !DASH
      save
C     !DASH
      real*8 CON4, CON7, F, FNU, ONE, P, R, ROJ, TR, XJNU, ZERO
      integer I, N
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  RIGEL, DIVIDE, HI, BYE
      intrinsic abs
C
C               XJNU(N), TR(N)
      dimension XJNU(*), TR(*)
C
      call HI ('BRIT')
C     !BEG
      call RIGEL      (7, CON7)
      call RIGEL      (4, CON4)
      R = CON7*(FNU**3)
      F = CON4*(FNU)
C
      do 100 I = 1,N
        if(abs(XJNU(I)).lt.ZZSMALL) then
          TR(I) = ZERO
        else
          call DIVIDE (R, XJNU(I), ROJ)
          P = log(abs(ROJ)+ONE)
          call DIVIDE (F, P, TR(I))
        end if
  100 continue
C     !END
      call BYE ('BRIT')
C
      return
      end
