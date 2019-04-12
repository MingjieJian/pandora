      subroutine CARNEL
     $(DATA,N,DU,DL)
C
C     Rudolf Loeser, 1978 Feb 02
C---- Edits a data table, and finds its extremae.
C     !DASH
      save
C     !DASH
      real*8 DATA, DL, DU, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external  HI, BYE
      intrinsic max, min
C
C               DATA(N)
      dimension DATA(*)
C
      call HI ('CARNEL')
C     !BEG
      DU = -ZZLARGE
      DL = +ZZLARGE
      do 100 I = 1,N
        if(DATA(I).gt.ZERO) then
          DU = max(DATA(I),DU)
          DL = min(DATA(I),DL)
        else if(DATA(I).lt.ZERO) then
          DATA(I) = ZERO
        end if
  100 continue
C     !END
      call BYE ('CARNEL')
C
      return
      end
