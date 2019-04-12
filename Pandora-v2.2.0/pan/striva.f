      subroutine STRIVA
     $(WAVE,BAND,WLO,WHI,NTMX,TAB,N,CALLER)
C
C     Rudolf Loeser, 2002 Sep 26
C---- Adds values to a wavelengths table.
C     !DASH
      save
C     !DASH
      real*8 BAND, DW, TAB, WAVE, WHI, WLO, ZERO
      integer N, NTMX
      logical GOOD, YES
      character CALLER*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external WITHIN, HUGINN, HI, BYE
C
      dimension TAB(*)
C
      call HI ('STRIVA')
C     !BEG
      call WITHIN   (WLO, WAVE, WHI, 0, YES)
C
      GOOD = .true.
      if(YES) then
        if(BAND.gt.ZERO) then
          GOOD = (N+2).le.NTMX
          if(GOOD) then
            DW = WAVE*BAND
            TAB(N+1) = WAVE-DW
            TAB(N+2) = WAVE+DW
            N = N+2
          end if
        else
          GOOD = (N+1).le.NTMX
          if(GOOD) then
            TAB(N+1) = WAVE
            N = N+1
          end if
        end if
      end if
C
      if(.not.GOOD) then
C       Table augmentation error stop
        call HUGINN (N, NTMX, (CALLER//'-STRIVA'))
      end if
C     !END
      call BYE ('STRIVA')
C
      return
      end
