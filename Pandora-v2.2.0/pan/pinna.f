      subroutine PINNA
     $(I,INCD,DUMP)
C
C     Rudolf Loeser, 1988 Jan 22
C---- Checks I to determine whether it qualifies for an INCD-dump.
C     (This is version 2 of PINNA.)
C     !DASH
      save
C     !DASH
      integer I, INCD, MINC
      logical DUMP
C     !DASH
      external  HI, BYE
      intrinsic mod
C
      call HI ('PINNA')
C     !BEG
      DUMP = .false.
      if(INCD.ge.0) then
C
        MINC = INCD
        if(MINC.eq.0) then
          MINC = 5
        end if
C
        if((MINC.eq.1).or.(I.eq.1)) then
          DUMP = .true.
        else if(mod(I,MINC).eq.1) then
          DUMP = .true.
        end if
C
      else
C
        if(I.eq.(-INCD)) then
          DUMP = .true.
        end if
C
      end if
C     !END
      call BYE ('PINNA')
C
      return
      end
