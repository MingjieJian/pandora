      subroutine ARMAND
     $(X,WAVES,ISTAR,NWV,WAVMN,WAVMX)
C
C     Rudolf Loeser, 2002 Sep 30
C---- Adds standard rates integrations wavelengths to WAVES.
C     (This is version 2 of ARMAND.)
C     !DASH
      save
C     !DASH
      real*8 TAB, WAVES, WAVMN, WAVMX, X
      integer I, ISTAR, NT, NWV
      logical YES
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- NARITO      as of 2005 Jul 22
      integer     NTMXSW
C     (Remember to recompile users when changing NTMXSW.)
      parameter   (NTMXSW=10000)
C     Upper limit for standard rates integration wavelengths.
C     .
C     !DASH
      external TRAVIS, HALT, WITHIN, HI, BYE
C
      dimension X(*)
C
C               WAVES(NWV)
      dimension WAVES(*)
C
      dimension TAB(NTMXSW)
C
      call HI ('ARMAND')
C     !BEG
      call TRAVIS     (X, NTMXSW, TAB, NT)
      if((ISTAR+NT).gt.(NWV-1)) then
        write (MSSLIN(1),100) ISTAR,NT,NWV
  100   format('ISTAR =',I12,', NT =',I12,' and NWV =',I12,
     $         '; thus NWV is too small.')
        call HALT     ('ARMAND', 1)
      else
C
        do 101 I = 1,NT
          call WITHIN (WAVMN, TAB(I), WAVMX, 0, YES)
          if(YES) then
            ISTAR = ISTAR+1
            WAVES(ISTAR) = TAB(I)
          end if
  101   continue
      end if
C     !END
      call BYE ('ARMAND')
C
      return
      end
