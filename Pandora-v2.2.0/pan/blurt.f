      subroutine BLURT
     $(I,LINES,NO,WAVE,DAMP,FACT,ZT1,ST1,YT1,ITS,TMAX,XLTIT,NSH,SLTIT,
     $ BULT,ITYPE)
C
C     Rudolf Loeser, 1995 Apr 25
C---- Prints Part 1, for SHARI.
C     (This is version 3 of BLURT.)
C     !DASH
      save
C     !DASH
      real*8 BULT, DAMP, FACT, SLTIT, ST1, TMAX, WAVE, XLTIT, YT1, ZT1
      integer I, ITS, ITYPE, J, LINES, LMHD, MINES, NH, NO, NSH
      character FUJ*7, HEADER*40, TIT*10
C     !DASH
      external  ABJECT, BASINA, HAKO, CHORE, LINER, HI, BYE
      intrinsic mod
C
      parameter (LMHD=50)
      dimension HEADER(LMHD)
C
C               SLTIT(NSH)
      dimension SLTIT(*)
C
      call HI ('BLURT')
C     !BEG
      call BASINA   (XLTIT, NSH, SLTIT, LMHD, NH, HEADER)
C
      if((LINES+NH).gt.55) then
        call ABJECT (NO)
        write (NO,100)
  100   format(' ',12X,'Wavelength',6X,'Damping',3X,'Opac.',2X,
     $             'I====== at TAU = 1.0 ======I'/
     $         ' ',11X,'(Angstroms)',5X,'parameter',2X,'Mult.',6X,'Z',
     $             8X,'CSF',7X,'JNU',3X,'IT',2X,'TAU-max')
        LINES = 2
        MINES = 0
      end if
C     !EJECT
      if(I.gt.0) then
        if(mod(MINES,5).eq.0) then
          call LINER (1, NO)
          LINES = LINES+1
        end if
      end if
C
      call HAKO      (DAMP, TIT)
      call CHORE     (ITYPE, FACT, BULT, FUJ)
C
      if(I.gt.0) then
        write (NO,101) I,WAVE,TIT,FUJ,ZT1,ST1,YT1,ITS,TMAX,HEADER(1)
  101   format(' ',I7,1PE19.12,A10,A7,3E10.2,I3,E9.2,1X,A40)
        LINES = LINES+1
        MINES = MINES+1
C
        if(NH.gt.1) then
          do 103 J = 2,NH
            write (NO,102) HEADER(J)
  102       format(' ',86X,A40)
            LINES = LINES+1
  103     continue
        end if
C
      else
        write (NO,104) ZT1,ST1,TMAX
  104   format(' ',15X,'(line-free)',17X,1P2E10.2,13X,E9.2)
        LINES = LINES+1
      end if
C     !END
      call BYE ('BLURT')
C
      return
      end
