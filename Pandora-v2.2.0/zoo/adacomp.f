      subroutine ADACOMP
     $(GS,SUB,VEC,USE)
C     Rudolf Loeser, 1986 Jul 28
C---- Computes and tests the integral, GS,
C     over the current subinterval (XL,XR), for ADAIR.
C     Returns with USE = .true. if the value of GS is acceptable.
C     !DASH
      save
C     !DASH
      real*8 DMAX, DMIN, DX, FL, FLIM, FLM, FM, FOURTH, FR, FRM, GG, GS,
     $       HALF, TWO, VEC, XL, XLM, XM, XR, XRM, ZERO
      integer KD, NF, NOUT
      logical MORE, USE
C     !COM
      common /ADACOMM/ XL,FL,XLM,FLM,XM,FM,XRM,FRM,XR,FR,
     $                 FLIM,DMAX,DMIN,GG,NOUT,KD,NF
C     !DASH
      external  SUB, ADAMORE, ADATEST
C
      dimension VEC(*)
C
      data ZERO, FOURTH, HALF, TWO /0.D0, 2.5D-1, 5.D-1, 2.D0/
C     !EJECT
C
C     !BEG
      DX   = XR-XL
      GS   = FOURTH*DX*(FL+TWO*FM+FR)
      USE  = .true.
      MORE = .false.
      if(DX.gt.DMAX) then
C----   Interval is too large, therefore
C       get supplementary data and reject GS
        call ADAMORE       (SUB,VEC)
        MORE = .true.
        USE  = .false.
      else
        if(DX.ge.DMIN) then
C----     Current interval is not yet too small, therefore
C         perform tests on explicit vs. interpolated function values,
C         and reject GS if necessary
          call ADATEST     (FL,FM,FR,USE)
          if(USE) then
C----       OK so far: get supplementary data for further tests
            call ADAMORE   (SUB,VEC)
            MORE = .true.
C----       Test left half
            call ADATEST   (FL,FLM,FM,USE)
            if(USE) then
C----         Still OK, so also test right half
              call ADATEST (FM,FRM,FR,USE)
            end if
          end if
        end if
      end if
      if((.not.USE).and.(.not.MORE)) then
C----   Make sure that supplementary data are available, since
C       they are needed for the next level of interval refinement
        call ADAMORE       (SUB,VEC)
      end if
C     !END
C
      return
      end
