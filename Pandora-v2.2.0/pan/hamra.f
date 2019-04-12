      subroutine HAMRA
     $(XLMDR,LLY,XLMZ,XLMH,XLMXC)
C
C     Rudolf Loeser, 1994 Nov 09
C---- Checks Lyman-alpha-line absorption/scattering parameters and
C     controls.
C     !DASH
      save
C     !DASH
      real*8 ONE, XLMDR, XLMH, XLMXC, XLMZ, ZERO
      integer LLY, LUEO
      logical GOOD, KILROY
C     !COM
C---- APULIA      as of 1994 Nov 02
      real*8      RAYSLM
      common      /APULIA/ RAYSLM
C     Wavelength crossover for Rayleigh scattering computations.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
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
      external LINER, PRIVET, MUSHED, ABORT, HI, BYE
C
C               XLMDR(LLY)
      dimension XLMDR(*)
C
      data GOOD,KILROY /.true., .true./
C
      call HI ('HAMRA')
C     !BEG
      if(LLY.gt.0) then
        if(XLMDR(1).ne.ONE) then
          call MUSHED ('HAMRA', 1, KILROY)
          write (LUEO,100) LLY
  100     format(' ','LMDR table for Lyman alpha wing opacity, of ',
     $               'length LLY =',I5)
          call PRIVET (LUEO, XLMDR, LLY)
          write (LUEO,101)
  101     format(' ','The first value does not = 1 as required.')
          GOOD = .false.
        end if
      else
        if(XLMXC.eq.ZERO) then
          call MUSHED ('HAMRA', 1, KILROY)
          write (LUEO,102)
  102     format(' ','LMXC = 0 is not allowed.')
          GOOD = .false.
        end if
      end if
C     !EJECT
      if(XLMXC.eq.(-ONE)) then
        if(LLY.le.0) then
          call MUSHED ('HAMRA', 1, KILROY)
          write (LUEO,103)
  103     format(' ','When LMXC = -1, tables of values of LMXX and ',
     $               'LMDR must be provided.')
          GOOD = .false.
        end if
      end if
C
      if(XLMZ.lt.RAYSLM) then
        call MUSHED   ('HAMRA', 1, KILROY)
        write (LUEO,104) XLMZ,RAYSLM
  104   format(' ','LMZ =',1PE16.8,', which is less than',E16.8)
        GOOD = .false.
      end if
C
      if(XLMH.gt.XLMZ) then
        call MUSHED   ('HAMRA', 1, KILROY)
        write (LUEO,105) XLMH,XLMZ
  105   format(' ','LMH =',1PE16.8,', which is greater than LMZ =',
     $             E16.8)
        GOOD = .false.
      end if
C
      if(.not.GOOD) then
        call ABORT
      end if
C     !END
      call BYE ('HAMRA')
C
      return
      end
