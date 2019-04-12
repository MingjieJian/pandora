      subroutine KALMAR
     $(IU,IL,XLMS,TAU,ST,B,N,IBND,XLMC,NC,CALLER)
C
C     Rudolf Loeser, 1996 Feb 29
C---- Dumps, for WAGRIN.
C     !DASH
      save
C     !DASH
      real*8 B, ST, TAU, XLMC, XLMS
      integer IBND, IL, IU, LUEO, N, NC
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, PRIVET, VECOUT, HI, BYE
C
C               TAU(N), ST(N), B(N), XLMC(NC)
      dimension TAU(*), ST(*), B(*), XLMC(*)
C
      call HI ('KALMAR')
C     !BEG
      call MESHED (CALLER, 2)
      write (LUEO,100) IU,IL,XLMS
  100 format(' ','Details of scattering albedo analysis of line ',
     $           '(',I2,'/',I2,'), LMS =',1PE23.15)
C
      call VECOUT (LUEO, TAU, N, 'TAU')
      call VECOUT (LUEO, ST,  N, 'ST' )
      call VECOUT (LUEO, B,   N, 'B'  )
C
      call LINER  (1, LUEO)
      write (LUEO,101) IBND
  101 format(' ','Composite Line Opacity wavelengths, band #',I4)
      call PRIVET (LUEO, XLMC, NC)
C     !END
      call BYE ('KALMAR')
C
      return
      end
