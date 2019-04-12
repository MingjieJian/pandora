      subroutine PADDLE
     $(ITYPE,LINE,CORE)
C
C     Rudolf Loeser, 2002 Nov 20
C---- Determines whether ITYPE marks a line core wavelength, or
C                                    a line wavelength generally.
C
C     Note: when CORE is true then LINE is also true; however
C     LINE can be true when CORE is false.
C     !DASH
      save
C     !DASH
      integer ITYPE
      logical CORE, LINE, REGCOR, SPECOR, SPEWNG
C     !DASH
      external FULEX, HI, BYE
C
      call HI ('PADDLE')
C     !BEG
      call FULEX (ITYPE, REGCOR, SPECOR, SPEWNG)
C
      CORE = REGCOR.or.SPECOR
      LINE = CORE  .or.SPEWNG
C     !END
      call BYE ('PADDLE')
C
      return
      end
