      subroutine TANAMA
     $(MN1,KAMB,IVLSW,KDIAG,KBNDS,KDAMP,I4DIO,I4DFM,I4DEQ,KINOUT)
C
C     Rudolf Loeser, 1999 Feb 19
C---- Printout header, for Special-N1 calculation.
C     (This is version 3 of TANAMA.)
C     !DASH
      save
C     !DASH
      integer I4DEQ, I4DFM, I4DIO, IVLSW, KAMB, KBNDS, KDAMP, KDIAG,
     $        KINOUT, LUEO, MN1
      character FLAB*10, ILAB*4
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DASHER, HI, BYE
C
      dimension ILAB(3), FLAB(3)
C
      data ILAB /'H', 'HeI', 'HeII'/
      data FLAB /'stationary', 'inflow', 'outflow'/
C     !EJECT
C
      call HI ('TANAMA')
C     !BEG
      call LINER  (3, LUEO)
      call DASHER (LUEO)
      call LINER  (1, LUEO)
      write (LUEO,100) ILAB(KAMB),KAMB,MN1,FLAB(KINOUT+1)
  100 format(' ','S u m m a r y   of control parameters for ',
     $           'Special-N1 calculation (option AMDN1):'/
     $       ' ','single-ion solution for ',A,' (KAMB =',I2,
     $           '), MN1 =',I4,', ',A/
     $       ' ','(for full explanation of control parameters, ',
     $           'see Section INPUT).')
      call LINER  (1, LUEO)
      if(IVLSW.ne.0) then
        write (LUEO,101) IVLSW
  101   format(' ','IVLSW =',I3,', exponential solution')
      else
        if(KDIAG.eq.3) then
          write (LUEO,102) KDIAG,'three',KBNDS,KDAMP
  102     format(' ','KDIAG =',I2,', ',A,'-diagonal solution, ',
     $               'KBNDS =',I2,', KDAMP =',I2)
        else if(KDIAG.eq.5) then
          write (LUEO,102) KDIAG,'five',KBNDS,KDAMP
        else if(KDIAG.eq.4) then
          if(KINOUT.eq.0) then
            write (LUEO,103) KDIAG,I4DFM,I4DEQ,I4DIO
  103       format(' ','KDIAG =',I2,', four-diagonal solution, ',
     $                 'I4DFM =',I2,', I4DEQ =',I2,:,', I4DIO =',I2)
          else
            write (LUEO,103) KDIAG,I4DFM,I4DEQ
          end if
        end if
      end if
      call DASHER (LUEO)
      call LINER  (2, LUEO)
C     !END
      call BYE ('TANAMA')
C
      return
      end
