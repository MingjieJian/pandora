      subroutine ECHEL
     $(KILROY,CALLER,N,IU,IL,NED,DRHO,ORHO,RHO,CAU1)
C
C     Rudolf Loeser, 1984 Jan 19
C---- Prints, for SOPHRON.
C     !DASH
      save
C     !DASH
      real*8 CAU1, DRHO, ORHO, RHO
      integer IL, IU, LUEO, N, NED
      logical KILROY
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, LINER, VECOUT, HI, BYE
C
C               CAU1(N), RHO(N), ORHO(N)
      dimension CAU1(*), RHO(*), ORHO(*)
C     !EJECT
C
      call HI ('ECHEL')
C     !BEG
      call MESHED (CALLER, 2)
C
      if(KILROY) then
        KILROY = .false.
        write (LUEO,100)
  100   format(' ','Rho editing by SOPHRON.'//
     $         ' ','The CHAIN equations for EP1 and EP2 in LYMAN ',
     $             'depend on   A(U,L) * RHO(U,L) + C(U,L)   for ',
     $             'L = 1 and U = 2,3,...'/
     $         ' ','RHO(U,1) more negative then -C(U,1)/A(U,1) can ',
     $             'lead to a negative Lyman continuum source ',
     $             'function, which is inconsistent.'/
     $         ' ','At depth indices larger than NED(U,1), such a ',
     $             'RHO(U,1) is replaced by   ',
     $             '- [ 1 - DRHU(U,1) ] * C(U,1) / A(U,1)   .'//
     $         ' ','If this editing impedes convergence, try a ',
     $             'smaller DRHO, a larger NED, or METEP=0.'/
     $         ' ','(When METEP=0, NOVA equations are used for EP1 ',
     $             'and EP2, which may have better properties.)'//
     $         ' ','NOTE: this editing does not occur when NED < 1 ',
     $             'or NED > (N-1); the default value is NED = N.')
      end if
C
      call LINER  (1, LUEO)
      write (LUEO,101) IU,IL,NED,DRHO
  101 format(' ','RHO editing for transition (',I2,'/',I2,'), NED =',
     $           I3,', DRHO =',F10.5)
      call VECOUT (LUEO, ORHO, N, 'Old Rho')
      call VECOUT (LUEO, CAU1, N, 'C/A'    )
      call VECOUT (LUEO, RHO,  N, 'New Rho')
C
      call MASHED (CALLER)
C     !END
      call BYE ('ECHEL')
C
      return
      end
