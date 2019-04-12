      subroutine PASTE
     $(LU,N,NS,ELL,EMAX,Z,S,E,EV,FION,FH,FHE,FHE2)
C
C     Rudolf Loeser, 1984 May 08
C---- Prints, for PENDANT.
C     !DASH
      save
C     !DASH
      real*8 C, CMPKM, DEDX, E, ELL, EMAX, EV, FH, FHE, FHE2, FION, S,
     $       TWO, Z, ZERO
      integer I, LU, N, NS
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external LINER, DIVIDE, SHIM, HI, BYE
C
C               Z(N), EV(N), FION(N), FH(N), FHE(N), FHE2(N), E(N), S(N)
      dimension Z(*), EV(*), FION(*), FH(*), FHE(*), FHE2(*), E(*), S(*)
C     !EJECT
C
      call HI ('PASTE')
C     !BEG
      if(LU.gt.0) then
        call LINER      (4,LU)
        write (LU,100) NS,ELL,EMAX
  100   format(' ','Calculated particle energy dissipation.'//
     $         ' ','NS =',I2,5X,'L =',1PE15.7,5X,'EMAX =',E15.7//
     $         ' ',17X,'Z',10X,'DE/DX',7X,'E(Volts)',11X,'FION',13X,
     $             'FH',12X,'FHE',11X,'FHE2')
        call LINER      (1,LU)
C
        C = TWO*CMPKM
        do 102 I = 1,N
C
          if(S(I).ne.ZERO) then
            call DIVIDE (S(I),(C*E(I)),DEDX)
          else
            DEDX = ZERO
          end if
C
          write (LU,101) I,Z(I),DEDX,EV(I),FION(I),FH(I),FHE(I),FHE2(I)
  101     format(' ',I3,1P7E15.7)
          call SHIM   (I,5,LU)
  102   continue
C
      end if
C     !END
      call BYE ('PASTE')
C
      return
      end
