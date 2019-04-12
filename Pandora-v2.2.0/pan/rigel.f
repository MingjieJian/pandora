      subroutine RIGEL
     $(I,CONST)
C     Rudolf Loeser, 1998 Jan 22
C---- Delivers pre-computed numerical constants.
C     (This is version 2 of RIGEL.)
C     !DASH
      save
C     !DASH
      real*8 A, ANGPCM, AUNIT, B, BOHRAD, BOLZMN, CLIGHT, CMPKM, CONST,
     $       DEBYE, DGPRAD, E, EIGHT, ELCHRG, ELMASS, ERGPEV, EVPK,
     $       FOUR, FREQEV, FRQUNT, G, H, HEMASS, HYLYK, HYMASS, ONE, P,
     $       PI, PLANCK, R, ROOT2, ROOT3, ROOTPI, RYDBRG, SOLDIA,
     $       SOLRAD, SOLSGR, STFBLZ, T, THREE, TWO, U, V, XLOGE, XMBARN,
     $       XNU, Y, Z, ZERO, dummy1, dummy2
      integer I, LU, N
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
C     .
      equivalence
     $(PCON( 1),PLANCK),(PCON( 2),BOLZMN),(PCON( 3),CLIGHT),
     $(PCON( 4),RYDBRG),(PCON( 5),HEMASS),(PCON( 6),ELMASS),
     $(PCON( 7),FREQEV),(PCON( 8),HYMASS),(PCON( 9),SOLSGR),
     $(PCON(10),SOLRAD),(PCON(11),AUNIT ),(PCON(12),ELCHRG),
     $(PCON(13),ERGPEV),(PCON(14),EVPK  ),(PCON(15),STFBLZ),
     $(PCON(16),BOHRAD),(PCON(17),HYLYK ),(PCON(18),SOLDIA)
      equivalence
     $(TUNI( 1),PI    ),(TUNI( 2),ROOTPI),(TUNI( 3),FRQUNT),
     $(TUNI( 4),XMBARN),(TUNI( 5),CMPKM ),(TUNI( 6),ANGPCM),
     $(TUNI( 7),ROOT2 ),(TUNI( 8),ROOT3 ),(TUNI( 9),XLOGE ),
     $(TUNI(10),DGPRAD),(TUNI(11),DEBYE )
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 5),FOUR  )
      equivalence (DLIT( 9),EIGHT )
C     !EJECT
      external HALT, DVECOUT, HYDATA, HI, BYE
C
      parameter (N=72)
      dimension A(N)
C
      data      B, E, G, H /2.D24, 1.D4, 3.2D1, 1.D-7/
      data      P, R, U, V, Y /4.D-17, 2.D-10, 1.D3, 1.D5, 1.D10/
C
      call HI ('RIGEL')
C     !BEG
      if((I.ge.1).and.(I.le.N)) then
C----   Deliver value
        CONST = A(I)
        if(CONST.eq.ZERO) then
C----     Value is invalid
          write (MSSLIN(1),100) CONST,I
  100     format('CONST =',1PE24.16,', thus index I =',I12,
     $           'is not valid.')
          call HALT  ('RIGEL', 1)
        end if
      else if(I.lt.0) then
C----   Print all values
        LU = -I
        call DVECOUT (LU, A, N, 'Precomputed constants')
      else
C     !EJECT
C----   Compute all values
C
C       NOTE: values set = 0 are currently not used.
C
        A( 1) = BOLZMN/(FOUR*PI*(ELCHRG**2))
        A( 2) = PLANCK*CLIGHT
        A( 3) = CLIGHT/PI
        A( 4) = (PLANCK/BOLZMN)*FRQUNT
        A( 5) = ((PLANCK*CLIGHT)/BOLZMN)*ANGPCM
C
        A( 6) = (PLANCK*CLIGHT)*B
        A( 7) = TWO*(PLANCK/(CLIGHT**2))*(FRQUNT**3)
        A( 8) = (PI*BOLZMN)/(TWO*HYMASS)
        A( 9) = ((PLANCK*CLIGHT)/BOLZMN)*E
        A(10) = PLANCK/(TWO*PI*ELMASS)
C
        A(11) = CMPKM/CLIGHT
C       A(12) = .... (below)
        A(13) = PLANCK*FRQUNT
        A(14) = (SOLSGR*HYMASS)*CMPKM
        A(15) = ((ELCHRG**4)*ELMASS*((TWO*PI)**2))/((PLANCK**2)*ERGPEV)
C
        A(16) = CLIGHT*H
        A(17) = (PI/PLANCK)*P
        A(18) = (CMPKM**2)/TWO
        A(19) = FRQUNT/CLIGHT
        A(20) = (PI/PLANCK)*FOUR
C
        A(21) = CLIGHT/FRQUNT
        A(22) = (PLANCK*CLIGHT)/BOLZMN
            T = ((PLANCK**2)/(TWO*PI*ELMASS*BOLZMN))**3
        A(23) = sqrt(T)
        A(24) = (PLANCK*FREQEV)/BOLZMN
        A(25) = (PI*(ELCHRG**2))/(ELMASS*CLIGHT)
C
        A(26) = (BOLZMN/HYMASS)*R
        A(27) = CLIGHT/CMPKM
        A(28) = CLIGHT*ANGPCM
        A(29) = ZERO
        A(30) = FOUR*PI
C
        A(31) = (CLIGHT*ANGPCM)/TWO
        A(32) = (CLIGHT*ANGPCM)/(U*FREQEV)
        A(33) = (FOUR*PI)/PLANCK
        A(34) = ((FRQUNT**2)/CLIGHT)/ANGPCM
        A(35) = PI*(CMPKM**2)
C     !EJECT
        A(36) = SOLSGR*HYMASS
        A(37) = FOUR*PI*ANGPCM
        A(38) = (FOUR*PI*CLIGHT*((SOLRAD/AUNIT)**2))*V
        A(39) = (CLIGHT/(AUNIT**2))*V
        A(40) = FOUR*(CLIGHT*ANGPCM)*(PI*CMPKM)**2
C
        A(41) = (ONE/(EIGHT*PI))*(CLIGHT/Y)**3
            T = EIGHT*((PI*ELCHRG*FRQUNT)**2)
        A(42) = (ELMASS*(CLIGHT**3))/T
        A(43) = ((THREE*(ELCHRG**2)))/(FOUR*PI*ELMASS*CLIGHT)
        A(44) = (PLANCK/BOLZMN)*FREQEV*XLOGE
        A(45) = THREE/(EIGHT*((TWO*PI)**5)*(CLIGHT**2))
C
        A(46) = ONE/(FOUR*((TWO*PI)**3)*(CLIGHT**2))
            T = (ELCHRG**2)/(ELMASS*(CLIGHT**2))
        A(47) = (EIGHT*PI*(T**2))/THREE
        A(48) = (ANGPCM*CMPKM)/FRQUNT
        A(49) = ZERO
        A(50) = (ELMASS*(ELCHRG**4))/((PLANCK/(TWO*PI))**3)
C
        A(51) = ZERO
        A(52) = ZERO
        A(53) = FOUR*PI*FRQUNT
        A(54) = ZERO
        A(55) = TWO*PI*(BOHRAD**2)
C
        A(56) = G/(THREE*ROOT3*PI)
        A(57) = (EIGHT*BOLZMN)/(PI*ELMASS)
        A(58) = (((PLANCK*CLIGHT)/BOLZMN)*ANGPCM)/RYDBRG
        A(59) = ZERO
        A(60) = HYLYK/(RYDBRG**3)
C
        A(61) = CLIGHT/(TWO*PI*FRQUNT)
        A(62) = ZERO
        A(63) = ZERO
        A(64) = (CLIGHT*ANGPCM)/(RYDBRG*FRQUNT)
        A(65) = TWO*(PLANCK*CLIGHT)
C
            T = THREE*(ELCHRG**2)*PLANCK
        A(66) = ((EIGHT*(PI**2)*ELMASS*CLIGHT)/T)*(DEBYE**2)
        A(67) = ZERO
            T = (EIGHT*(PI**2)*(ELCHRG**2))/(ELMASS*CLIGHT)
        A(68) = T
        A(69) = (TWO*(SOLRAD/CMPKM))/(SOLDIA/U)
        A(70) = T/((RYDBRG/ANGPCM)**2)
C     !EJECT
        A(71) = CLIGHT/ANGPCM
            T = EIGHT/(PI*ELMASS*(BOLZMN**3))
            Z = sqrt(T)
        A(72) = (ERGPEV**2)*Z
C
C
                call HYDATA (2,XNU,dummy1,dummy2)
        A(12) = A(16)/XNU
      end if
C     !END
      call BYE ('RIGEL')
C
      return
      end
