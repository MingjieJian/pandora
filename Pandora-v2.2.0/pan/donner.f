      subroutine DONNER
     $(IU,IL,TEMP,DENS,DUMP,CE)
C
C     Rudolf Loeser, 1990 Dec 07
C---- Computes default value of CE for Hydrogen.
C
C     Vriens, L., and Smeets, A.H.M.  1980, Phys.Rev.A, 22, 940.
C
C     !DASH
      save
C     !DASH
      real*8 AL, ARG, B, BLU, BOLZMN, BRAK, C, C1, C2, CE, CON58, CQL,
     $       DENS, DLU, ELL, ELL2, EMBF, FLU, FOUR, HLU, ONE, RKT, SFLU,
     $       TE, TEMP, THREE, TLIMG, TLX, TUL, TUL2, TWO, UML, XUL, YOO,
     $       YOO2, YOO3, ZERO
      integer IL, IU, LUEO
      logical DUMP
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 2),BOLZMN)
C
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  CUEL, NUKIK, JINGU, NUNNI, LINER, RIGEL, HI, BYE
      intrinsic max
C
      data C,C1,C2 /2.03D-13, 6.D-2, 3.D-1/
      data TLIMG /2.D3/
C
      call HI ('DONNER')
C     !BEG
      call CUEL      (IL, TEMP, DENS, CQL)
      if(CQL.lt.ZERO) then
        CE = ZERO
        if(DUMP) then
          call LINER (2, LUEO)
          write (LUEO,100) IU,IL,TEMP,DENS,CQL
        end if
        goto 101
      end if
C
      TE   = max(TEMP,TLIMG)
      ELL  = IL
      ELL2 = IL**2
      YOO  = IU
      YOO2 = IU**2
      YOO3 = YOO2*YOO
      UML  = YOO-ELL
      XUL  = ONE-ELL2/YOO2
      TUL  = XUL*CQL
      TUL2 = TUL**2
      call NUKIK   (IU, IL, CQL, SFLU)
      TLX = (TWO*ELL2)/TUL
      FLU = TLX*SFLU
      call JINGU   (IL, B)
      BLU = ((TLX**2)/YOO3)*(ONE+FOUR/(THREE*TUL)+B/TUL2)
      call NUNNI   (YOO, ELL, TE, HLU)
      EMBF = exp(-BLU/FLU)
      DLU = EMBF+C1*(UML**2)/(YOO*ELL2)
      call RIGEL   (58, CON58)
      ARG  = (C2*TE)/CON58+DLU
      AL   = log(ARG)
      BRAK = FLU*AL+BLU
      RKT  = sqrt(BOLZMN*TE)
C
      CE = (C*BRAK)/(RKT*(ONE+HLU))
C     !EJECT
      if(DUMP) then
        call LINER (2, LUEO)
        write (LUEO,100) IU,IL,TEMP,DENS,CQL,XUL,TUL,SFLU,FLU,B,BLU,
     $                   HLU,EMBF,DLU,AL,BRAK,RKT,CE
  100   format(' ','CEIJ according to Vriens and Smeets.',5X,
     $             'IU =',I5,3X,'IL =',I5,5X,'TEMP =',1PE16.8,5X,
     $             'DENS =',E16.8/
     $         ' ','QL =',E16.8,:,5X,'XUL =',E16.8,5X,'TUL =',E16.8,5X,
     $             'SFLU =',E16.8,5X,'FLU =',E16.8/
     $         ' ','B =',E16.8,5X,'BLU =',E16.8,5X,'HLU =',E16.8,5X,
     $             'EMBF =',E16.8,5X,'DLU =',E16.8/
     $         ' ','AL =',E16.8,5X,'BRAK =',E16.8,5X,'RKT =',E16.8,5X,
     $             'CEIJ =',E16.8)
      end if
C
  101 continue
C     !END
      call BYE ('DONNER')
C
      return
      end
