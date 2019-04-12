      subroutine DONWAI
     $(IU,IL,TEMP,DENS,DUMP,CE)
C
C     Rudolf Loeser, 1990 Oct 11
C---- Computes default value of CE for Hydrogen.
C
C     Johnson, L.C.  1972, ApJ, 174, 227-236.
C
C     !DASH
      save
C     !DASH
      real*8 B, BLU, BRACE, BRAK1, BRAK2, CE, CON55, CON57, CQL, DENS,
     $       E1YUL, E1ZUL, E2YUL, E2ZUL, ELL, ELL2, EXYUL, FLU, FOUR,
     $       HALF, ONE, SFLU, TE, TEMP, TERM, THREE, TLIM, TLX, TLXL,
     $       TUL, TUL2, TWO, V, XUL, YOO, YOO2, YOO3, YUL, YUL2, ZERO,
     $       ZUL, dummy
      integer IL, IU, LUEO
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
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
      external  CUEL, RIGEL, ORNIS, NUKIK, EXPINT, JENGU, LINER, HI, BYE
      intrinsic max
C
      data TLIM /2.D3/
C
      call HI ('DONWAI')
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
      TE   = max(TEMP,TLIM)
      ELL  = IL
      ELL2 = IL**2
      YOO  = IU
      YOO2 = IU**2
      YOO3 = YOO2*YOO
      XUL  = ONE-ELL2/YOO2
      TUL = XUL*CQL
      call ORNIS  (IL, XUL, ONE, TUL, TE, YUL, ZUL)
      TUL2 = TUL**2
      YUL2 = YUL**2
      call RIGEL  (57, CON57)
      V = sqrt(CON57*TE)
      call NUKIK  (IU, IL, CQL, SFLU)
      TLX  = (TWO*ELL2)/TUL
      FLU  = TLX*SFLU
      call EXPINT (1, YUL, E1YUL, EXYUL)
      call EXPINT (2, YUL, E2YUL, dummy)
      call EXPINT (1, ZUL, E1ZUL, dummy)
      call EXPINT (2, ZUL, E2ZUL, dummy)
      BRAK1 = (ONE/YUL+HALF)*E1YUL-(ONE/ZUL+HALF)*E1ZUL
      BRAK2 = (E2YUL/YUL-E2ZUL/ZUL)
      TLXL  = log(TLX)
      call JENGU  (IL, B)
      BLU   = ((TLX**2)/YOO3)*(ONE+FOUR/(THREE*TUL)+B/TUL2)
      TERM  = (BLU-FLU*TLXL)
      BRACE = FLU*BRAK1+TERM*BRAK2
      call RIGEL  (55, CON55)
C
      CE = (CON55*ELL2*V*(YUL2/TUL)*BRACE)/EXYUL
C     !EJECT
      if(DUMP) then
        call LINER (2, LUEO)
        write (LUEO,100) IU,IL,TEMP,DENS,CQL,XUL,TUL,YUL,ZUL,V,SFLU,FLU,
     $                   EXYUL,E1YUL,E2YUL,E1ZUL,E2ZUL,BRAK1,BRAK2,TLXL,
     $                   B,BLU,TERM,BRACE,CE
  100   format(' ','CEIJ according to Johnson.',5X,'IU =',I5,3X,
     $             'IL =',I5,5X,'TEMP =',1PE16.8,5X,'DENS =',E16.8/
     $         ' ','QL =',E16.8,:,5X,'XUL =',E16.8,5X,'TUL =',E16.8,5X,
     $             'YUL =',E16.6,5X,'ZUL =',E16.8/
     $         ' ','V =',E16.8,5X,'SFLU =',E16.8,5X,'FLU =',E16.8,5X,
     $             'EXYUL =',E16.8/
     $         ' ','E1YUL =',E16.8,5X,'E2YUL =',E16.8,5X,'E1ZUL =',
     $             E16.8,5X,'E2ZUL =',E16.8/
     $         ' ','BRAK1 =',E16.8,5X,'BRAK2 =',E16.8,5X,'TLXL =',
     $             E16.8,5X,'B =',E16.8/
     $         ' ','BLU =',E16.8,5X,'TERM =',E16.8,5X,'BRACE =',E16.8,
     $             5X,'CEIJ =',E16.8)
      end if
C
  101 continue
C     !END
      call BYE ('DONWAI')
C
      return
      end
