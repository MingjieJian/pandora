      subroutine DFCNOS
     $(CNOS,TE,PART,ENE,XK,ZET,AMA,DUMP)
C
C     Rudolf Loeser, 1990 Jul 05
C---- Computes some collision rates, for DFHECOL (q.v.).
C     !DASH
      save
C     !DASH
      real*8 AE, AMA, AMARED, AT, C1, C2, C3, C4, CNOS, COUFRE, COULOM,
     $       ENE, F01, F9, FACTOR, PART, RAMARED, RATCAR, RATMAS, RT,
     $       TE, XK, ZET, ZETEF
      integer I, J, LUEO
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external HI, BYE
C
      dimension CNOS(5,5), PART(5), ZET(5), AMA(5)
C
      data C1,C2,C3,C4 /3.45D0, 1.15D0, 8.5D-1, 1.33333333333333333D0/
      data F01,F9      /1.D-1, 9.D0/
C
      call HI ('DFCNOS')
C     !BEG
      AT = log10(TE)
      AE = log10(ENE)
      RT = sqrt(TE)
      COULOM = F9+C1*AT-C2*AE
      COUFRE = C3*(F01*COULOM)/(RT**3)
      FACTOR = C4*COUFRE
C
      do 102 I = 1,5
        do 101 J = 1,5
          ZETEF   = ZET(I)*ZET(J)
          AMARED  = (AMA(I)*AMA(J))/(AMA(I)+AMA(J))
          RATMAS  = AMARED/AMA(I)
          RATCAR  = ZETEF**2
          RAMARED = sqrt(AMARED)
C
          CNOS(I,J) = FACTOR*RATCAR*RATMAS/RAMARED
  101   continue
  102 continue
C
      if(DUMP) then
        write (LUEO,103) ENE,COULOM,COUFRE
  103   format(' ','NE=',1PE11.3,' COULN=',E11.3,' COURATE=',E11.3)
      end if
C     !END
      call BYE ('DFCNOS')
C
      return
      end
