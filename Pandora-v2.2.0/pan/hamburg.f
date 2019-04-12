      subroutine HAMBURG
     $(ELE,ION,THETA,PE,U,CHI,N)
C
C     Rudolf Loeser, 1982 Jun 16
C---- Computes a table of values of Partition Function, "U",
C     as function of reciprocal temperature "THETA"
C     and of electron pressure "PE",
C     for the "ION"'th stage of ionization of element "ELE".
C     (ION=1 for the neutral atom.)
C
C---- Also returns a table of the ionization potential "CHI".
C     >>>>>   Returns only CHI when PE=1.
C
C---- Returns U=-1.0, CHI=1000.0, if data unavailable.
C     !DASH
      save
C     !DASH
      real*8 CHI, ERROR1, ERROR2, G0, ONE, PE, SIGMA, THETA, U, ZERO
      integer I, ION, KIND, LOOK, N, NOTE, NT
      logical OK
      character ELE*2, TELE*2
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external LOOKSC, SET1, GRUMBLE, HI, BYE
C
C               THETA(N), PE(N), U(N), CHI(N)
      dimension THETA(*), PE(*), U(*), CHI(*)
C
      dimension TELE(37)
C
      data ERROR1,ERROR2 /-1.D0, 1.D3/
C
      data NT /37/
      data TELE /'AL', 'AR', 'B' , 'BA', 'BE', 'C' , 'CA', 'CL',
     $           'CO', 'CR', 'CU', 'F' , 'FE', 'GA', 'H' , 'HE',
     $           'K' , 'KR', 'LI', 'MG', 'MN', 'N' , 'NA', 'NB',
     $           'NE', 'NI', 'O' , 'P' , 'S' , 'SC', 'SI', 'SR',
     $           'TI', 'V' , 'Y' , 'ZN', 'ZR'/
C     !EJECT
C
      call HI ('HAMBURG')
C     !BEG
      call LOOKSC      (TELE,NT,ELE,KIND,NOTE,LOOK)
      if(LOOK.eq.2) then
        KIND = NT
      end if
      OK = ((LOOK.eq.1).and.(NOTE.eq.1)).or.(LOOK.eq.2)
      if(.not.OK) then
        call SET1      (U  ,N,ERROR1)
        call SET1      (CHI,N,ERROR2)
      else
C
        do 100 I = 1,N
          call GRUMBLE (KIND,ION,THETA(I),PE(I),CHI(I),G0,SIGMA)
          if(PE(I).ne.ONE) then
            U(I) = G0+SIGMA
            if(G0.eq.ERROR1) then
              CHI(I) = ERROR2
            end if
          else
            U(I) = ZERO
          end if
  100   continue
C
      end if
C     !END
      call BYE ('HAMBURG')
C
      return
      end
