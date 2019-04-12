      subroutine ELI
     $(XLM,IPOP,GAUNT,ISL,IEL)
C
C     Rudolf Loeser, 1981 Jun 10
C---- Retrieves bound-free Gaunt Factors, for Population Ions.
C     XLM is wavelength in Angstroms.
C     IPOP is the number of the Population Ion involved.
C     ISL,IEL define the range of levels for which data are wanted.
C     (This is version 2 of ELI.)
C     !DASH
      save
C     !DASH
      real*8 G, GAUNT, ONE, XLM
      integer I, IEL, IPOP, ISL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external GUNNAR, HI, BYE
C
C               GAUNT(Maxdatl)
      dimension GAUNT(*)
C
      call HI ('ELI')
C     !BEG
      do 100 I = ISL,IEL
C
        if(IPOP.eq.1) then
          call GUNNAR (I,XLM,G)
        else
          G = ONE
        end if
C
        GAUNT(I) = G
  100 continue
C     !END
      call BYE ('ELI')
C
      return
      end
