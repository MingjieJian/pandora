      subroutine LADDER
     $(WTAB,LTYPE,NW,WLOG,LININT)
C
C     Rudolf Loeser, 1978 Feb 01
C---- Makes a table of log(wavelength;wavenumber) for DONATI.
C     (Sets WLOG=0 for wavelengths to be omitted).
C     (This is version 2 of LADDER.)
C     !DASH
      save
C     !DASH
      real*8 WLOG, WTAB, ZERO
      integer I, LTYPE, NW
      logical LINE, LININT, lummy
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  PADDLE, HI, BYE
      intrinsic max
C
C               WTAB(Nmkuse), WLOG(Nmkuse), LTYPE(Nmkuse)
      dimension WTAB(*),      WLOG(*),      LTYPE(*)
C
      call HI ('LADDER')
C     !BEG
      do 100 I = 1,NW
        if(LININT) then
          LINE = .false.
        else
          call PADDLE (LTYPE(I), LINE, lummy)
        end if
C
        if((WTAB(I).le.ZERO).or.LINE) then
          WLOG(I) = ZERO
        else
          WLOG(I) = log10(WTAB(I))
          WLOG(I) = max(WLOG(I),ZERO)
        end if
  100 continue
C     !END
      call BYE ('LADDER')
C
      return
      end
