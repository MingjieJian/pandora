      subroutine WARTS
     $(INDEX,NEWDAT,KOPAC,CONT,NOPAC,N,DOIT)
C
C     Rudolf Loeser, 2004 Aug 27
C---- Determines whether updated values of the INDEX'th contributor
C     must be computed, and returns DOIT accordingly.
C
C     Also,
C     makes sure its values =0 when its control switch =0.
C
C     (This is version 3 of WARTS.)
C     !DASH
      save
C     !DASH
      real*8 CONT
      integer INDEX, KOPAC, N, NOPAC
      logical DOIT, NEWDAT
C     !DASH
      external ZEROD, HI, BYE
C
C               CONT(Nopac,N), KOPAC(Nopac)
      dimension CONT(NOPAC,*), KOPAC(*)
C
      call HI ('WARTS')
C     !BEG
      DOIT = .false.
      if(KOPAC(INDEX).le.0) then
        call ZEROD (CONT(INDEX,1), NOPAC, N)
      else
        DOIT = NEWDAT
      end if
C     !END
      call BYE ('WARTS')
C
      return
      end
