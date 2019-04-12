      subroutine BRUSH
     $(RES,N,KODE,RMX,RMN)
C
C     Rudolf Loeser, 1980 Aug 05
C---- Sets up extrema, for MANNA.
C     (This is version 4 of BRUSH.)
C     !DASH
      save
C     !DASH
      real*8 ONE, RES, RMN, RMX
      integer IMAX, IMIN, KODE, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MINMAXD, HI, BYE
C
C               RES(N)
      dimension RES(*)
C
      call HI ('BRUSH')
C     !BEG
      if(KODE.eq.0) then
        RMX = -ONE
        RMN =  ONE
      else
        call MINMAXD (RES,1,N,IMIN,IMAX)
        RMX = RES(IMAX)
        RMN = RES(IMIN)
      end if
C     !END
      call BYE ('BRUSH')
C
      return
      end
