      subroutine OCTOPUS
     $(XLM,X,INCDNT,TAU,EXT,CNDT,CNXP)
C
C     Rudolf Loeser, 1980 Mar 13
C---- Computes Incident Radiation term, for CSF calculation.
C     !DASH
      save
C     !DASH
      real*8 CNDT, CNXP, EXT, TAU, X, XLM
      integer N
      logical INCDNT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external ZERO1, SPUMONI, RAGE, ARRMUL, HI, BYE
C
      dimension X(*)
C
C               TAU(N), CNDT(N), CNXP(N), EXT(N)
      dimension TAU(*), CNDT(*), CNXP(*), EXT(*)
C
      call HI ('OCTOPUS')
C     !BEG
      if(INCDNT) then
        call SPUMONI (XLM, 2, X, CNDT)
        call RAGE    (TAU, N, EXT)
        call ARRMUL  (CNDT, EXT, CNXP, N)
      else
        call ZERO1   (CNDT, N)
        call ZERO1   (CNXP, N)
      end if
C     !END
      call BYE ('OCTOPUS')
C
      return
      end
