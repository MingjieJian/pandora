      subroutine SEAN
     $(J,KWA,N,Z,ARRCA,NZ,ZT,TAB,TABL)
C
C     Rudolf Loeser, 1993 Sep 16
C---- Interpolates "averaged" line opacity from the Z-grid to the
C     Z-table of the run.
C     !DASH
      save
C     !DASH
      real*8 ARRCA, OPAC, TAB, TABL, Z, ZERO, ZT
      integer I, IRET, J, KWA, N, NZ
      logical OK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external SLOG, MOVE1, LININT, SEXP, HI, BYE
C
C               Z(N), ARRCA(KWA,N), ZT(NZ), TAB(NZ), TABL(NZ)
      dimension Z(*), ARRCA(KWA,*), ZT(*),  TAB(*),  TABL(*)
C
      call HI ('SEAN')
C     !BEG
      do 100 I = 1,NZ
        OK = TAB(I).gt.ZERO
        if(OK) then
          TABL(I) = log(TAB(I))
        else
          call MOVE1 (TAB, NZ, TABL)
          goto 101
        end if
  100 continue
C
  101 continue
      do 102 I = 1,N
        call LININT (ZT, 1, TABL, 1, NZ, Z(I), OPAC, 1, 1, IRET)
        if(OK) then
          ARRCA(J,I) = exp(OPAC)
        else
          ARRCA(J,I) = OPAC
        end if
  102 continue
C     !END
      call BYE ('SEAN')
C
      return
      end
