      subroutine JUANITA
     $(INDX,XLM,N,NOPAC,BDHM,TE,CO,CONT)
C
C     Rudolf Loeser, 1999 Oct 27
C---- Computes a set of H- bound-free emission values.
C     (This is version 2 of JUANITA.)
C     !DASH
      save
C     !DASH
      real*8 A, BDHM, CO, CON6, CONT, HNUKT, SE, TE, XLM, ZERO
      integer INDX, J, N, NOPAC
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RIGEL, PROD, HI, BYE
C
C               BDHM(N), TE(N), CO(Nopac,N), CONT(Nopac,N)
      dimension BDHM(*), TE(*), CO(NOPAC,*), CONT(NOPAC,*)
C
      call HI ('JUANITA')
C     !BEG
      call RIGEL    (6, CON6)
      A = CON6/(XLM**3)
C
      do 100 J = 1,N
        if(CO(INDX,J).ne.ZERO) then
          call PROD (TE(J), XLM, 2, HNUKT, SE)
          CONT(INDX,J) = ((A*SE)/(BDHM(J)-SE))*CO(INDX,J)
        else
          CONT(INDX,J) = ZERO
        end if
  100 continue
C     !END
      call BYE ('JUANITA')
C
      return
      end
