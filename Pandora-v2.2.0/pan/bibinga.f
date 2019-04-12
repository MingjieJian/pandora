      subroutine BIBINGA
     $(N,NL,XNU,XND,A,RHO,IU,IL,CRT)
C
C     Rudolf Loeser, 1992 Dec 22
C---- Computes cooling rate for transition (IU,IL).
C     (This is version 2 of BIBINGA.)
C     !DASH
      save
C     !DASH
      real*8 A, CRT, FAC, FRQUNT, PLANCK, RHO, XND, XNU
      integer IL, IU, IUL, N, NL
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 1),PLANCK)
      equivalence (TUNI( 3),FRQUNT)
C     !DASH
      external INTRANS, ARRMUL, CONMUL, HI, BYE
C
C               XNU(NSL), XND(N,NL), A(NL,NL), RHO(N), CRT(N,NT)
      dimension XNU(*),   XND(N,*),  A(NL,*),  RHO(*), CRT(N,*)
C
      call HI ('BIBINGA')
C     !BEG
      call INTRANS (IU, IL, 'BIBINGA', IUL)
      call ARRMUL  (XND(1,IU), RHO, CRT(1,IUL), N)
C
      FAC = (XNU(IU)-XNU(IL))*A(IU,IL)*PLANCK*FRQUNT
      call CONMUL  (FAC, CRT(1,IUL), N)
C     !END
      call BYE ('BIBINGA')
C
      return
      end
